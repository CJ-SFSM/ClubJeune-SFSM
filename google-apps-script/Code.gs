const ABSTRACT_SHEET = 'Abstracts2027';
const HEADERS = [
  'received_at', 'presentation_type', 'title', 'authors',
  'affiliation_1', 'affiliation_2', 'affiliation_3', 'affiliation_4', 'affiliation_5',
  'themes', 'theme_autre', 'abstract',
  'ref_1', 'ref_2', 'ref_3', 'ref_4', 'ref_5', 'ref_6', 'email', 'consent',
  'language', 'submission_id', 'payload_hash',
];
const LEGACY_HEADERS = ['received_at', 'submission_id', 'payload_hash', 'language']
  .concat(HEADERS.slice(1, -3));
const PREVIOUS_HEADERS = HEADERS.slice(0, -3).concat(['submission_id', 'payload_hash', 'language']);
const TOPICS = [
  'Chimie Inorganique', 'Chimie Organique', 'Chimie-Physique',
  'Développement méthodologique', 'Élucidation Structurale', 'Imagerie',
  'Instrumentation', 'Lipidomique', 'Métabolomique', 'Mobilité Ionique',
  'Pétroléomique', 'Polymères', 'Protéomique', 'Quantification', 'Autre',
];
const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;

function configuration_() {
  const properties = PropertiesService.getScriptProperties();
  const spreadsheetId = properties.getProperty('SPREADSHEET_ID');
  const origin = properties.getProperty('SITE_ORIGIN') || 'https://cj-sfsm.github.io';
  if (!spreadsheetId || !/^[A-Za-z0-9_-]+$/.test(spreadsheetId) ||
      !/^https:\/\/[a-z0-9.-]+(?::[0-9]+)?$/i.test(origin)) {
    throw new Error('Invalid receiver configuration');
  }
  return {
    spreadsheetId: spreadsheetId,
    origin: origin,
    turnstileSecret: properties.getProperty('TURNSTILE_SECRET_KEY') || '',
  };
}

/** Create/check the tab and migrate the original column order, preserving rows. */
function setup() {
  const config = configuration_();
  const lock = LockService.getScriptLock();
  lock.waitLock(10000);
  try {
    const spreadsheet = SpreadsheetApp.openById(config.spreadsheetId);
    const sheet = spreadsheet.getSheetByName(ABSTRACT_SHEET) || spreadsheet.insertSheet(ABSTRACT_SHEET);
    if (sheet.getLastRow() === 0) {
      sheet.getRange(1, 1, 1, HEADERS.length).setValues([HEADERS]);
      sheet.setFrozenRows(1);
    } else {
      let headers = checkHeaders_(sheet, true);
      if (JSON.stringify(headers) === JSON.stringify(LEGACY_HEADERS)) {
        // moveColumns uses the destination index BEFORE the source is removed.
        sheet.moveColumns(sheet.getRange(1, 2, 1, 3), HEADERS.length + 1);
        headers = PREVIOUS_HEADERS;
      }
      if (JSON.stringify(headers) === JSON.stringify(PREVIOUS_HEADERS)) {
        // Move language before submission_id and payload_hash, preserving cells.
        sheet.moveColumns(sheet.getRange(1, HEADERS.length, 1, 1), HEADERS.length - 2);
      }
    }
    checkHeaders_(sheet);
    SpreadsheetApp.flush();
  } finally {
    lock.releaseLock();
  }
}

function doGet() {
  return HtmlService.createHtmlOutput('RCJSM abstract receiver. Submit through the conference website.');
}

function doPost(event) {
  let config;
  let submissionId = '';
  let nonce = '';
  let saved = false;
  try {
    config = configuration_();
    // Bound processing before inspecting or writing attacker-controlled values.
    if (!event || !event.parameters || event.contentLength > 100000) {
      return receipt_(config.origin, submissionId, nonce, 'invalid');
    }
    submissionId = single_(event.parameters, 'submission_id', 36, true);
    nonce = single_(event.parameters, 'nonce', 36, true);
    if (!UUID.test(submissionId) || !UUID.test(nonce)) throw new Error('invalid');
    const payload = validate_(event.parameters, config.origin);
    const digest = Utilities.computeDigest(Utilities.DigestAlgorithm.SHA_256, JSON.stringify(payload))
      .map(function (byte) { return ('0' + ((byte + 256) % 256).toString(16)).slice(-2); }).join('');
    const lock = LockService.getScriptLock();
    if (!lock.tryLock(1000)) return receipt_(config.origin, submissionId, nonce, 'busy');
    try {
      const sheet = SpreadsheetApp.openById(config.spreadsheetId).getSheetByName(ABSTRACT_SHEET);
      // Accept known earlier layouts while a deployed receiver awaits setup().
      const headers = checkHeaders_(sheet, true);
      const lastRow = sheet.getLastRow();
      // Dedupe + write use one lock. The hash prevents reuse of an ID for
      // different content; retrying an acknowledged/lost response adds no row.
      const previous = lastRow > 1
        ? sheet.getRange(2, headers.indexOf('submission_id') + 1, lastRow - 1, 2).getValues()
          .find(function (row) { return row[0] === submissionId; })
        : null;
      if (previous) {
        if (previous[1] !== digest) return receipt_(config.origin, submissionId, nonce, 'conflict');
        saved = true;
        return receipt_(config.origin, submissionId, nonce, 'saved', confirmationEmailStatus_(submissionId));
      }
      if (config.turnstileSecret && !verifyCaptcha_(event.parameters, config, submissionId)) {
        return receipt_(config.origin, submissionId, nonce, 'captcha');
      }
      const values = [new Date().toISOString()].concat(payload.slice(1), [payload[0], submissionId, digest]);
      // Escape spreadsheet/CSV formula prefixes, including after whitespace.
      const cells = headers.map(function (header) {
        const value = values[HEADERS.indexOf(header)];
        return /^[\s\uFEFF]*[=+\-@]/u.test(value) ? "'" + value : value;
      });
      sheet.getRange(lastRow + 1, 1, 1, HEADERS.length).setNumberFormat('@').setValues([cells]);
      SpreadsheetApp.flush();
      saved = true;
    } finally {
      lock.releaseLock();
    }
    // Only the request that inserts the row attempts an email. Release the
    // sheet lock first; mail failures must never turn a saved abstract into an error.
    const emailStatus = sendConfirmationEmail_(payload, submissionId);
    return receipt_(config.origin, submissionId, nonce, 'saved', emailStatus);
  } catch (error) {
    // Do not log form contents, addresses, secrets or Google exception details.
    if (saved) return receipt_(config.origin, submissionId, nonce, 'saved', 'unknown');
    const status = error.message === 'invalid' ? 'invalid' : 'unavailable';
    return receipt_(config ? config.origin : 'https://cj-sfsm.github.io', submissionId, nonce, status);
  }
}

function single_(parameters, name, maxLength, required) {
  const values = parameters[name] || [];
  if (values.length > 1 || (values.length === 1 && typeof values[0] !== 'string')) throw new Error('invalid');
  const value = (values[0] || '').trim();
  if ((required && !value) || value.length > maxLength || /[\u0000-\u0008\u000B\u000C\u000E-\u001F]/u.test(value)) {
    throw new Error('invalid');
  }
  return value;
}

function validate_(parameters, origin) {
  if (single_(parameters, 'site_origin', 200, true) !== origin ||
      single_(parameters, '_gotcha', 200, false)) throw new Error('invalid');
  const elapsed = Number(single_(parameters, 'elapsed_ms', 16, true));
  if (!Number.isFinite(elapsed) || elapsed < 3000) throw new Error('invalid');
  const language = single_(parameters, 'language', 2, true);
  const presentation = single_(parameters, 'presentation_type', 40, true);
  if (!['fr', 'en'].includes(language) ||
      !['Orale_12+3', 'Courte_3_sans_questions'].includes(presentation)) throw new Error('invalid');
  const title = single_(parameters, 'title', 220, true);
  const authors = single_(parameters, 'authors', 400, true);
  const affiliations = [1, 2, 3, 4, 5].map(function (index) {
    return single_(parameters, 'affiliation_' + index, 220, index === 1);
  });
  const themes = parameters.themes || [];
  if (!themes.length || themes.length > TOPICS.length || new Set(themes).size !== themes.length ||
      themes.some(function (topic) { return !TOPICS.includes(topic); })) throw new Error('invalid');
  const other = single_(parameters, 'theme_autre', 100, themes.includes('Autre'));
  const abstract = single_(parameters, 'abstract', 2500, true);
  if (abstract.split(/\s+/u).length > 300) throw new Error('invalid');
  const references = [1, 2, 3, 4, 5, 6].map(function (index) {
    return single_(parameters, 'ref_' + index, 260, false);
  });
  const email = single_(parameters, 'email', 254, true).toLowerCase();
  if (!/^[^\s@,;<>]+@[^\s@,;<>]+\.[^\s@,;<>]+$/u.test(email) ||
      single_(parameters, 'consent', 3, true) !== 'yes') throw new Error('invalid');
  return [language, presentation, title, authors].concat(affiliations,
    [themes.join('; '), other, abstract], references, [email, 'yes']);
}

function checkHeaders_(sheet, allowLegacy) {
  const headers = sheet && sheet.getLastRow() >= 1
    ? sheet.getRange(1, 1, 1, HEADERS.length).getValues()[0] : [];
  if (JSON.stringify(headers) !== JSON.stringify(HEADERS) &&
      !(allowLegacy && [LEGACY_HEADERS, PREVIOUS_HEADERS].some(function (known) {
        return JSON.stringify(headers) === JSON.stringify(known);
      }))) {
    throw new Error('Run setup() and preserve column headers');
  }
  return headers;
}

function verifyCaptcha_(parameters, config, submissionId) {
  const token = single_(parameters, 'cf-turnstile-response', 2048, false);
  if (!token) return false;
  const response = UrlFetchApp.fetch('https://challenges.cloudflare.com/turnstile/v0/siteverify', {
    method: 'post',
    payload: { secret: config.turnstileSecret, response: token, idempotency_key: submissionId },
    muteHttpExceptions: true,
  });
  if (response.getResponseCode() !== 200) throw new Error('Captcha service unavailable');
  const result = JSON.parse(response.getContentText());
  const hostname = config.origin.replace(/^https:\/\//, '').split(':')[0];
  return result.success === true && result.hostname === hostname && result.action === 'abstract';
}

function confirmationEmailStatus_(submissionId) {
  try {
    const status = PropertiesService.getScriptProperties().getProperty('confirmation_email:' + submissionId);
    return status === 'sent' || status === 'failed' ? status : 'unknown';
  } catch (error) {
    return 'unknown';
  }
}

function sendConfirmationEmail_(payload, submissionId) {
  let status = 'failed';
  try {
    if (MailApp.getRemainingDailyQuota() > 0) {
      const english = payload[0] === 'en';
      const title = payload[2];
      const email = payload[payload.length - 2];
      const body = english
        ? 'This is an automated confirmation email.\n\nHello,\n\nYour abstract submission for RCJSM has been recorded.\n\n' +
          'Title: ' + title + '\nReference: ' + submissionId + '\n\n' +
          'This email acknowledges receipt of your submission; it does not confirm selection for a presentation.\n' +
          'For corrections, contact clubjeunesm@gmail.com and include your reference.\n\nClub Jeune SFSM'
        : 'Ceci est un email automatique de confirmation.\n\nBonjour,\n\nVotre soumission de résumé pour les RCJSM a bien été enregistrée.\n\n' +
          'Titre : ' + title + '\nRéférence : ' + submissionId + '\n\n' +
          'Cet email confirme la réception de votre soumission, pas sa sélection pour une présentation.\n' +
          'Pour toute correction, contactez clubjeunesm@gmail.com en indiquant votre référence.\n\nLe Club Jeune SFSM';
      MailApp.sendEmail({
        to: email,
        subject: english ? 'RCJSM — Abstract submission received' : 'RCJSM — Confirmation de dépôt de votre résumé',
        body: body,
        name: 'Club Jeune SFSM',
        replyTo: 'clubjeunesm@gmail.com',
      });
      status = 'sent';
    }
  } catch (error) {
    // Quota, permission and provider errors affect the email only.
  }
  try {
    // Preserve the result for receipt retries, without adding table columns or
    // storing the recipient/content in script properties. Never resend on retry.
    PropertiesService.getScriptProperties().setProperty('confirmation_email:' + submissionId, status);
  } catch (error) {
    // Even failure to remember the mail status cannot invalidate the saved row.
  }
  return status;
}

function receipt_(origin, submissionId, nonce, status, emailStatus) {
  const json = JSON.stringify({
    type: 'rcjsm-abstract', submissionId: submissionId, nonce: nonce, status: status, emailStatus: emailStatus,
  }).replace(/</g, '\\u003c').replace(/>/g, '\\u003e').replace(/&/g, '\\u0026')
    .replace(/\u2028/g, '\\u2028').replace(/\u2029/g, '\\u2029');
  const target = JSON.stringify(origin).replace(/</g, '\\u003c');
  return HtmlService.createHtmlOutput('<!doctype html><meta charset="utf-8">' +
    '<script>window.top.postMessage(' + json + ',' + target + ');</script>')
    .setXFrameOptionsMode(HtmlService.XFrameOptionsMode.ALLOWALL);
}

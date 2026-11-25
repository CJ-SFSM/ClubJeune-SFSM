const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const { createHash, randomUUID } = require('node:crypto');

function receiver(properties = {}) {
  const state = {
    rows: [], busy: false, writeError: false, flushError: false,
    captcha: { success: true, hostname: 'cj-sfsm.github.io', action: 'abstract' },
    captchaCalls: 0, locked: false, columnMoves: 0,
    mailQuota: 100, mailQuotaError: false, mailError: false, mailAttempts: 0,
    sentEmails: [], mailStatusReadError: false, mailStatusWriteError: false,
  };
  const sheet = {
    getLastRow: () => state.rows.length,
    setFrozenRows: () => {},
    moveColumns(range, destinationIndex) {
      const start = range.getColumn() - 1;
      const width = range.getNumColumns();
      for (const row of state.rows) {
        const moved = row.splice(start, width);
        row.splice(destinationIndex - 1 - (start < destinationIndex - 1 ? width : 0), 0, ...moved);
      }
      state.columnMoves++;
    },
    getRange(row, col, count, width) {
      return {
        getColumn: () => col,
        getNumColumns: () => width,
        getValues: () => state.rows.slice(row - 1, row - 1 + count).map((values) => values.slice(col - 1, col - 1 + width)),
        setNumberFormat() { return this; },
        setValues(values) {
          if (state.writeError) throw new Error('simulated Google write failure');
          for (let i = 0; i < count; i++) {
            if (!state.rows[row - 1 + i]) state.rows[row - 1 + i] = [];
            state.rows[row - 1 + i].splice(col - 1, width, ...values[i]);
          }
          return this;
        },
      };
    },
  };
  const config = { SPREADSHEET_ID: 'test-spreadsheet', SITE_ORIGIN: 'https://cj-sfsm.github.io', ...properties };
  const context = vm.createContext({
    PropertiesService: { getScriptProperties: () => ({
      getProperty: (key) => {
        if (key.startsWith('confirmation_email:') && state.mailStatusReadError) throw new Error('status unavailable');
        return config[key];
      },
      setProperty: (key, value) => {
        if (state.mailStatusWriteError) throw new Error('status unavailable');
        config[key] = value;
      },
    }) },
    MailApp: {
      getRemainingDailyQuota: () => {
        if (state.mailQuotaError) throw new Error('mail permission or quota unavailable');
        return state.mailQuota;
      },
      sendEmail: (message) => {
        if (state.locked || state.rows.length < 2) throw new Error('email attempted before saving/unlocking');
        state.mailAttempts++;
        if (state.onMailSend) state.onMailSend(message);
        if (state.mailError) throw new Error('mail service unavailable');
        state.sentEmails.push(JSON.parse(JSON.stringify(message)));
        state.mailQuota--;
      },
    },
    SpreadsheetApp: {
      openById: (id) => {
        if (id !== config.SPREADSHEET_ID) throw new Error('unexpected spreadsheet');
        return { getSheetByName: () => sheet, insertSheet: () => sheet };
      },
      flush: () => { if (state.flushError) throw new Error('simulated Google flush failure'); },
    },
    LockService: { getScriptLock: () => ({
      waitLock: () => { state.locked = true; },
      tryLock: () => { state.locked = !state.busy; return !state.busy; },
      releaseLock: () => { state.locked = false; },
    }) },
    Utilities: {
      DigestAlgorithm: { SHA_256: 'sha256' },
      computeDigest: (_, value) => Array.from(createHash('sha256').update(value).digest()),
    },
    HtmlService: {
      XFrameOptionsMode: { ALLOWALL: 'ALLOWALL' },
      createHtmlOutput: (html) => ({ html, setXFrameOptionsMode() { return this; } }),
    },
    UrlFetchApp: { fetch: () => {
      state.captchaCalls++;
      return { getResponseCode: () => 200, getContentText: () => JSON.stringify(state.captcha) };
    } },
  });
  vm.runInContext(fs.readFileSync(path.join(__dirname, '../../google-apps-script/Code.gs'), 'utf8'), context);
  context.setup();
  return {
    context, state,
    post: (parameters, contentLength = 1000) => context.doPost({ parameters, contentLength }),
  };
}

function submission(overrides = {}) {
  return {
    submission_id: [randomUUID()], nonce: [randomUUID()],
    site_origin: ['https://cj-sfsm.github.io'], elapsed_ms: ['10000'],
    language: ['fr'], presentation_type: ['Orale_12+3'],
    title: ['Étude des molécules'], authors: ['Marie DUPONT1, Alice MARTIN2'],
    affiliation_1: ['Université de Paris'], affiliation_2: ['Laboratoire de Lyon'],
    themes: ['Chimie Organique', 'Protéomique'],
    abstract: ['Un résumé scientifique avec des accents et une référence [1].'],
    ref_1: ['Une référence scientifique'], email: ['marie@example.org'], consent: ['yes'],
    ...overrides,
  };
}

function receipt(output) {
  const match = output.html.match(/window\.top\.postMessage\((\{.*\}),/);
  if (!match) throw new Error('missing acknowledgement');
  return JSON.parse(match[1]);
}

module.exports = { receiver, submission, receipt };

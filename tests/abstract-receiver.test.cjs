const { test } = require('node:test');
const assert = require('node:assert/strict');
const { randomUUID } = require('node:crypto');
const { receiver, submission, receipt } = require('./helpers/apps-script.cjs');

test('records all fields and multiple topics, returns only a receipt', () => {
  const app = receiver();
  const input = submission();
  const output = app.post(input);
  assert.equal(receipt(output).status, 'saved');
  assert.equal(receipt(output).emailStatus, 'sent');
  const fields = Object.fromEntries(app.state.rows[0].map((name, index) => [name, app.state.rows[1][index]]));
  assert.equal(fields.title, input.title[0]);
  assert.equal(fields.abstract, input.abstract[0]);
  assert.equal(fields.themes, input.themes.join('; '));
  assert.equal(fields.affiliation_2, input.affiliation_2[0]);
  assert.equal(fields.email, input.email[0]);
  assert.equal(fields.consent, 'yes');
  assert.equal(app.state.rows[0][0], 'received_at');
  assert.deepEqual(Array.from(app.state.rows[0].slice(-3)), ['language', 'submission_id', 'payload_hash']);
  assert.equal(fields.submission_id, input.submission_id[0]);
  assert.match(fields.payload_hash, /^[0-9a-f]{64}$/);
  assert.equal(fields.language, input.language[0]);
  assert.equal(app.state.rows[1].length, app.state.rows[0].length);
  assert.ok(!output.html.includes(input.email[0]));
  assert.ok(!output.html.includes(input.abstract[0]));
  assert.equal(app.state.locked, false);
});

test('75 submissions and retried receipts produce exactly 75 data rows', () => {
  const app = receiver();
  for (let i = 0; i < 75; i++) {
    const input = submission({ title: ['Abstract ' + i] });
    assert.equal(receipt(app.post(input)).status, 'saved');
    input.nonce = [randomUUID()];
    assert.equal(receipt(app.post(input)).status, 'saved');
  }
  assert.equal(app.state.rows.length, 76);
  assert.equal(app.state.sentEmails.length, 75);
});

test('does not reuse a receipt for altered content under the same ID', () => {
  const app = receiver();
  const input = submission();
  app.post(input);
  input.title = ['Changed after first save'];
  assert.equal(receipt(app.post(input)).status, 'conflict');
  assert.equal(app.state.rows.length, 2);
});

test('rejects invalid or abusive submissions before storing data', () => {
  const cases = [
    { _gotcha: ['spam'] }, { elapsed_ms: ['0'] }, { elapsed_ms: ['Infinity'] },
    { site_origin: ['https://evil.example'] }, { consent: [] }, { consent: ['on'] },
    { email: ['invalid'] }, { email: ['first,second@example.org'] },
    { email: ['first;second@example.org'] }, { email: ['Name<marie@example.org>'] },
    { affiliation_1: [' '] }, { title: ['x'.repeat(221)] },
    { title: ['first', 'second'] }, { title: ['bad\u0000title'] },
    { language: ['es'] }, { presentation_type: ['invalid'] },
    { themes: [] }, { themes: ['unexpected'] }, { themes: ['Imagerie', 'Imagerie'] },
    { themes: ['Autre'] }, { abstract: ['word '.repeat(301)] },
    { abstract: ['x'.repeat(2501)] }, { submission_id: ['invalid'] },
  ];
  for (const invalid of cases) {
    const app = receiver();
    assert.equal(receipt(app.post(submission(invalid))).status, 'invalid', JSON.stringify(invalid));
    assert.equal(app.state.rows.length, 1);
  }
  const app = receiver();
  assert.equal(receipt(app.post(submission(), 100001)).status, 'invalid');
  assert.equal(app.state.rows.length, 1);
});

test('accepts Other when specified and protects formula-like values', () => {
  const app = receiver();
  const input = submission({ themes: ['Autre'], theme_autre: ['Autre discipline'], title: ['=IMPORTXML("url")'], ref_1: ['+command'] });
  assert.equal(receipt(app.post(input)).status, 'saved');
  assert.ok(app.state.rows[1].includes('\'=IMPORTXML("url")'));
  assert.ok(app.state.rows[1].includes("'+command"));
});

test('busy or failed writes are never acknowledged as saved', () => {
  const app = receiver();
  const input = submission();
  app.state.busy = true;
  assert.equal(receipt(app.post(input)).status, 'busy');
  app.state.busy = false;
  app.state.writeError = true;
  assert.equal(receipt(app.post(input)).status, 'unavailable');
  assert.equal(app.state.rows.length, 1);
  assert.equal(app.state.locked, false);
  app.state.writeError = false;
  assert.equal(receipt(app.post(input)).status, 'saved');
});

test('lost acknowledgement after committed write can be retried without a duplicate', () => {
  const app = receiver();
  const input = submission();
  app.state.flushError = true;
  assert.equal(receipt(app.post(input)).status, 'unavailable');
  app.state.flushError = false;
  assert.equal(receipt(app.post(input)).status, 'saved');
  assert.equal(app.state.rows.length, 2);
});

test('preserves existing rows on setup and rejects changed headers', () => {
  const app = receiver();
  app.post(submission());
  app.context.setup();
  assert.equal(app.state.rows.length, 2);
  app.state.rows[0][0] = 'renamed';
  assert.equal(receipt(app.post(submission())).status, 'unavailable');
  assert.equal(app.state.rows.length, 2);
});

test('setup moves legacy metadata to the end without changing existing responses', () => {
  const app = receiver();
  const input = submission({ title: ['=FORMULA'], language: ['en'] });
  app.post(input);
  const expected = app.state.rows.map((row) => Array.from(row));
  const legacy = ['received_at', 'submission_id', 'payload_hash', 'language',
    'presentation_type', 'title', 'authors',
    'affiliation_1', 'affiliation_2', 'affiliation_3', 'affiliation_4', 'affiliation_5',
    'themes', 'theme_autre', 'abstract', 'ref_1', 'ref_2', 'ref_3', 'ref_4', 'ref_5', 'ref_6', 'email', 'consent'];
  app.state.rows = expected.map((row) => legacy.map((header) => row[expected[0].indexOf(header)]));

  // A newly deployed receiver can still handle the old layout before setup.
  assert.equal(receipt(app.post(input)).status, 'saved');
  const beforeMigration = submission({ title: ['Saved before migration'] });
  assert.equal(receipt(app.post(beforeMigration)).status, 'saved');
  assert.equal(app.state.rows[2][legacy.indexOf('title')], beforeMigration.title[0]);
  const original = app.state.rows.map((row) => Object.fromEntries(legacy.map((header, i) => [header, row[i]])));

  app.context.setup();
  assert.equal(app.state.columnMoves, 2);
  const migrated = app.state.rows.map((row) => Object.fromEntries(app.state.rows[0].map((header, i) => [header, row[i]])));
  assert.deepEqual(migrated, original);
  assert.deepEqual(Array.from(app.state.rows[0]), expected[0]);
  assert.deepEqual(Array.from(app.state.rows[1]), expected[1]);

  // Migration is repeatable and deduplication still finds old submissions.
  app.context.setup();
  assert.equal(app.state.columnMoves, 2);
  assert.equal(receipt(app.post(input)).status, 'saved');
  assert.equal(receipt(app.post(beforeMigration)).status, 'saved');
  input.title = ['Changed after migration'];
  assert.equal(receipt(app.post(input)).status, 'conflict');
  assert.equal(app.state.rows.length, 3);
  const afterMigration = submission({ title: ['Saved after migration'] });
  assert.equal(receipt(app.post(afterMigration)).status, 'saved');
  assert.equal(app.state.rows[3][app.state.rows[0].indexOf('title')], afterMigration.title[0]);
  assert.equal(app.state.rows[3].at(-2), afterMigration.submission_id[0]);
});

test('setup moves trailing language before ID and hash without changing responses', () => {
  const app = receiver();
  const input = submission({ language: ['en'] });
  app.post(input);
  const expected = app.state.rows.map((row) => Array.from(row));
  const previous = expected[0].slice(0, -3).concat(['submission_id', 'payload_hash', 'language']);
  app.state.rows = expected.map((row) => previous.map((header) => row[expected[0].indexOf(header)]));
  assert.equal(receipt(app.post(input)).status, 'saved');
  app.context.setup();
  assert.equal(app.state.columnMoves, 1);
  assert.deepEqual(app.state.rows.map((row) => Array.from(row)), expected);
  assert.equal(receipt(app.post(input)).status, 'saved');
  assert.equal(app.state.rows.length, 2);
  app.context.setup();
  assert.equal(app.state.columnMoves, 1);
});

test('setup refuses an unknown schema without moving or overwriting responses', () => {
  const app = receiver();
  app.post(submission());
  app.state.rows[0][1] = 'renamed';
  const before = JSON.stringify(app.state.rows);
  assert.throws(() => app.context.setup(), /preserve column headers/);
  assert.equal(JSON.stringify(app.state.rows), before);
  assert.equal(app.state.columnMoves, 0);
  assert.equal(app.state.locked, false);
});

test('checks CAPTCHA on the server, including hostname and action', () => {
  const app = receiver({ TURNSTILE_SECRET_KEY: 'test-secret' });
  const input = submission({ 'cf-turnstile-response': ['test-token'] });
  app.state.captcha.hostname = 'evil.example';
  assert.equal(receipt(app.post(input)).status, 'captcha');
  app.state.captcha.hostname = 'cj-sfsm.github.io';
  app.state.captcha.action = 'other';
  assert.equal(receipt(app.post(input)).status, 'captcha');
  app.state.captcha.action = 'abstract';
  assert.equal(receipt(app.post(input)).status, 'saved');
  assert.equal(app.state.captchaCalls, 3);
  input.nonce = [randomUUID()];
  assert.equal(receipt(app.post(input)).status, 'saved');
  assert.equal(app.state.captchaCalls, 3);
  assert.equal(app.state.rows.length, 2);
});

test('escapes script markup in receipts and exposes no public reader', () => {
  const app = receiver();
  const output = app.context.receipt_('https://cj-sfsm.github.io', '</script><script>attack()', '', 'invalid');
  assert.equal(output.html.split('</script>').length, 2);
  assert.ok(output.html.includes('"https://cj-sfsm.github.io"'));
  app.post(submission());
  assert.ok(!app.context.doGet().html.includes('marie@example.org'));
});

test('sends one localized confirmation to the participant only, after saving and unlocking', () => {
  for (const language of ['fr', 'en']) {
    const app = receiver();
    const input = submission({ language: [language] });
    let savedAtSend = false;
    app.state.onMailSend = () => {
      savedAtSend = app.state.rows.length === 2 && app.state.locked === false;
    };
    assert.equal(receipt(app.post(input)).emailStatus, 'sent');
    assert.equal(savedAtSend, true);
    assert.equal(app.state.sentEmails.length, 1);
    const message = app.state.sentEmails[0];
    assert.equal(message.to, input.email[0]);
    assert.equal(message.cc, undefined);
    assert.equal(message.bcc, undefined);
    assert.equal(message.replyTo, 'clubjeunesm@gmail.com');
    assert.ok(message.body.includes(input.submission_id[0]));
    assert.ok(message.body.includes(input.title[0]));
    assert.ok(!message.body.includes(input.abstract[0]));
    assert.match(message.subject, language === 'en' ? /Abstract submission received/ : /Confirmation de dépôt/);
    input.nonce = [randomUUID()];
    assert.equal(receipt(app.post(input)).emailStatus, 'sent');
    assert.equal(app.state.mailAttempts, 1);
  }
});

test('quota exhaustion saves the abstract and reports email failure without retrying mail', () => {
  const app = receiver();
  app.state.mailQuota = 0;
  const input = submission();
  const result = receipt(app.post(input));
  assert.equal(result.status, 'saved');
  assert.equal(result.emailStatus, 'failed');
  assert.equal(app.state.rows.length, 2);
  assert.equal(app.state.mailAttempts, 0);
  app.state.mailQuota = 100;
  assert.equal(receipt(app.post(input)).emailStatus, 'failed');
  assert.equal(app.state.mailAttempts, 0);
});

test('permission and mail provider errors cannot invalidate a saved abstract', () => {
  for (const failure of ['mailQuotaError', 'mailError']) {
    const app = receiver();
    app.state[failure] = true;
    const input = submission();
    const result = receipt(app.post(input));
    assert.equal(result.status, 'saved');
    assert.equal(result.emailStatus, 'failed');
    assert.equal(app.state.rows.length, 2);
    assert.equal(app.state.locked, false);
    const attempts = app.state.mailAttempts;
    app.state[failure] = false;
    assert.equal(receipt(app.post(input)).emailStatus, 'failed');
    assert.equal(app.state.mailAttempts, attempts);
  }
});

test('a concurrent retry while mail is in progress does not send another message', () => {
  const app = receiver();
  const input = submission();
  let retried;
  app.state.onMailSend = () => {
    retried = receipt(app.post({ ...input, nonce: [randomUUID()] }));
  };
  assert.equal(receipt(app.post(input)).emailStatus, 'sent');
  assert.equal(retried.status, 'saved');
  assert.equal(retried.emailStatus, 'unknown');
  assert.equal(app.state.sentEmails.length, 1);
  assert.equal(app.state.rows.length, 2);
  assert.equal(receipt(app.post(input)).emailStatus, 'sent');
});

test('failure to store or read mail status never resends email or rejects the abstract', () => {
  for (const failure of ['mailStatusWriteError', 'mailStatusReadError']) {
    const app = receiver();
    app.state[failure] = true;
    const input = submission();
    assert.equal(receipt(app.post(input)).emailStatus, 'sent');
    const retried = receipt(app.post(input));
    assert.equal(retried.status, 'saved');
    assert.equal(retried.emailStatus, 'unknown');
    assert.equal(app.state.sentEmails.length, 1);
    assert.equal(app.state.rows.length, 2);
  }
});

test('a failed or conflicting abstract never triggers a confirmation email', () => {
  const app = receiver();
  const input = submission();
  app.state.writeError = true;
  assert.equal(receipt(app.post(input)).status, 'unavailable');
  assert.equal(app.state.mailAttempts, 0);
  app.state.writeError = false;
  app.state.flushError = true;
  assert.equal(receipt(app.post(input)).status, 'unavailable');
  assert.equal(app.state.mailAttempts, 0);
  app.state.flushError = false;
  const retried = receipt(app.post(input));
  assert.equal(retried.status, 'saved');
  assert.equal(retried.emailStatus, 'unknown');
  assert.equal(app.state.mailAttempts, 0);
  input.title = ['Conflicting content'];
  assert.equal(receipt(app.post(input)).status, 'conflict');
  assert.equal(app.state.mailAttempts, 0);
});

(() => {
  "use strict";

  const googleOrigin = /^https:\/\/(?:script\.google\.com|(?:[a-z0-9-]+[.-])?script\.googleusercontent\.com)$/;
  const element = (tag, properties) => Object.assign(document.createElement(tag), properties);
  const wait = (delay) => new Promise((resolve) => setTimeout(resolve, delay));
  let turnstileLoading;

  function loadTurnstile() {
    return turnstileLoading ||= new Promise((resolve, reject) => {
      document.head.append(element("script", {
        src: "https://challenges.cloudflare.com/turnstile/v0/api.js?render=explicit",
        onload: resolve,
        onerror: reject,
      }));
    });
  }

  function post(endpoint, payload, submissionId) {
    return new Promise((resolve) => {
      const nonce = crypto.randomUUID();
      const frame = element("iframe", { name: `abstract-${nonce}`, hidden: true, title: "Abstract submission" });
      const transport = element("form", {
        action: endpoint, method: "POST", target: frame.name, hidden: true, acceptCharset: "UTF-8",
      });
      for (const [name, value] of [...payload, ["submission_id", submissionId], ["nonce", nonce], ["site_origin", location.origin]]) {
        transport.append(element("input", { type: "hidden", name, value }));
      }
      function finish(result) {
        clearTimeout(timeout);
        window.removeEventListener("message", receive);
        transport.remove();
        frame.remove();
        resolve(result);
      }
      function receive({ origin, data }) {
        // Apps Script replies from a nested Google iframe. Check its origin and
        // the per-attempt nonce; an iframe load alone never confirms a save.
        if (googleOrigin.test(origin) && data?.type === "rcjsm-abstract" &&
            data.nonce === nonce && data.submissionId === submissionId) finish(data);
      }
      const timeout = setTimeout(() => finish({ status: "timeout" }), 30000);
      window.addEventListener("message", receive);
      document.body.append(frame, transport);
      try {
        HTMLFormElement.prototype.submit.call(transport);
      } catch {
        finish({ status: "unavailable" });
      }
    });
  }

  document.querySelectorAll("[data-abstract-form]").forEach((form) => {
    const { dataset: messages, elements } = form;
    const fieldset = form.querySelector("fieldset");
    const button = form.querySelector('[type="submit"]');
    const status = form.querySelector("[data-abstract-status]");
    const abstract = elements.namedItem("abstract");
    const topics = elements.namedItem("themes");
    const startedAt = Date.now();
    let sending = false, lastPayload = "", submissionId, captchaWidget, captchaToken = "";

    function show(message, state = "pending") {
      status.textContent = message;
      status.dataset.state = state;
    }
    function validateWords() {
      abstract.setCustomValidity(abstract.value.trim().split(/\s+/u).length > 300 ? messages.msgWordLimit : "");
    }
    function setCaptcha(token = "") {
      captchaToken = token;
      button.disabled = !token || fieldset.disabled;
    }
    abstract.addEventListener("input", validateWords);
    topics.addEventListener("change", () => {
      elements.namedItem("theme_autre").required = [...topics.selectedOptions].some(({ value }) => value === "Autre");
    });
    button.disabled = Boolean(messages.turnstileKey);
    if (messages.turnstileKey) {
      loadTurnstile().then(() => {
        captchaWidget = window.turnstile.render(form.querySelector("[data-abstract-captcha]"), {
          sitekey: messages.turnstileKey,
          action: "abstract",
          language: messages.language,
          callback: setCaptcha,
          "expired-callback": () => setCaptcha(),
          "error-callback": () => { setCaptcha(); show(messages.msgCaptcha, "error"); },
        });
      }).catch(() => show(messages.msgCaptcha, "error"));
    }
    window.addEventListener("beforeunload", (event) => {
      if (sending) {
        event.preventDefault();
        event.returnValue = "";
      }
    });

    form.addEventListener("submit", async (event) => {
      event.preventDefault();
      if (fieldset.disabled) return;
      validateWords();
      if (!form.reportValidity()) return;
      if (messages.turnstileKey && !captchaToken) return show(messages.msgCaptcha, "error");

      const payload = new FormData(form);
      payload.delete("cf-turnstile-response");
      payload.set("language", messages.language);
      const fingerprint = JSON.stringify([...payload]);
      if (fingerprint !== lastPayload) {
        submissionId = crypto.randomUUID();
        lastPayload = fingerprint;
      }
      payload.set("elapsed_ms", String(Date.now() - startedAt));
      payload.set("cf-turnstile-response", captchaToken);
      sending = fieldset.disabled = true;
      form.setAttribute("aria-busy", "true");
      show(messages.msgSending);

      let result;
      try {
        for (let attempt = 0; attempt < 3; attempt++) {
          if (attempt) {
            show(messages.msgRetrying);
            await wait(2000 * attempt + Math.random() * 2000);
          }
          result = await post(form.action, payload, submissionId);
          if (!["busy", "unavailable", "timeout"].includes(result.status)) break;
        }
      } catch {
        result = { status: "unavailable" };
      } finally {
        sending = false;
        form.removeAttribute("aria-busy");
      }

      fieldset.disabled = result.status === "saved";
      if (fieldset.disabled) {
        const emailMessage = result.emailStatus === "sent" ? messages.msgEmailSent
          : result.emailStatus === "failed" ? messages.msgEmailFailed : messages.msgEmailUnknown;
        show(`${messages.msgSuccess} ${submissionId}\n\n${emailMessage}`,
          result.emailStatus === "sent" ? "success" : "warning");
      } else {
        show(result.status === "invalid" ? messages.msgInvalid
          : result.status === "captcha" ? messages.msgCaptcha : messages.msgError, "error");
        if (captchaWidget !== undefined) {
          setCaptcha();
          window.turnstile.reset(captchaWidget);
        }
      }
      status.focus();
    });
  });
})();

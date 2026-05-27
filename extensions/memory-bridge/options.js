"use strict";

const DEFAULTS = {
  bridgeUrl: "http://127.0.0.1:8730",
  token: "",
  topK: 5,
  enabled: false
};

const fields = {
  enabled: document.getElementById("enabled"),
  bridgeUrl: document.getElementById("bridgeUrl"),
  token: document.getElementById("token"),
  topK: document.getElementById("topK"),
  status: document.getElementById("status")
};

async function load() {
  const cfg = await chrome.storage.sync.get(DEFAULTS);
  fields.enabled.checked = Boolean(cfg.enabled);
  fields.bridgeUrl.value = cfg.bridgeUrl;
  fields.token.value = cfg.token;
  fields.topK.value = cfg.topK;
}

async function save() {
  await chrome.storage.sync.set({
    enabled: fields.enabled.checked,
    bridgeUrl: fields.bridgeUrl.value.trim() || DEFAULTS.bridgeUrl,
    token: fields.token.value,
    topK: Math.max(1, Math.min(20, Number(fields.topK.value) || DEFAULTS.topK))
  });
  fields.status.textContent = "Saved";
}

async function health() {
  await save();
  const response = await chrome.runtime.sendMessage({ type: "health" });
  if (response && response.error) {
    fields.status.textContent = response.error;
  } else {
    fields.status.textContent = `Bridge ${response.version || "ok"}`;
  }
}

document.getElementById("save").addEventListener("click", () => {
  save().catch((error) => {
    fields.status.textContent = error.message || String(error);
  });
});

document.getElementById("health").addEventListener("click", () => {
  health().catch((error) => {
    fields.status.textContent = error.message || String(error);
  });
});

load().catch((error) => {
  fields.status.textContent = error.message || String(error);
});

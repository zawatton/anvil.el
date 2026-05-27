"use strict";

const DEFAULTS = {
  bridgeUrl: "http://127.0.0.1:8730",
  token: "",
  topK: 5,
  enabled: false
};

async function settings() {
  return chrome.storage.sync.get(DEFAULTS);
}

function endpoint(base, path, params) {
  const url = new URL(path, base.replace(/\/+$/, "") + "/");
  if (params) {
    for (const [key, value] of Object.entries(params)) {
      if (value !== undefined && value !== null && value !== "") {
        url.searchParams.set(key, String(value));
      }
    }
  }
  return url.toString();
}

async function bridgeFetch(path, init = {}, params = null) {
  const cfg = await settings();
  const headers = Object.assign({}, init.headers || {});
  if (cfg.token) {
    headers.Authorization = `Bearer ${cfg.token}`;
  }
  const response = await fetch(endpoint(cfg.bridgeUrl, path, params), {
    ...init,
    headers
  });
  const text = await response.text();
  let body = null;
  if (text) {
    try {
      body = JSON.parse(text);
    } catch (_err) {
      body = text;
    }
  }
  if (!response.ok) {
    const message = body && body.error ? body.error : response.statusText;
    throw new Error(message);
  }
  return { body, version: response.headers.get("anvil-memory-bridge-version") };
}

function memoryBlock(rows) {
  if (!Array.isArray(rows) || rows.length === 0) {
    return "";
  }
  const items = rows.map((row) => {
    const title = row.name || row.file || "memory";
    const snippet = row.snippet || row.description || row.body || "";
    return `<memory title="${String(title).replaceAll('"', "'")}">${snippet}</memory>`;
  });
  return `${items.join("\n")}\n\n`;
}

async function searchMemory(query) {
  const cfg = await settings();
  if (!cfg.enabled) {
    return { block: "", rows: [] };
  }
  const result = await bridgeFetch("/memory/search", {}, {
    q: query || "",
    limit: cfg.topK || DEFAULTS.topK
  });
  return { block: memoryBlock(result.body), rows: result.body || [] };
}

async function saveMemory(payload) {
  const cfg = await settings();
  if (!cfg.enabled) {
    throw new Error("Anvil Memory Bridge extension is disabled.");
  }
  return bridgeFetch("/memory/save", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify(payload)
  });
}

chrome.runtime.onMessage.addListener((message, _sender, sendResponse) => {
  (async () => {
    if (message && message.type === "search-memory") {
      sendResponse(await searchMemory(message.query));
    } else if (message && message.type === "save-memory") {
      sendResponse(await saveMemory(message.payload));
    } else if (message && message.type === "health") {
      sendResponse(await bridgeFetch("/memory/health"));
    } else {
      throw new Error("Unknown anvil memory bridge message.");
    }
  })().catch((error) => {
    sendResponse({ error: error.message || String(error) });
  });
  return true;
});

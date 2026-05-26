"use strict";

const SETTINGS_KEY = "anvilMobileMemorySettings";
const QUEUE_KEY = "anvilMobileMemoryQueue";

const defaults = {
  bridgeUrl: "http://127.0.0.1:8730",
  token: ""
};

const ui = {
  bridgeUrl: document.getElementById("bridgeUrl"),
  token: document.getElementById("token"),
  status: document.getElementById("status"),
  query: document.getElementById("query"),
  results: document.getElementById("results"),
  name: document.getElementById("name"),
  type: document.getElementById("type"),
  body: document.getElementById("body")
};

function loadSettings() {
  try {
    return Object.assign({}, defaults, JSON.parse(localStorage.getItem(SETTINGS_KEY) || "{}"));
  } catch (_err) {
    return defaults;
  }
}

function saveSettings() {
  const settings = {
    bridgeUrl: ui.bridgeUrl.value.trim() || defaults.bridgeUrl,
    token: ui.token.value
  };
  localStorage.setItem(SETTINGS_KEY, JSON.stringify(settings));
  setStatus("settings saved");
  return settings;
}

function queue() {
  try {
    const value = JSON.parse(localStorage.getItem(QUEUE_KEY) || "[]");
    return Array.isArray(value) ? value : [];
  } catch (_err) {
    return [];
  }
}

function setQueue(items) {
  localStorage.setItem(QUEUE_KEY, JSON.stringify(items));
  setStatus(`offline queue: ${items.length}`);
}

function setStatus(text) {
  ui.status.textContent = text;
}

function endpoint(path, params) {
  const settings = loadSettings();
  const url = new URL(path, settings.bridgeUrl.replace(/\/+$/, "") + "/");
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
  const settings = loadSettings();
  const headers = Object.assign({}, init.headers || {});
  if (settings.token) {
    headers.Authorization = `Bearer ${settings.token}`;
  }
  const response = await fetch(endpoint(path, params), {
    ...init,
    headers
  });
  const text = await response.text();
  let body = null;
  if (text) {
    body = JSON.parse(text);
  }
  if (!response.ok) {
    throw new Error(body && body.error ? body.error : response.statusText);
  }
  return body;
}

function memoryPayload() {
  const body = ui.body.value.trim();
  if (!body) {
    throw new Error("body is required");
  }
  return {
    name: ui.name.value.trim() || `mobile_note_${Date.now()}`,
    type: ui.type.value || "memo",
    description: ui.name.value.trim() || "Mobile memory",
    body,
    tags: ["mobile", "memory-bridge"]
  };
}

async function saveMemory(payload) {
  return bridgeFetch("/memory/save", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify(payload)
  });
}

async function saveOrQueue() {
  const payload = memoryPayload();
  try {
    await saveMemory(payload);
    ui.body.value = "";
    setStatus("saved");
  } catch (error) {
    const items = queue();
    items.push({ payload, queuedAt: Date.now(), error: error.message || String(error) });
    setQueue(items);
  }
}

async function flushQueue() {
  const pending = queue();
  const remaining = [];
  for (const item of pending) {
    try {
      await saveMemory(item.payload);
    } catch (error) {
      remaining.push(Object.assign({}, item, {
        error: error.message || String(error),
        lastTriedAt: Date.now()
      }));
    }
  }
  setQueue(remaining);
}

async function search() {
  const rows = await bridgeFetch("/memory/search", {}, {
    q: ui.query.value.trim(),
    limit: 10
  });
  ui.results.replaceChildren(...rows.map((row) => {
    const div = document.createElement("div");
    div.className = "result";
    const title = document.createElement("strong");
    title.textContent = row.name || row.file || "memory";
    const snippet = document.createElement("p");
    snippet.textContent = row.snippet || row.description || row.body || "";
    div.append(title, snippet);
    return div;
  }));
  setStatus(`${rows.length} results`);
}

async function health() {
  const body = await bridgeFetch("/memory/health");
  setStatus(`bridge ${body.version || "ok"} rows=${body.rows}`);
}

function init() {
  const settings = loadSettings();
  ui.bridgeUrl.value = settings.bridgeUrl;
  ui.token.value = settings.token;
  setQueue(queue());
  document.getElementById("saveSettings").addEventListener("click", saveSettings);
  document.getElementById("health").addEventListener("click", () => health().catch((error) => setStatus(error.message)));
  document.getElementById("search").addEventListener("click", () => search().catch((error) => setStatus(error.message)));
  document.getElementById("saveMemory").addEventListener("click", () => saveOrQueue().catch((error) => setStatus(error.message)));
  document.getElementById("flush").addEventListener("click", () => flushQueue().catch((error) => setStatus(error.message)));
  window.addEventListener("online", () => flushQueue().catch((error) => setStatus(error.message)));
}

init();

"use strict";

function activeEditor() {
  const node = document.activeElement;
  if (!node) {
    return null;
  }
  if (node.matches && node.matches("textarea, input[type='text']")) {
    return node;
  }
  if (node.isContentEditable) {
    return node;
  }
  return node.closest && node.closest("[contenteditable='true']");
}

function editorText(editor) {
  if (!editor) {
    return "";
  }
  if ("value" in editor) {
    return editor.value || "";
  }
  return editor.innerText || editor.textContent || "";
}

function setEditorText(editor, text) {
  if ("value" in editor) {
    editor.value = text;
    editor.dispatchEvent(new Event("input", { bubbles: true }));
    return;
  }
  editor.focus();
  document.execCommand("selectAll", false, null);
  document.execCommand("insertText", false, text);
  editor.dispatchEvent(new InputEvent("input", { bubbles: true, data: text }));
}

function selectedText() {
  const selection = window.getSelection();
  return selection ? String(selection).trim() : "";
}

async function injectMemory() {
  const editor = activeEditor();
  const text = editorText(editor);
  if (!editor || !text.trim()) {
    return;
  }
  const response = await chrome.runtime.sendMessage({
    type: "search-memory",
    query: text.slice(0, 500)
  });
  if (response && response.error) {
    throw new Error(response.error);
  }
  if (response && response.block) {
    setEditorText(editor, `${response.block}${text}`);
  }
}

async function saveSelection() {
  const text = selectedText();
  if (!text) {
    return;
  }
  const name = `browser_capture_${Date.now()}`;
  const response = await chrome.runtime.sendMessage({
    type: "save-memory",
    payload: {
      name,
      type: "memo",
      description: "Browser chat capture",
      body: text,
      tags: ["browser", "memory-bridge"]
    }
  });
  if (response && response.error) {
    throw new Error(response.error);
  }
}

document.addEventListener("keydown", (event) => {
  if (!event.altKey || !event.ctrlKey || event.metaKey) {
    return;
  }
  if (event.key.toLowerCase() === "m") {
    event.preventDefault();
    injectMemory().catch((error) => console.warn("anvil-memory inject failed", error));
  } else if (event.key.toLowerCase() === "s") {
    event.preventDefault();
    saveSelection().catch((error) => console.warn("anvil-memory save failed", error));
  }
});

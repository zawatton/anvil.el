#!/usr/bin/env python3
"""Render docs/standalone-module-coverage.md from a census results.tsv.

Usage: standalone-coverage-table.py RESULTS_TSV OUT_MD
The blocker classes below were assigned by hand after reading each first
blocker (see the doc's "Classes" section); the measured columns come only
from the census output.
"""
import re
import sys
from pathlib import Path

LOADED = {"anvil", "anvil-server-commands", "anvil-context", "anvil-context-batch",
          "anvil-context-dependencies", "anvil-defs", "anvil-context-project",
          "anvil-disclosure", "anvil-memory", "anvil-worklog", "anvil-lint", "anvil-cron",
          "anvil-file", "anvil-compact", "anvil-session", "anvil-shell-filter",
          "anvil-state",
          "anvil-agent", "anvil-data", "anvil-dev", "anvil-discovery",
          "anvil-harness-telemetry", "anvil-session-store",
          "anvil-bisect", "anvil-cad",
          "anvil-orchestrator-routing", "anvil-manifest",
          "anvil-eval", "anvil-semantic"}

# Modules that load under the loader but register only some (or none) of their
# tools after real-call screening: module -> (tools column, class, note).
PARTIAL = {
    "anvil-sexp": ("3 of 8 (read-file, surrounding-form, replace-defun)", "ok",
                   "S10: loads via a `define-compilation-mode` fallback and a temporarily declared `bytecomp`; the rest need syntax-ppss/bounds-of-thing-at-point, byte-compile or checkdoc, or answer wrongly (macroexpand)"),
    "anvil-elisp": ("0 of 6", "a",
                    "S10: loads (not loaded by the loader), but every tool fails or answers wrongly on standalone (no docstrings, documentation-property/find-lisp-object-file-name/shell-quote-argument void, ert finds 0 tests, byte-compile impossible)"),
}

# Port candidates (tools register) that a real-call screening rejected:
# module -> (class, note).  See the doc's "Real-call screening" section.
SCREENED = {
    "anvil-git": ("a", "registers; real calls blocked (see screening)"),
    "anvil-git-msg": ("a", "registers; real calls blocked (see screening)"),
    "anvil-sqlite": ("a", "registers; `sqlite-select` lacks return type `set`"),
    "anvil-ts": ("a", "registers; treesit subprocess `with-root` not implemented"),
    "anvil-py": ("a", "registers; treesit subprocess `with-root` not implemented"),
    "anvil-js": ("a", "registers; treesit subprocess `with-root` not implemented"),
    "anvil-browser": ("a", "registers; `browser-fetch` fails (process coding support)"),
    "anvil-org-index": ("c", "registers; uses the host DB and `message`, needs paths"),
    "anvil-claude-watchdog": ("c", "registers; watches the host's Claude processes"),
    # 2026-11-02 screening
    "anvil-manifest": ("d", "screened: registers on server-id \"default\"; quoted spec symbols"),
    "anvil-orchestrator-routing": ("d", "screened: `anvil-orchestrator-stats` void (missing require)"),
    "anvil-buffer": ("a", "screened: buffer primitives misbehave after the module loads"),
    "anvil-fusion-mcp": ("d", "screened: loads with tabulated-list; not ported (ask calls LLMs, poller has no job source)"),
    "anvil-fusion-longrun-mcp": ("d", "screened: loads with tabulated-list; not ported (start/resume spawn workers, poller has no job source)"),
    "anvil-mu4e": ("c", "screened: not called (touches mail)"),
}

# Substring of the first blocker -> (class, root cause, effort)
RULES = [
    ("\"org-id\"", "b", "org-id.el missing (only the lightweight Org subset loads)", "M"),
    ("coding-system-p", "a", "`coding-system-p` not bound", "S"),
    ("broker did not become ready", "c", "needs the node-pty broker process", "-"),
    ("Malformed arglist", "a", "vendored filenotify.el `cl-defun` destructuring arglist", "M"),
    ("define-compilation-mode", "a", "compile.el loads but `define-compilation-mode` is not bound", "S"),
    ("tabulated-list-mode-map", "b", "tabulated-list.el missing", "S"),
    ("\"tabulated-list\"", "b", "tabulated-list.el missing", "S"),
    ("\"profiler\"", "b", "profiler.el missing", "S"),
    ("\"mailcap\"", "b", "mailcap.el missing (url stack)", "M"),
    ("\"url-util\"", "b", "url-util.el missing (url stack)", "M"),
    ("\"url-http\"", "b", "url-http.el missing (url stack + network)", "L"),
    ("\"mail-utils\"", "c", "Wanderlust/mail stack", "L"),
    ("nl-llm-agent-provider", "b", "nl-llm-agent-provider not on load-path", "S"),
    ("internal--daemon-sockname", "c", "needs a running Emacs daemon", "-"),
    ("node-pty not installed", "c", "needs node-pty broker process", "-"),
    ("emacs-cc-treesit-available-p", "c", "needs tree-sitter C runtime", "-"),
    ("malformed spec", "d", "demo spec shape rejected by real anvil-server", "S"),
]


def classify(text):
    for needle, cls, cause, effort in RULES:
        if needle in text:
            return cls, cause, effort
    return "?", text[:80], "?"


def main():
    rows = []
    for line in Path(sys.argv[1]).read_text().splitlines():
        f = line.split("\t")
        if len(f) < 6 or f[0] != "COVERAGE":
            continue
        mod, load, lerr, reg, added = f[1], f[2], f[3], f[4], f[5]
        rerr = f[6] if len(f) > 6 else ""
        if mod in PARTIAL:
            tools, cls, cause = PARTIAL[mod]
            status, effort = "yes", "-"
        elif mod in LOADED:
            status, tools, cls, cause, effort = "yes", "already loaded", "ok", "in standalone loader", "-"
        elif load != "pass":
            cls, cause, effort = classify(lerr)
            status, tools = "no", "-"
        elif reg == "fail":
            cls, cause, effort = classify(rerr)
            status, tools = "yes", "enable fails"
        elif reg == "pass" and mod in SCREENED:
            cls, cause = SCREENED[mod]
            status, tools = "yes", f"yes ({added})"
            effort = "S" if cls == "port" else "-"
        elif reg == "pass":
            status, tools, cls, cause, effort = "yes", f"yes ({added})", "port", "not in loader", "S"
        elif reg == "no-enable":
            status, tools, cls, cause, effort = "yes", "no `-enable`", "lib", "library or registered by a parent", "-"
        else:
            status, tools, cls, cause, effort = "yes", "0 tools", "lib", "`-enable` runs but registers no MCP tools", "-"
        rows.append((mod, status, tools, cls, cause, effort))
    out = [Path(sys.argv[2]).read_text().split("<!-- TABLE -->")[0] + "<!-- TABLE -->", "",
           "| module | loads? | tools register? | class | first blocker / note | effort |",
           "|---|---|---|---|---|---|"]
    for r in rows:
        out.append("| " + " | ".join(re.sub(r"\|", "\\|", c) for c in r) + " |")
    Path(sys.argv[2]).write_text("\n".join(out) + "\n")


if __name__ == "__main__":
    main()

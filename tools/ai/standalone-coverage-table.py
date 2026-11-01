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
          "anvil-disclosure", "anvil-memory", "anvil-worklog", "anvil-lint", "anvil-cron"}

# Substring of the first blocker -> (class, root cause, effort)
RULES = [
    ("nelisp-ec-buffer", "a", "`(require 'org)` fails on NeLisp (buffer object used as array)", "L"),
    ("run-with-idle-timer", "a", "`run-with-idle-timer` not bound", "S"),
    ("run-with-timer", "a", "`run-with-timer` not bound", "S"),
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
        if mod in LOADED:
            status, tools, cls, cause, effort = "yes", "already loaded", "ok", "in standalone loader", "-"
        elif load != "pass":
            cls, cause, effort = classify(lerr)
            status, tools = "no", "-"
        elif reg == "fail":
            cls, cause, effort = classify(rerr)
            status, tools = "yes", "enable fails"
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

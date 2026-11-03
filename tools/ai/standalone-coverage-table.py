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
    "anvil-org": ("4 of 14 (read-file, get-allowed-files, get-todo-config, get-tag-config)", "ok",
                  "S11: loads with shims (version<, org-version, url-unhex-string behind a temporary url-util feature, rx, subr-arity); the other 10 need org-current-level/org-outline-level/org-back-to-heading, org-capture and org-agenda machinery, or user-init-file, which the lightweight Org subset lacks"),
    "anvil-defs": ("14 of 14", "ok",
                   "S12/S13.3: all 14 registered (defs-index, defs-index-update and the 11 above, plus defs-detect-changes from port 16, screened by real call); the index uses an isolated DB (ANVIL_DEFS_INDEX_DB, default $XDG_CACHE_HOME/anvil-standalone/anvil-defs-index.db), never the host's; defs-detect-changes emulates the ignored make-process cwd with a cd prefix while its handler runs"),
    "anvil-orchestrator": ("21 of 23 (all but cancel, stop)", "ok",
                           "S12: port 12 registers the tool specs without the enable function (no state rehydration, no timers; `timer-list` and `process-list` shims). State DB and job dir are isolated (ANVIL_STATE_DB, ANVIL_ORCHESTRATOR_WORK_DIR under $XDG_CACHE_HOME/anvil-standalone). cancel and stop stay unregistered: a signalled task reports exit-code 0 where the host reports 15 (wave 3)"),
    "anvil-elisp": ("0 of 6", "a",
                    "S10: loads (not loaded by the loader), but every tool fails or answers wrongly on standalone (no docstrings, documentation-property/find-lisp-object-file-name/shell-quote-argument void, ert finds 0 tests, byte-compile impossible)"),
}

# Port candidates (tools register) that a real-call screening rejected:
# module -> (class, note).  See the doc's "Real-call screening" section.
SCREENED = {
    "anvil-git": ("a", "registers; real calls blocked (see screening)"),
    "anvil-git-msg": ("a", "registers; real calls blocked (see screening)"),
    "anvil-sqlite": ("ok", "S12.5: `sqlite-query` registered; fboundp-guarded `sqlite-select` `set` cursor shim (`sqlite-more-p`/`sqlite-next`/`sqlite-finalize`)"),
    "anvil-ts": ("a", "registers; treesit subprocess `with-root` not implemented"),
    "anvil-py": ("a", "registers; treesit subprocess `with-root` not implemented"),
    "anvil-js": ("a", "registers; treesit subprocess `with-root` not implemented"),
    "anvil-http": ("ok", "S13.4: 7 tools served (http-fetch, http-head, http-post, http-robots-check, http-cache-status, http-cache-index, http-cache-get) screened against the host on a fixture; http-fetch-batch and http-cache-clear unregistered"),
    "anvil-web": ("ok", "S13.4: web-tweet-fetch screened against the host on a fixture; web-reddit-thread unregistered"),
    "anvil-browser": ("a", "screened (S13.3): all 5 registered; error replies equal the host (Chrome cannot start here), success path unverified"),
    "anvil-org-index": ("ok", "S12.6: 2 of 2 (`org-index-search`, `org-index-rebuild`); DB in the isolated cache, rebuild runs in a bounded standalone child, `message` muted"),
    "anvil-claude-watchdog": ("c", "registers; watches the host's Claude processes"),
    # 2026-11-02 screening
    "anvil-manifest": ("d", "screened: registers on server-id \"default\"; quoted spec symbols"),
    "anvil-orchestrator-routing": ("d", "screened: `anvil-orchestrator-stats` void (missing require)"),
    "anvil-buffer": ("a", "screened: buffer primitives misbehave after the module loads"),
    "anvil-dashboard": ("ok", "screened S12.7: `dashboard-snapshot`, `-serve-start`, `-serve-stop` registered (optional require); HTTP probes match host; start result fixed to be JSON-safe"),
    "anvil-fusion-mcp": ("ok", "screened S12.6: `fusion-ask`, `fusion-result` registered; fake-provider calls match host, tools muted for `message`"),
    "anvil-fusion-longrun-mcp": ("ok", "screened S12.6: `longrun-start`, `-resume`, `-status` registered; quest store DB in the isolated cache"),
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

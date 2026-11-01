# anvil modules on the standalone (NeLisp) server — coverage census

Measured 2026-11-01 on anvil.el 531b5b1 (with the working tree's uncommitted
changes) and `nelisp` from `~/.local/bin/nelisp`.  Reproduce with:

```
tools/ai/standalone-coverage-census.sh OUT_DIR          # all anvil-*.el
tools/ai/standalone-coverage-table.py OUT_DIR/results.tsv docs/standalone-module-coverage.md
```

Method: for each module, a fresh process loads `tools/anvil-standalone-mcp.el`
with `ANVIL_STANDALONE_TEST_NO_SERVER=1`, then `(require 'MODULE)`, then calls
`MODULE-enable` if it exists and counts tools added to `anvil-server--tools`.
Databases, cwd and TMPDIR point at a scratch directory.

Limits: "tools register" means the tool specs were accepted, not that the
tools work.  Modules such as `anvil-buffer`, `anvil-eval`, `anvil-dashboard`
and `anvil-mu4e` register but act on host Emacs buffers/UI, so their calls are
expected to fail or be meaningless on standalone; each port must be verified by
a real MCP call.  Only the first blocker is recorded; fixing it may reveal more.

## Classes

- `ok`: already loaded by the standalone loader.
- `port`: loads and registers tools; only needs adding to the loader (then a
  real-call check).
- `lib`: loads, has no `-enable` entry point (helper library or tools
  registered by a parent module).
- `a`: missing NeLisp primitive / Emacs C-level function or substrate defect.
- `b`: missing Lisp library that could be vendored.
- `c`: inherently host- or environment-bound.
- `d`: anvil code bug.

Shared root causes: `(require 'org)` failing blocks anvil-file, and through it
anvil-org, anvil-text, anvil-data, anvil-elisp and anvil-workspace-sync.
Missing `run-with-timer`/`run-with-idle-timer` blocks the enable step of seven
modules.  Missing `tabulated-list` blocks the orchestrator family.

## Real-call screening of port candidates (2026-11-01)

Each candidate was loaded into a separate standalone server process (not the
shared daemon) and called through MCP `tools/call` over stdio.  Runtime:
`~/.local/bin/nelisp` -> `dev/nelisp/target/anvil/a14b1a2e4/nelisp`, a build
of a codex branch commit (2026-10-24) that is not an ancestor of NeLisp main.

| module | real call | blocker |
|---|---|---|
| `anvil-lint` | `lint-scanners`, `lint` (scoped) OK | - |
| `anvil-cron` | `cron-list`, `cron-status` OK | - |
| `anvil-git`, `anvil-git-msg` | not ported | (1) `process-coding-system-alist` unbound (anvil-host.el let-binds it); (2) `get-buffer-process` calls `process-list`, which standalone lacks (still true on the newest NeLisp branch 0e250a877, 2026-10-28); (3) with both shimmed, 9/10 calls return, but NeLisp `make-process` ignores `default-directory`, so results describe the server cwd's repo, not `path` (silent wrong answers); (4) `git-pr-body` needs `shell-quote-argument` |
| `anvil-sqlite` | fails | runtime `sqlite-select` rejects return type `set` |
| `anvil-ts`, `anvil-py`, `anvil-js` | fail | treesit subprocess backend `with-root` not implemented (Doc 38 Phase G TODO) |

anvil-lint and anvil-cron were added to the loader on 2026-11-01 (image tool count 40 -> 46; the sixth is `metrics-token-report`, pulled in by their requires).

Note: NeLisp origin/main (66561c85e) cannot run the standalone loader at all (`nl-ffi-memory` is missing there); the installed runtime's line is ~2200 commits ahead of origin/main.

Adding modules also requires updating the tool-count check (40) in
`tools/anvil-standalone-image.py` `smoke`.

<!-- TABLE -->

| module | loads? | tools register? | class | first blocker / note | effort |
|---|---|---|---|---|---|
| anvil-agent | yes | yes (1) | port | not in loader | S |
| anvil-autoresearch | no | - | b | tabulated-list.el missing | S |
| anvil-bench | no | - | b | profiler.el missing | S |
| anvil-bisect | yes | yes (3) | port | not in loader | S |
| anvil-browser | yes | enable fails | a | `run-with-idle-timer` not bound | S |
| anvil-buffer | yes | yes (3) | port | not in loader | S |
| anvil-cad | yes | yes (5) | port | not in loader | S |
| anvil-claude-watchdog | yes | enable fails | a | `run-with-timer` not bound | S |
| anvil-cli | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-clipboard | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-compact | yes | enable fails | a | `run-with-idle-timer` not bound | S |
| anvil-config | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-context-batch | yes | already loaded | ok | in standalone loader | - |
| anvil-context-dependencies | yes | already loaded | ok | in standalone loader | - |
| anvil-context | yes | already loaded | ok | in standalone loader | - |
| anvil-context-project | yes | already loaded | ok | in standalone loader | - |
| anvil-context-range | yes | yes (1) | port | not in loader | S |
| anvil-cron | yes | already loaded | ok | in standalone loader | - |
| anvil-crypto | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-daemon-guard | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-dashboard | yes | yes (3) | port | not in loader | S |
| anvil-data | no | - | a | `(require 'org)` fails on NeLisp (buffer object used as array) | L |
| anvil-defs-context | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-defs | yes | already loaded | ok | in standalone loader | - |
| anvil-dev | yes | yes (6) | port | not in loader | S |
| anvil-disclosure | yes | already loaded | ok | in standalone loader | - |
| anvil-discovery | yes | yes (2) | port | not in loader | S |
| anvil-disk | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-elisp | no | - | a | `(require 'org)` fails on NeLisp (buffer object used as array) | L |
| anvil-emacs | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-eval | yes | yes (6) | port | not in loader | S |
| anvil-extend | no | - | a | vendored filenotify.el `cl-defun` destructuring arglist | M |
| anvil-file | no | - | a | `(require 'org)` fails on NeLisp (buffer object used as array) | L |
| anvil-fs | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-ask | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-async | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-eval | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-exec | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-lens | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-longrun-async | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-longrun-mcp | yes | yes (3) | port | not in loader | S |
| anvil-fusion-longrun | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-longrun-store | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-mcp | yes | yes (2) | port | not in loader | S |
| anvil-fusion-panels | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-plan | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-route | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-traj | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-verify | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-git-msg | yes | yes (2) | port | not in loader | S |
| anvil-git | yes | yes (8) | port | not in loader | S |
| anvil-harness-telemetry | yes | yes (3) | port | not in loader | S |
| anvil-host | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-http | no | - | b | mailcap.el missing (url stack) | M |
| anvil-js | yes | yes (12) | port | not in loader | S |
| anvil-lint | yes | already loaded | ok | in standalone loader | - |
| anvil-manifest | yes | yes (4) | port | not in loader | S |
| anvil-memory-batch | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-memory-bridge | no | - | b | url-util.el missing (url stack) | M |
| anvil-memory-context | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-memory-obs | no | - | b | tabulated-list.el missing | S |
| anvil-memory | yes | already loaded | ok | in standalone loader | - |
| anvil-mu4e | yes | yes (5) | port | not in loader | S |
| anvil-nelisp-shims-demo | no | - | d | demo spec shape rejected by real anvil-server | S |
| anvil-nelisp-shims | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-net | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-nso-provider | no | - | b | nl-llm-agent-provider not on load-path | S |
| anvil-offload | yes | 0 tools | lib | `-enable` runs but registers no MCP tools | - |
| anvil-orchestrator | no | - | b | tabulated-list.el missing | S |
| anvil-orchestrator-openrouter | no | - | b | tabulated-list.el missing | S |
| anvil-orchestrator-presets | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-orchestrator-routing | yes | yes (1) | port | not in loader | S |
| anvil-orchestrator-workflow | no | - | b | tabulated-list.el missing | S |
| anvil-org | no | - | a | `(require 'org)` fails on NeLisp (buffer object used as array) | L |
| anvil-org-index | yes | enable fails | a | `run-with-idle-timer` not bound | S |
| anvil-org-watch | no | - | b | url-util.el missing (url stack) | M |
| anvil-pdf | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-proc | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-pty-broker | yes | enable fails | c | needs node-pty broker process | - |
| anvil-py | yes | yes (12) | port | not in loader | S |
| anvil-semantic | yes | yes (5) | port | not in loader | S |
| anvil-server-commands | yes | already loaded | ok | in standalone loader | - |
| anvil-server-ert | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-server-metrics | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-server | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-session-capture | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-session | yes | enable fails | a | `run-with-idle-timer` not bound | S |
| anvil-session-store | yes | yes (3) | port | not in loader | S |
| anvil-sexp-cst | yes | enable fails | c | needs tree-sitter C runtime | - |
| anvil-sexp | no | - | a | compile.el loads but `define-compilation-mode` is not bound | S |
| anvil-shell-filter-builtin | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-shell-filter | yes | enable fails | a | `run-with-idle-timer` not bound | S |
| anvil-sqlite | yes | yes (1) | port | not in loader | S |
| anvil-state | yes | enable fails | a | `run-with-idle-timer` not bound | S |
| anvil-text | no | - | a | `(require 'org)` fails on NeLisp (buffer object used as array) | L |
| anvil-treesit-backend | yes | 0 tools | lib | `-enable` runs but registers no MCP tools | - |
| anvil-ts | yes | yes (14) | port | not in loader | S |
| anvil-uri | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-web | no | - | b | mailcap.el missing (url stack) | M |
| anvil-wl | no | - | c | Wanderlust/mail stack | L |
| anvil-wl-filter | no | - | c | Wanderlust/mail stack | L |
| anvil-wl-graph | no | - | b | url-http.el missing (url stack + network) | L |
| anvil-wl-imap | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-wl-maildir | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-wl-oauth | no | - | b | url-http.el missing (url stack + network) | L |
| anvil-wl-smtp | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-wl-sync | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-wl-wanderlust | yes | 0 tools | lib | `-enable` runs but registers no MCP tools | - |
| anvil-worker | no | - | c | needs a running Emacs daemon | - |
| anvil-worklog | yes | already loaded | ok | in standalone loader | - |
| anvil-workspace-sync | no | - | a | `(require 'org)` fails on NeLisp (buffer object used as array) | L |
| anvil-xlsx | yes | no `-enable` | lib | library or registered by a parent | - |

# anvil modules on the standalone (NeLisp) server — coverage census

Measured 2026-11-02 on anvil.el 8a9202f (with other sessions' uncommitted
module edits in the working tree), `nelisp` from `~/.local/bin/nelisp`, and the
nelisp-emacs-lib checkout at `0d0536a2` (plus its uncommitted json/stdio/sqlite
edits).  First census: 2026-11-01 on anvil.el 531b5b1.  Reproduce with:

```
tools/ai/standalone-coverage-census.sh OUT_DIR          # all anvil-*.el
tools/ai/standalone-coverage-table.py OUT_DIR/results.tsv docs/standalone-module-coverage.md
```

Method: for each module, a fresh process loads `tools/anvil-standalone-mcp.el`
with `ANVIL_STANDALONE_TEST_NO_SERVER=1`, then `(require 'MODULE)`, then calls
`MODULE-enable` if it exists and counts tools added to `anvil-server--tools`.
Databases, cwd, TMPDIR, HOME and the XDG directories point at a scratch
directory, so modules cannot open the real `~/.emacs.d` databases.

Limits: "tools register" means the tool specs were accepted, not that the
tools work.  Modules such as `anvil-buffer`, `anvil-eval`, `anvil-dashboard`
and `anvil-mu4e` register but act on host Emacs buffers/UI, so their calls are
expected to fail or be meaningless on standalone; each port must be verified by
a real MCP call.  Only the first blocker is recorded; fixing it may reveal more.
Rows marked "screened" take their class from the real-call screenings below
(2026-11-01 and 2026-11-02), not from the census.

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

Shared root causes: missing `tabulated-list` blocks the orchestrator family;
the url/mailcap stack blocks the web and mail modules; compile.el without
`define-compilation-mode` blocks anvil-elisp and anvil-sexp.

Fixed on 2026-11-02 (were the largest blockers on 2026-11-01):
- `(require 'org)` failed (`wrong-type-argument arrayp`): nelisp-emacs-lib
  swapped its buffer structs for alist objects on standalone while host-built
  .elc inline the record accessors.  Fixed on nelisp-emacs-lib branch
  `fix/standalone-struct-records-20261102` (records everywhere; vendored Org
  parser load made best-effort).  anvil-file, anvil-data and anvil-text now
  load; anvil-org next needs org-id.el.
- `run-with-timer` / `run-with-idle-timer` were unbound.  The loader now
  supplies them on top of `run-at-time`; NeLisp branch
  `feat/run-with-timer-20261102` adds them natively.

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
| `anvil-file` | `file-read`, `file-outline`, `file-create`, `file-replace-string` OK | ported 2026-11-02 |
| `anvil-compact`, `anvil-session`, `anvil-shell-filter` | `compact-estimate`, `compact-stats`, `session-list`, `shell-filter`, `shell-gain` OK | ported 2026-11-02 (state DB under the XDG cache, not the host's) |
| `anvil-browser` | fails | `browser-fetch`: `process-coding-system-alist` unbound on this runtime |
| `anvil-org-index` | not called | uses the host's `~/.emacs.d` DB, calls `message` (stdout is the MCP channel), needs `anvil-org-index-paths` |
| `anvil-claude-watchdog` | not called | 30 s timer watching the host's Claude processes; would duplicate host events |

### Second screening (2026-11-02): the remaining 18 port candidates

Same method, on a `git archive` of 56bb67f with per-module HOME/XDG/TMPDIR
and databases under a scratch directory (the real `~/.emacs.d` was checked
untouched).  Calls were limited to read-only tools or writes inside the scratch
directory; tools that call an LLM or remote API (`fusion-ask`,
`longrun-start`/`-resume`, `semantic-embed-index`), spawn Emacs
(`bisect-test`, `test-run-all`) or scaffold modules were not called.

| module | real call | blocker |
|---|---|---|
| `anvil-agent` | `agent-spec-loader` OK | - |
| `anvil-data` | `list-keys`, `get-path`, `set-path` (preview + apply, re-read shows the new value) OK | - |
| `anvil-dev` | `self-sync-check`, `claude-limits-analyze`, `codex-efficiency-check`, `release-audit` OK | `self-sync-check` reports nil git info |
| `anvil-discovery` | `tools-by-intent`, `tools-usage-report` OK | - |
| `anvil-harness-telemetry` | `record`, then `stats` (1 entry), `recent` OK | - |
| `anvil-session-store` | `recent`, `search-ranked`, `snapshot-ref` OK | empty DB only; content not verified |
| `anvil-bisect` | `bisect-last-result` OK, `bisect-cancel` fails | (d) `bisect-cancel` returns a symbol the server rejects |
| `anvil-cad` | `cad-read-outline`, `cad-extract` OK; `cad-generate`, `cad-annotate` fail | (d) compares the `write-region` result with `t`; the file is written, then "cannot write" is signalled |
| `anvil-manifest` | `manifest-cost` OK only | (d) registers on server-id `"default"`, so the loader would not serve it; `attention`/`recovery-suggest` hit `(wrong-type-argument symbolp 'dev)` (doubly quoted spec, not compared with GNU Emacs); `docstring-candidates` needs tabulated-list |
| `anvil-orchestrator-routing` | `orchestrator-routing-select` fails | (d) `anvil-orchestrator-stats` void: missing require; preloading anvil-orchestrator then needs tabulated-list |
| `anvil-buffer` | `buffer-list-modified`, `buffer-read` fail | (a) after the module loads, `buffer-live-p` returns nil and `buffer-file-name` rejects its argument; the same calls work without the module |
| `anvil-eval` | all three fail | (a) `with-timeout` void; with a shim `emacs-eval`/`emacs-eval-jobs` work and `nelisp-eval` needs `NELISP_ELISP_DIR`; registers on server-id `"anvil"` |
| `anvil-semantic` | `status`, `notes-lexical-search`, `semantic-search` (lexical) return; `reindex` fails | (a) `directory-files-recursively` void; the searches ran on an empty index, so their `[]` proves nothing |
| `anvil-fusion-mcp` | `fusion-result` fails | (b) tabulated-list.el missing |
| `anvil-fusion-longrun-mcp` | `longrun-status` fails | (b) tabulated-list.el missing |
| `anvil-mu4e` | not called | (c) touches mail |
| `anvil-context-range`, `anvil-dashboard` | not called | untracked in the working tree, absent from the archive |

Shared gaps found: tabulated-list.el (fusion, fusion-longrun, manifest,
orchestrator-routing), `with-timeout` (eval), `directory-files-recursively`
(semantic).  The six modules that passed (agent, data, dev, discovery,
harness-telemetry, session-store) were added to the loader the same day.

Loader defect found by this screening: `tools/anvil-standalone-mcp.el` at
56bb67f requires `anvil-context-batch`, `anvil-context-dependencies` and
`anvil-context-project`, which are untracked working files, so the server does
not start from a clean checkout of the branch (the screening stubbed them).  Fixed 2026-11-03: those three requires are optional and their tools
register only when the feature loaded; a clean checkout serves 94 tools.

anvil-lint and anvil-cron were added to the loader on 2026-11-01 (image tool count 40 -> 46; the sixth is `metrics-token-report`, pulled in by their requires).  anvil-file followed on 2026-11-02 (46 -> 61) and anvil-compact, anvil-session and anvil-shell-filter after it (61 -> 79), and agent, data, dev, discovery, harness-telemetry and session-store after the second screening (79 -> 98; their databases go to the XDG cache).

Note: NeLisp origin/main (66561c85e) cannot run the standalone loader at all (`nl-ffi-memory` is missing there); the installed runtime's line is ~2200 commits ahead of origin/main.

Adding modules also requires updating the tool-count check (a lower bound: 94 plus one per optional context module that loaded; 98 in a full working tree) in
`tools/anvil-standalone-image.py` `smoke`.

<!-- TABLE -->

| module | loads? | tools register? | class | first blocker / note | effort |
|---|---|---|---|---|---|
| anvil-agent | yes | already loaded | ok | in standalone loader | - |
| anvil-autoresearch | no | - | b | tabulated-list.el missing | S |
| anvil-bench | no | - | b | profiler.el missing | S |
| anvil-bisect | yes | yes (3) | d | screened: `bisect-cancel` returns a symbol the server rejects | - |
| anvil-browser | yes | yes (5) | a | registers; `browser-fetch` fails (process coding support) | - |
| anvil-buffer | yes | yes (3) | a | screened: buffer primitives misbehave after the module loads | - |
| anvil-cad | yes | yes (5) | d | screened: write tools compare `write-region` result with `t` | - |
| anvil-claude-watchdog | yes | yes (1) | c | registers; watches the host's Claude processes | - |
| anvil-cli | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-clipboard | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-compact | yes | already loaded | ok | in standalone loader | - |
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
| anvil-data | yes | already loaded | ok | in standalone loader | - |
| anvil-defs-context | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-defs | yes | already loaded | ok | in standalone loader | - |
| anvil-dev | yes | already loaded | ok | in standalone loader | - |
| anvil-disclosure | yes | already loaded | ok | in standalone loader | - |
| anvil-discovery | yes | already loaded | ok | in standalone loader | - |
| anvil-disk | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-elisp | no | - | a | compile.el loads but `define-compilation-mode` is not bound | S |
| anvil-emacs | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-eval | yes | yes (6) | a | screened: `with-timeout` void; registers on server-id "anvil" | - |
| anvil-extend | no | - | a | vendored filenotify.el `cl-defun` destructuring arglist | M |
| anvil-file | yes | already loaded | ok | in standalone loader | - |
| anvil-fs | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-ask | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-async | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-eval | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-exec | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-lens | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-longrun-async | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-longrun-mcp | yes | yes (3) | b | screened: tabulated-list.el missing | - |
| anvil-fusion-longrun | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-longrun-store | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-mcp | yes | yes (2) | b | screened: tabulated-list.el missing | - |
| anvil-fusion-panels | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-plan | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-route | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-traj | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-fusion-verify | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-git-msg | yes | yes (2) | a | registers; real calls blocked (see screening) | - |
| anvil-git | yes | yes (8) | a | registers; real calls blocked (see screening) | - |
| anvil-harness-telemetry | yes | already loaded | ok | in standalone loader | - |
| anvil-host | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-http | no | - | b | mailcap.el missing (url stack) | M |
| anvil-js | yes | yes (12) | a | registers; treesit subprocess `with-root` not implemented | - |
| anvil-lint | yes | already loaded | ok | in standalone loader | - |
| anvil-manifest | yes | yes (4) | d | screened: registers on server-id "default"; quoted spec symbols | - |
| anvil-memory-batch | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-memory-bridge | no | - | b | url-util.el missing (url stack) | M |
| anvil-memory-context | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-memory-obs | no | - | b | tabulated-list.el missing | S |
| anvil-memory | yes | already loaded | ok | in standalone loader | - |
| anvil-mu4e | yes | yes (5) | c | screened: not called (touches mail) | - |
| anvil-nelisp-shims-demo | no | - | d | demo spec shape rejected by real anvil-server | S |
| anvil-nelisp-shims | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-net | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-nso-provider | no | - | b | nl-llm-agent-provider not on load-path | S |
| anvil-offload | yes | 0 tools | lib | `-enable` runs but registers no MCP tools | - |
| anvil-orchestrator | no | - | b | tabulated-list.el missing | S |
| anvil-orchestrator-openrouter | no | - | b | tabulated-list.el missing | S |
| anvil-orchestrator-presets | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-orchestrator-routing | yes | yes (1) | d | screened: `anvil-orchestrator-stats` void (missing require) | - |
| anvil-orchestrator-workflow | no | - | b | tabulated-list.el missing | S |
| anvil-org | no | - | b | org-id.el missing (only the lightweight Org subset loads) | M |
| anvil-org-index | yes | yes (2) | c | registers; uses the host DB and `message`, needs paths | - |
| anvil-org-watch | no | - | b | url-util.el missing (url stack) | M |
| anvil-pdf | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-proc | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-pty-broker | yes | enable fails | c | needs the node-pty broker process | - |
| anvil-py | yes | yes (12) | a | registers; treesit subprocess `with-root` not implemented | - |
| anvil-semantic | yes | yes (5) | a | screened: reindex needs `directory-files-recursively` | - |
| anvil-server-commands | yes | already loaded | ok | in standalone loader | - |
| anvil-server-ert | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-server-metrics | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-server | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-session-capture | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-session | yes | already loaded | ok | in standalone loader | - |
| anvil-session-store | yes | already loaded | ok | in standalone loader | - |
| anvil-sexp-cst | yes | enable fails | c | needs tree-sitter C runtime | - |
| anvil-sexp | no | - | a | compile.el loads but `define-compilation-mode` is not bound | S |
| anvil-shell-filter-builtin | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-shell-filter | yes | already loaded | ok | in standalone loader | - |
| anvil-sqlite | yes | yes (1) | a | registers; `sqlite-select` lacks return type `set` | - |
| anvil-state | yes | already loaded | ok | in standalone loader | - |
| anvil-text | yes | no `-enable` | lib | library or registered by a parent | - |
| anvil-treesit-backend | yes | 0 tools | lib | `-enable` runs but registers no MCP tools | - |
| anvil-ts | yes | yes (14) | a | registers; treesit subprocess `with-root` not implemented | - |
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
| anvil-workspace-sync | no | - | a | `coding-system-p` not bound | S |
| anvil-xlsx | yes | no `-enable` | lib | library or registered by a parent | - |

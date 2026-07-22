# Anvil Root Resilience Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (- [ ]) syntax for tracking.

**Goal:** Correct lazy-worker status reporting, fail safely on oversized inline results, attribute every dedicated-root watchdog kill, and deploy the verified result on Hera.

**Architecture:** Upstream Anvil owns backend-independent worker state and response-size safety. The Nix deployment owns per-agent watchdog activity records, supervisor validation, probe enrichment, packaging, and Hera activation. Large results fail before expensive response encoding; diagnostics carry only bounded enums, registered tool identifiers, and timing metadata.

**Tech Stack:** Emacs Lisp with ERT, Python 3 standard library with unittest, Nix, nix-darwin, Git, Anvil MCP.

## Global Constraints

- Worker states are exactly cold, alive, busy, unresponsive, and dead.
- Probing is read-only, nonblocking, never spawns a worker, and never calls the full liveness probe.
- anvil-file-max-unbounded-read-bytes defaults to 1,048,576 bytes.
- A large file read requires a positive limit; offset alone does not make a read bounded.
- anvil-server-max-inline-result-bytes defaults to 2,097,152 UTF-8 bytes.
- Rejected file contents and rejected tool results never appear in errors or telemetry.
- Watchdog telemetry never records arguments, paths, expressions, request IDs, raw JSON, results, output, or caller-derived environment values; the generated non-secret run ID is the sole environment-carried protocol value.
- The synchronization lease and activity channel remain separate; activity updates never change dispatch generation.
- Watchdog schema version is exactly 1; activity messages/records are at most 1,024 UTF-8 bytes and events at most 4,096 bytes.
- Root Emacs receives a one-shot Unix socket connection, never a writable activity-record descriptor; the monitor owns both fixed record inodes.
- Diagnostic opens require O_NOFOLLOW on Darwin/Linux and fail configuration if it is unavailable.
- restart_reason remains daemon-exited:CODE; last_watchdog is additive and accepted only for the exact matching daemon PID and 32-hex-character launch run ID.
- Existing 45-second heartbeat and 225-second dispatch deadlines remain unchanged.
- ai-nix is read-only for this task.
- Preserve unrelated edits in /Users/johnw/src/nix/config/packages.nix and /Users/johnw/src/nix/docs/PI-AGENT-WIGGUM-PLAN.md.
- Every behavior change follows a witnessed red-green cycle.
- Every work commit receives an independent fess audit; final completion also requires whole-branch review, full gates, an Anvil rebase before the definitive pin, a Nix rebase before its push, Hera switch, and production smoke.

---

### Task 0: Create the isolated Anvil worktree and prove the baseline

**Files:**
- No tracked file changes.

**Interfaces:**
- Produces: /Users/johnw/src/emacs-lisp/anvil-root-resilience on branch fix/anvil-root-resilience.
- Base: the fetched fork/fix/issue-53-interrupted-hangs remote-tracking ref at this audited planning commit.

- [ ] **Step 1: Verify the published planning base**

From /Users/johnw/src/emacs-lisp/anvil.el run:

    git fetch fork
    local_base=$(git rev-parse fix/issue-53-interrupted-hangs)
    remote_base=$(git rev-parse fork/fix/issue-53-interrupted-hangs)
    test "$local_base" = "$remote_base"
    test "$remote_base" = "$(git rev-parse HEAD)"

The three revisions must match. If the remote ref is not this audited planning
commit, push the already-audited parent branch and repeat the check before
creating any child worktree.

- [ ] **Step 2: Detect worktree state and create the child branch**

Confirm neither the target path nor branch already exists. Then run:

    git worktree add /Users/johnw/src/emacs-lisp/anvil-root-resilience \
      -b fix/anvil-root-resilience fork/fix/issue-53-interrupted-hangs

Do not rewrite fix/issue-53-interrupted-hangs. All later Anvil source and test work runs in the new worktree.

- [ ] **Step 3: Verify the clean baseline**

From /Users/johnw/src/emacs-lisp/anvil-root-resilience run:

    make test
    make test-all
    make lint
    make byte-compile

Expected: every command exits zero. A failing baseline triggers root-cause investigation before implementation.

- [ ] **Step 4: Initialize durable progress**

Create the ignored .superpowers/sdd/progress.md ledger and record Task 0 complete with the exact baseline command results. The tracked handoff path update belongs to the Task 1 commit, where the handoff is explicitly listed.

---

### Task 1: Truthful nonblocking worker states

**Files:**
- Modify: anvil-worker.el
- Modify: tests/anvil-worker-test.el
- Modify: openspec/specs/worker-pool-spawn-deferred/spec.md
- Modify: docs/superpowers/handoffs/2026-07-22-anvil-root-resilience.md

**Interfaces:**
- Produces: anvil-worker--reported-state WORKER ENDPOINT-ALIVE returning one of cold, alive, busy, unresponsive, or dead.
- Consumes: worker plist keys :busy, :hung-checks, :demanded, :last-state, :server-file.
- Preserves: anvil-worker--quick-alive-p as the only endpoint check used by reporting.

- [ ] **Step 1: Add three exact failing worker tests**

Create `anvil-worker-test-reported-state-precedence` with this table:

    (dolist (case
             '(((:demanded nil) nil cold)
               ((:demanded t) nil dead)
               ((:demanded nil) t alive)
               ((:demanded t :hung-checks 1) t unresponsive)
               ((:demanded t :hung-checks 1 :busy t) t busy)))
      (pcase-let ((`(,worker ,endpoint ,expected) case))
        (should (eq expected
                    (anvil-worker--reported-state worker endpoint)))))

Create `anvil-worker-test-status-nonblocking` around the real
`anvil-worker-status`. Install a pool with at least one worker in each
relevant state, count `anvil-worker--quick-alive-p` calls by worker, and make
`anvil-worker-spawn` and `anvil-worker--worker-alive-p` fail immediately if
called. Assert the quick check is called exactly once per worker and the
rendered status contains the exact `state=`, `demanded=`, and `last=`
fields.

Create `anvil-worker-test-probe-rendering` around the real MCP probe. Cover a
cold never-demanded worker, an unresponsive reachable worker with
`probe-failures=1/LIMIT`, and a reachable busy worker that also has stale
probe failures. Assert cold rows are not dead, `last=alive`, `last=dead`,
and `last=unknown` render as required, busy wins over unresponsive, and the quick check runs exactly once per worker.

- [ ] **Step 2: Run the focused worker tests and witness RED**

Run this single-quoted, non-vacuous selector:

    emacs --batch -Q -L . -L tests -l ert -l tests/anvil-worker-test.el \
      --eval '(let* ((selector "^anvil-worker-test-\\(reported-state-precedence\\|status-nonblocking\\|probe-rendering\\)$")
                     (selected (ert-select-tests selector t)))
                (unless (= 3 (length selected))
                  (error "expected 3 focused worker tests, selected %d"
                         (length selected)))
                (ert-run-tests-batch-and-exit selector))'

Expected failure: `anvil-worker--reported-state` is absent or an existing
status/probe path performs the full liveness check or renders the wrong state.

- [ ] **Step 3: Implement the minimal classifier and formatter**

Add near the existing pool status functions:

    (defun anvil-worker--reported-state (worker endpoint-alive)
      "Return WORKER's nonblocking externally reported state."
      (cond
       ((and endpoint-alive (plist-get worker :busy)) 'busy)
       ((and endpoint-alive
             (> (or (plist-get worker :hung-checks) 0) 0))
        'unresponsive)
       (endpoint-alive 'alive)
       ((not (plist-get worker :demanded)) 'cold)
       (t 'dead)))

Add a formatter shared by `anvil-worker-status` and
`anvil-worker--tool-probe`. It emits state, demanded=yes or no, last=alive,
dead, or unknown, conditional PID, and conditional
probe-failures=N/LIMIT. Both callers compute endpoint-alive exactly once with
`anvil-worker--quick-alive-p` and pass it to the helper.

Update the registered probe description to name all five states. Do not change
health, spawn, ownership, or recovery functions.

- [ ] **Step 4: Update the worker-pool observability requirement and handoff**

Add an acceptance scenario to worker-pool-spawn-deferred/spec.md:

- never-demanded plus absent endpoint reports cold;
- demanded plus absent endpoint reports dead;
- reachable endpoint reports alive, busy, or unresponsive;
- status and probe reporting perform no spawn or full liveness probe.

Update the handoff current repository/branch to the isolated worktree and record
the Task 0 baseline evidence.

- [ ] **Step 5: Verify GREEN and the full worker suites**

Run the focused command from Step 2, then:

    emacs --batch -Q -L . -L tests \
      -l ert -l tests/anvil-worker-test.el \
      -f ert-run-tests-batch-and-exit

    emacs --batch -Q -L . -L tests \
      -l ert -l tests/anvil-worker-pool-test.el \
      -f ert-run-tests-batch-and-exit

Expected: all selected tests pass with zero unexpected results.

- [ ] **Step 6: Commit and independently audit**

Stage only the four listed task files and commit:

    git commit -S -m "Report lazy Anvil workers as cold"

Dispatch a read-only fess audit for the commit using the frozen spec and this
task. Verify and fix every real finding before Task 2.

---

### Task 2: Fail oversized unbounded file reads before body loading

**Files:**
- Modify: anvil-file.el
- Modify: tests/anvil-file-test.el

**Interfaces:**
- Produces: anvil-file-max-unbounded-read-bytes, default 1,048,576.
- Produces: anvil-file--guard-unbounded-read ABS LIMIT.
- Consumes: file-attribute-size without reading file contents.
- Defines a bounded request as one with a numeric LIMIT greater than zero.

- [ ] **Step 1: Add two exact failing read-boundary tests**

Create `anvil-file-test-unbounded-read-guard`. Make an oversized temporary
file while dynamically binding the threshold to 16 bytes. Around
`anvil-file-read`, stub `anvil--insert-file` so any body load fails the
test. Assert that all of these signal `user-error` with the exact file size,
configured maximum, `offset=0`, and `limit=200` fields, and that the unique
file contents are absent from the message:

    (anvil-file-read path)
    (anvil-file-read path 50 nil)
    (anvil-file-read path 50 0)

Create `anvil-file-test-read-limit-boundaries`. Prove a small unbounded read
is unchanged, a large read with positive limit 1 returns one line, and nil,
zero, or negative `anvil-file-max-unbounded-read-bytes` restores legacy
behavior. Count body-loader calls so rejected reads prove the guard runs first.

- [ ] **Step 2: Run focused file tests and witness RED**

    emacs --batch -Q -L . -L tests -l ert -l tests/anvil-file-test.el \
      --eval '(let* ((selector "^anvil-file-test-\\(unbounded-read-guard\\|read-limit-boundaries\\)$")
                     (selected (ert-select-tests selector t)))
                (unless (= 2 (length selected))
                  (error "expected 2 focused file tests, selected %d"
                         (length selected)))
                (ert-run-tests-batch-and-exit selector))'

Expected failure: oversized calls reach `anvil--insert-file` or do not signal
the exact bounded `user-error`.

- [ ] **Step 3: Implement the early guard**

Add:

    (defcustom anvil-file-max-unbounded-read-bytes (* 1024 1024)
      "Maximum regular-file size accepted without a positive LIMIT.
    Nil or a non-positive value disables the guard."
      :type '(choice (const :tag "Disabled" nil) integer)
      :group 'anvil-file)

    (defun anvil-file--guard-unbounded-read (abs limit)
      "Reject an oversized read of ABS unless LIMIT is positive."
      (let ((cap anvil-file-max-unbounded-read-bytes))
        (when (and (numberp cap) (> cap 0)
                   (not (and (numberp limit) (> limit 0))))
          (let* ((attrs (file-attributes abs 'integer))
                 (size (and attrs
                            (null (file-attribute-type attrs))
                            (file-attribute-size attrs))))
            (when (and size (> size cap))
              (user-error
               (concat "file-read refused unbounded %d-byte file "
                       "(maximum %d); retry with offset=0 and limit=200, "
                       "then advance offset")
               size cap))))))

Call this helper after `anvil--prepare-path` and before
`anvil-file-warn-if-diverged` or `anvil--insert-file`. Offset is
intentionally not part of the decision.

Update the tool docstring and registered description so clients know that large
files require a positive limit.

- [ ] **Step 4: Verify GREEN and all file tests**

Run the focused command, then:

    emacs --batch -Q -L . -L tests \
      -l ert -l tests/anvil-file-test.el \
      -f ert-run-tests-batch-and-exit

Expected: all tests pass with zero unexpected results.

- [ ] **Step 5: Commit and independently audit**

Commit:

    git commit -S -m "Bound unpaginated Anvil file reads"

Run the per-commit fess audit and resolve every verified finding.

---

### Task 3: Stop oversized inline results before JSON-RPC encoding

**Files:**
- Modify: anvil-server.el
- Modify: tests/anvil-test.el
- Modify at closeout: docs/superpowers/handoffs/2026-07-22-anvil-root-resilience.md

**Interfaces:**
- Produces: anvil-server-max-inline-result-bytes, default 2,097,152.
- Produces: anvil-server--enforce-inline-result-limit TOOL-NAME RESULT-TEXT returning RESULT-TEXT or signaling anvil-server-tool-error.
- Integration point: immediately after result-text construction and before disclosure, metrics, MCP wrapping, or anvil-server--respond-with-result.

- [ ] **Step 1: Add two exact failing server tests**

In the existing aggregate `tests/anvil-test.el`, create
`anvil-test-inline-result-limit-end-to-end`. Register a temporary tool
returning a unique payload longer than a dynamically bound 32-byte cap. Send a
real `tools/call` JSON-RPC request through
`anvil-server-process-jsonrpc`. Assert:

- the response is valid JSON-RPC;
- the tool result has `isError` true;
- the error names only the registered tool, observed UTF-8 byte count, limit,
  and bounded-interface guidance;
- the unique payload is absent;
- disclosure and metrics hooks do not receive the rejected payload.

Create `anvil-test-inline-result-limit-boundaries`: exactly 32 UTF-8 bytes
succeeds, 33 fails, a multibyte string is measured after UTF-8 encoding, and
nil, zero, or negative configuration disables the guard. Always unregister the
fixture tool in `unwind-protect`.

- [ ] **Step 2: Run focused server tests and witness RED**

    emacs --batch -Q -L . -L tests -l ert -l tests/anvil-test.el \
      --eval '(let* ((selector "^anvil-test-inline-result-limit-\\(end-to-end\\|boundaries\\)$")
                     (selected (ert-select-tests selector t)))
                (unless (= 2 (length selected))
                  (error "expected 2 focused server tests, selected %d"
                         (length selected)))
                (ert-run-tests-batch-and-exit selector))'

Expected failure: the oversized payload is returned or the guard function is
absent.

- [ ] **Step 3: Implement the result guard**

Add near server dispatch configuration:

    (defcustom anvil-server-max-inline-result-bytes (* 2 1024 1024)
      "Maximum UTF-8 bytes returned inline by one tool.
    Nil or a non-positive value disables the guard."
      :type '(choice (const :tag "Disabled" nil) integer)
      :group 'anvil-server)

    (defun anvil-server--enforce-inline-result-limit
        (tool-name result-text)
      "Return RESULT-TEXT when it fits, otherwise signal a bounded error."
      (let ((cap anvil-server-max-inline-result-bytes))
        (if (or (not (numberp cap)) (<= cap 0))
            result-text
          (let ((bytes
                 (string-bytes
                  (encode-coding-string result-text 'utf-8 t))))
            (when (> bytes cap)
              (signal
               'anvil-server-tool-error
               (list
                (format
                 (concat "Tool %s produced %d UTF-8 bytes, exceeding "
                         "the inline limit of %d; use a paginated, "
                         "filtered, tee, or asynchronous interface")
                 tool-name bytes cap))))
            result-text))))

Apply it as a separate `let*` binding before
`anvil-disclosure-budget-apply`, metrics payload recording, MCP wrapping, and
`anvil-server--respond-with-result`. Never interpolate `result-text` into
the error.

- [ ] **Step 4: Verify GREEN and full server tests**

Run the focused command, then:

    emacs --batch -Q -L . -L tests \
      -l ert -l tests/anvil-test.el \
      -f ert-run-tests-batch-and-exit

Expected: all tests pass with zero unexpected results.

- [ ] **Step 5: Run all upstream Anvil gates**

    make test
    make test-all
    make lint
    make byte-compile

Expected: every command exits zero with no test failures or compile errors.

- [ ] **Step 6: Commit and independently audit the server guard**

Commit only `anvil-server.el` and `tests/anvil-test.el`:

    git commit -S -m "Reject oversized inline Anvil results"

Run the per-commit fess audit and resolve every verified finding.

- [ ] **Step 7: Finalize Anvil before the Nix pin**

Drain partner observations, run an independent whole-branch review against the
frozen design and plan, fix all Critical and Important findings, rerun all four
gates, and perform a final fess audit of the last work commit.

Update and commit the tracked handoff with the Anvil gate/review evidence. Then
establish the definitive published history exactly once:

    git fetch fork
    git rebase fork/fix/issue-53-interrupted-hangs
    make test
    make test-all
    make lint
    make byte-compile
    git push -u fork fix/anvil-root-resilience
    git status --short --branch

If the branch was already published before this final rebase, use
`git push --force-with-lease` only after verifying the remote still names the
pre-rebase tip. No Anvil history rewrite is allowed after Task 4 computes the
published archive hash.

Record the definitive `git rev-parse HEAD` in the ignored progress ledger for
Task 4.

---

### Task 4: Pin the definitive Anvil revision in an isolated Nix worktree

**Files:**
- Modify: packages/anvil-mcp/source.nix

**Interfaces:**
- Consumes: the published, final Anvil commit from Task 3.
- Produces: matching date, hash, rev, and version metadata in source.nix.
- Provenance: committer ISO timestamp from `%cI`; version from the single
  `;; Version:` header in that revision's `anvil.el`.

- [ ] **Step 1: Create an isolated Nix worktree**

From /Users/johnw/src/nix, inspect existing worktrees and branches first.
Because main contains unrelated changes, create a sibling linked worktree on
branch fix/anvil-root-resilience based on the current committed local main.
Do not touch the dirty checkout.

    git fetch origin
    nix_base=$(git rev-parse main)
    git worktree add /Users/johnw/src/nix-anvil-root-resilience \
      -b fix/anvil-root-resilience "$nix_base"

Confirm the new worktree is clean and its HEAD equals `$nix_base`. Run every
Nix command through `direnv exec .`.

- [ ] **Step 2: Compute all source metadata from the published commit**

    anvil_repo=/Users/johnw/src/emacs-lisp/anvil-root-resilience
    anvil_rev=$(git -C "$anvil_repo" rev-parse HEAD)
    test "$anvil_rev" = "$(git -C "$anvil_repo" rev-parse fork/fix/anvil-root-resilience)"
    anvil_date=$(git -C "$anvil_repo" show -s --format=%cI "$anvil_rev")
    version_lines=$(git -C "$anvil_repo" show "$anvil_rev:anvil.el" |
      sed -n 's/^;; Version:[[:space:]]*//p')
    test "$(printf '%s\n' "$version_lines" | sed '/^$/d' | wc -l | tr -d ' ')" = 1
    anvil_version=$version_lines
    base32_hash=$(nix-prefetch-url --unpack \
      "https://github.com/jwiegley/anvil.el/archive/${anvil_rev}.tar.gz")
    sri_hash=$(nix hash convert --hash-algo sha256 --to sri "$base32_hash")
    printf 'date=%s\nrev=%s\nversion=%s\nhash=%s\n' \
      "$anvil_date" "$anvil_rev" "$anvil_version" "$sri_hash"

Use the four printed values exactly. Do not bump the package version merely
because the revision changed.

- [ ] **Step 3: Update source.nix and verify the Darwin package**

Replace only `date`, `hash`, `rev`, and `version`; keep owner and repo
unchanged.

    direnv exec . nix eval --raw \
      .#packages.aarch64-darwin.anvil-mcp-dedicated.currentAnvilRev

The output must equal `$anvil_rev`. Then run:

    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L

Linux `anvil-mcp-headless` checks are optional here and may be run only when
a Linux builder is explicitly available; they are not Darwin attributes.

- [ ] **Step 4: Commit and independently audit the pin**

Commit only source.nix:

    git commit -S -m "Pin Anvil root resilience changes"

Run an independent fess audit against the pin commit before watchdog work.

---

### Task 5: Persist exact sanitized watchdog causes

**Files:**
- Modify: packages/anvil-mcp/default.nix
- Modify: packages/anvil-mcp/watchdog-test.py
- Modify: packages/anvil-mcp/timeout-ordering-test.py when generated constant coverage requires it
- Modify: packages/anvil-mcp/agent-supervisor.py
- Modify: packages/anvil-mcp/agent-supervisor-test.py for launch-run-id coverage

**Interfaces:**
- Produces in generated Python: validate_activity, select_deadline_cause, and write_watchdog_event.
- Produces private entries: .anvil-root-activity.sock,
  .anvil-root-activity.json, and .anvil-root-watchdog.json.
- Uses schema version 1, a 1,024-byte activity-message/record ceiling, and a
  4,096-byte event ceiling exactly as frozen in the design.
- Makes `O_NOFOLLOW` mandatory; absence is a configuration error.
- Preserves timeoutPolicy values and lease generation semantics.

- [ ] **Step 1: Add failing watchdog protocol and cause tests**

Use the launcher definitions extracted by the existing test harness, not a
parallel hand-written implementation. Add deterministic tests for:

- all seven cause enums;
- heartbeat/dispatch simultaneous expiry choosing the earlier absolute
  deadline;
- exact activity and event key sets, enum sets, field types, and millisecond
  units;
- 1,024-byte activity and 4,096-byte event ceilings;
- startup activity and strictly increasing sequence validation;
- 32-character lowercase-hex run identifiers and matching daemon PIDs;
- unknown method mapping to `other` and registered-only, 128-byte tool names;
- mandatory `O_NOFOLLOW`, regular files, owner UID, mode 0600, and stable
  activity/event inodes;
- one root socket connection, socket unlink after acceptance, and continued
  writes to the original activity inode after socket-path replacement;
- no writable activity-record descriptor visible to a root subprocess;
- a diagnostic write failure still invoking the kill path;
- a unique secret sentinel absent from serialized activity/event records and
  diagnostics.

Make the generated `write_watchdog_event` available as the fixture writer used
by Task 6 supervisor tests; a valid supervisor fixture must never be assembled
independently.

- [ ] **Step 2: Build the Darwin check and witness RED**

    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L

Expected failure is in the watchdog/run-id tests because the protocol helpers
and monitor-owned records do not yet exist.

- [ ] **Step 3: Implement the monitor-owned activity channel**

In `dedicatedLockLauncher`:

- fail configuration immediately if `O_NOFOLLOW` or required Unix-domain
  socket support is unavailable;
- create the activity and event files under the canonical mode-0700 runtime
  directory with `O_CREAT|O_EXCL|O_NOFOLLOW`, mode 0600;
- validate regular-file type, owner UID, mode, link count, device, and inode;
- create a private mode-0600 Unix stream endpoint and give its pathname, not a
  record descriptor, to root Emacs;
- keep activity/event descriptors and the accepted connection private to the
  monitor; close them in the execing parent;
- accept exactly one root connection, validate schema/run-id/PID/sequence on
  each newline-delimited message, unlink the socket after acceptance, and
  rewrite/fsync the fixed activity inode only for a valid message;
- retain at most 1,024 bytes of pending input and close a peer that exceeds the
  bound or sends malformed frames;
- serialize only the exact version-1 fields frozen in the design.

In generated Emacs Lisp, connect once with `make-network-process`, mark the
process no-query, and send compact UTF-8 activity records at startup, parse,
dispatch, tool-call, result-encode, response-write, and idle. Map methods
through the fixed enum and emit a tool only after exact lookup in the active
registered-tool table. Never send params, arguments, request IDs, paths,
expressions, results, raw JSON, or caller-derived environment values. The generated run ID is the sole environment-carried protocol value. Scrub the socket path
and run identifier from worker/offload subprocess environments.

In `agent-supervisor.py`, generate `secrets.token_hex(16)` before each
daemon launch, pass it as `ANVIL_EMACS_WATCHDOG_RUN_ID`, and retain it on the
process object for Task 6. Add a unit test proving a fresh exact-format value is
passed and retained.

- [ ] **Step 4: Implement exact event attribution**

Refactor `kill_parent_if` to accept a cause/event factory that runs only
after the verifier confirms the failure still exists. Write the exact
version-1 event through the pre-opened event descriptor immediately before
SIGKILL. Truncate/rewind first and fsync best-effort, but never let telemetry
failure suppress the kill.

At timeout, compute heartbeat and dispatch absolute deadlines and choose the
one that elapsed first. Name integrity, monitor-state, durable-refresh, and
internal failures at their actual call sites. The synchronization lease and
activity channel remain independent.

- [ ] **Step 5: Verify GREEN**

Re-run the Darwin dedicated check. It must invoke
`watchdog-test.py`, `timeout-ordering-test.py`, and the launch-run-id unit
test against the exact generated launcher.

- [ ] **Step 6: Commit and independently audit watchdog attribution**

Commit the listed files:

    git commit -S -m "Record dedicated Anvil watchdog causes"

Run the per-commit fess audit and resolve every verified finding.

---

### Task 6: Validate watchdog events in the supervisor and enrich the probe

**Files:**
- Modify: packages/anvil-mcp/agent-supervisor.py
- Modify: packages/anvil-mcp/agent-supervisor-test.py
- Modify: packages/anvil-mcp/agent-supervisor-smoke.py
- Modify: packages/anvil-mcp/default.nix
- Modify: packages/anvil-mcp/headless-smoke.py or headless-smoke.nix only where integration assertions belong

**Interfaces:**
- Produces: read_watchdog_event(runtime_dir, expected_pid, expected_uid,
  expected_run_id) returning a sanitized dict or None.
- Defines stale exactly as run_id inequality; PID mismatch is a separate
  rejection.
- Adds: last_watchdog to supervisor status only for a valid matching event.
- Preserves: restart_reason daemon-exited:CODE.
- Adds one Nix-local probe line with restart count, cause, phase, and registered
  tool.

- [ ] **Step 1: Add failing supervisor unit tests using the generated writer**

Have the Nix check pass the Task 5 generated event-writer artifact to
`agent-supervisor-test.py`. Use that writer for every valid fixture; mutate
one field at a time only for rejection tests. Cover:

- valid event adoption;
- stale run ID and wrong PID as distinct rejections;
- symlink rejection and mandatory `O_NOFOLLOW`;
- wrong owner, link count, or mode rejection where the platform permits;
- exact key-set rejection, unknown schema/cause/phase/method rejection, and
  non-null invalid optional deadline fields;
- over-128-byte tool and over-4,096-byte record rejection by reading at most
  4,097 bytes;
- bool rejection for integer fields, since Python bool is an int subclass;
- restart_reason unchanged;
- the secret sentinel absent from the sanitized return/status.

- [ ] **Step 2: Add failing forced-timeout smoke assertions**

Extend the existing heartbeat and dispatch watchdog scenarios. Require:

- non-yielding root failure records `heartbeat-timeout` and the last
  phase/registered tool;
- recursive dispatch overrun records `dispatch-timeout`;
- supervisor `restart_count` increases;
- `restart_reason` remains `daemon-exited:-9`;
- probe output contains exactly one bounded line with
  `root-restarts=N cause=CAUSE phase=PHASE tool=TOOL-OR-none`;
- the secret sentinel is absent from activity, event, status, probe, daemon
  diagnostics, and test logs.

- [ ] **Step 3: Run the Darwin check and witness RED**

    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L

Expected failure: missing strict event ingestion, `last_watchdog`, or probe
summary.

- [ ] **Step 4: Implement strict event ingestion**

In `agent-supervisor.py`:

- require `O_NOFOLLOW`; open relative to a validated runtime-directory
  descriptor with `O_RDONLY|O_NOFOLLOW`;
- `fstat` and validate regular type, owner, mode 0600, and link count one;
- read no more than 4,097 bytes and reject a record over 4,096 bytes;
- decode strict UTF-8 and parse JSON;
- require the exact schema-version-1 key set, matching daemon PID and retained
  run ID, allowed enums, a null or at-most-128-byte registered tool, and
  non-negative integer timings that explicitly reject bool;
- return None for invalid, stale, or wrong-PID telemetry because diagnostics
  are optional, while preserving process restart;
- after `daemon.poll` returns, read the event with that process's PID and
  retained run ID before starting the replacement root, and store a valid
  result as `last_watchdog`;
- keep `restart_reason` unchanged.

In `default.nix`, advise `anvil-worker--tool-probe` only in the dedicated
deployment. Read the supervisor status through its existing validated path and
append exactly one bounded root line. Never include paths, arguments, request
IDs, raw status, or unvalidated strings.

- [ ] **Step 5: Verify GREEN and complete Nix gates**

    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L

    direnv exec . nix flake check -L

    direnv exec . ./build system

Run Linux headless checks only when an explicit Linux builder is available.
Expected: every applicable command exits zero.

- [ ] **Step 6: Commit and independently audit supervisor integration**

Commit:

    git commit -S -m "Expose Anvil watchdog restart diagnostics"

Run the per-commit fess audit and resolve every verified finding.

---

### Task 7: Whole-branch review, publication, deployment, and production proof

**Files:**
- No planned tracked changes; keep final evidence in the ignored progress ledger.
- Create code or test changes only when review or runtime evidence proves they
  are required.

**Interfaces:**
- Consumes: the already-published definitive Anvil revision and all Nix commits.
- Produces: a pushed Nix branch, switched Hera generation, and production
  evidence satisfying every acceptance criterion.

- [ ] **Step 1: Drain partner observations**

In each repository, inspect `doc/observations` for regular non-hidden Markdown
files. If present, run the partner-cleanup workflow before final review and
verify its cleanup commit.

- [ ] **Step 2: Run independent whole-branch and cross-repository reviews**

Generate review packages from each merge base to HEAD. Dispatch the strongest
available reviewer against the frozen spec, plan, test evidence, generated
launcher, supervisor, and full diffs. Fix all Critical and Important findings
in one coherent wave, rerun covering tests, and re-review.

An Anvil fix discovered after Task 4 must follow this complete order: commit and
audit the fix, rerun all Anvil gates, push the new fast-forward Anvil tip,
recompute its published archive hash, update and commit the Nix pin, audit that
pin commit, and rerun all Nix gates. Never leave `source.nix` naming an
unpublished or superseded Anvil revision.

- [ ] **Step 3: Run final fess audits**

Audit the last work commit in each repository, even when the most recent commit
only fixes earlier findings. Require explicit coverage of stubs, vacuous tests,
fixture drift, error swallowing, suppressions, fallback smuggling, spec drift,
scope creep, documentation drift, verification gaps, and loose ends.

- [ ] **Step 4: Rebase only the Nix branch and rerun full gates**

Anvil history was finalized before the pin and must not be rewritten here.
Fetch and verify it instead:

    git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience fetch fork
    test "$(git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience rev-parse HEAD)" = \
      "$(git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience rev-parse fork/fix/anvil-root-resilience)"

For Nix:

    git fetch origin
    git rebase origin/main
    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L
    direnv exec . nix flake check -L
    direnv exec . ./build system

After the rebase, verify the evaluated `currentAnvilRev` still equals the
published Anvil branch tip. Resolve conflicts without guessing intent. Any
post-rebase task change requires a commit, independent fess audit, and the
covering gates again.

- [ ] **Step 5: Push and prove both repositories are synchronized**

The Anvil branch should already be published. Satisfy its landing check without
rewriting history:

    git pull --rebase fork fix/anvil-root-resilience
    git push -u fork fix/anvil-root-resilience
    git status --short --branch

For the Nix branch, do not pull a remote feature ref that does not exist:

    if git ls-remote --exit-code --heads origin fix/anvil-root-resilience >/dev/null
    then
      git pull --rebase origin fix/anvil-root-resilience
    else
      git fetch origin
      git rebase origin/main
    fi
    git push -u origin fix/anvil-root-resilience
    git status --short --branch

Both statuses must show no task-owned uncommitted changes and no ahead/behind
divergence. The evaluated Nix pin must equal the pushed Anvil tip.

- [ ] **Step 6: Switch Hera**

From the clean Nix worktree:

    direnv exec . sudo darwin-rebuild switch \
      --flake .#hera \
      --override-input ai-nix /Users/johnw/src/ai-nix

Do not modify ai-nix. Confirm exit zero and record the active generation and
evaluated Anvil revision.

- [ ] **Step 7: Acquire a fresh bridge and run production smoke**

A pre-existing per-agent bridge retains its old generation. Start or reacquire
a fresh bridge after the switch, then prove:

- `anvil-mcp --version` resolves to the newly pinned version/revision;
- a new lazy worker pool reports cold, demanded=no, and no dead rows;
- both status paths use one quick check per worker and create no worker process;
- an oversized unbounded file-read returns explicit `offset=0 limit=200`
  guidance without a root restart;
- the same file with a positive limit returns a bounded page;
- a small normal file-read succeeds;
- ordinary `emacs-eval` and `shell-run` succeed;
- supervisor `restart_count` stays unchanged throughout non-faulting smoke;
- packaged forced heartbeat and dispatch scenarios expose their exact
  `last_watchdog` causes;
- no secret sentinel appears in retained diagnostics.

- [ ] **Step 8: Final requirement audit and cleanup**

Walk every acceptance criterion in the frozen design and bind it to direct
evidence in the progress ledger. Confirm no regular partner observations
remain. Clear task-created stashes, prune stale worktree metadata, and remove
task-created worktrees only after their branches are pushed and their removal
cannot discard evidence. Leave unrelated user changes untouched.

Mark the persistent goal complete only after all evidence is present.

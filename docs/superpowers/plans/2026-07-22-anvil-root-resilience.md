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
- Watchdog telemetry never records arguments, paths, expressions, request IDs, raw JSON, results, environment values, or output.
- The synchronization lease and the activity record remain separate; activity updates never change dispatch generation.
- restart_reason remains daemon-exited:CODE; last_watchdog is additive and accepted only for a valid matching daemon PID.
- Existing 45-second heartbeat and 225-second dispatch deadlines remain unchanged.
- ai-nix is read-only for this task.
- Preserve unrelated edits in /Users/johnw/src/nix/config/packages.nix and /Users/johnw/src/nix/docs/PI-AGENT-WIGGUM-PLAN.md.
- Every behavior change follows a witnessed red-green cycle.
- Every work commit receives an independent fess audit; final completion also requires whole-branch review, full gates, local rebase, push, Hera switch, and production smoke.

---

### Task 0: Create the isolated Anvil worktree and prove the baseline

**Files:**
- No tracked file changes.

**Interfaces:**
- Produces: /Users/johnw/src/emacs-lisp/anvil-root-resilience on branch fix/anvil-root-resilience.
- Base: fork/fix/issue-53-interrupted-hangs at the committed plan revision.

- [ ] **Step 1: Detect worktree state and create the child branch**

From /Users/johnw/src/emacs-lisp/anvil.el run:

    git fetch fork
    git worktree add /Users/johnw/src/emacs-lisp/anvil-root-resilience \
      -b fix/anvil-root-resilience fix/issue-53-interrupted-hangs

Do not rewrite fix/issue-53-interrupted-hangs. All later Anvil source and test work runs in the new worktree.

- [ ] **Step 2: Verify the clean baseline**

From /Users/johnw/src/emacs-lisp/anvil-root-resilience run:

    make test
    make test-all
    make lint
    make byte-compile

Expected: every command exits zero. A failing baseline triggers root-cause investigation before implementation.

- [ ] **Step 3: Initialize durable progress**

Create the ignored .superpowers/sdd/progress.md ledger and record Task 0 complete with the baseline command results. Update the tracked handoff current-repository path to the isolated worktree in the first implementation commit.

---

### Task 1: Truthful nonblocking worker states

**Files:**
- Modify: anvil-worker.el
- Modify: tests/anvil-worker-test.el
- Modify: openspec/specs/worker-pool-spawn-deferred/spec.md

**Interfaces:**
- Produces: anvil-worker--reported-state WORKER ENDPOINT-ALIVE returning one of cold, alive, busy, unresponsive, or dead.
- Consumes: worker plist keys :busy, :hung-checks, :demanded, :last-state, :server-file.
- Preserves: anvil-worker--quick-alive-p as the only endpoint check used by reporting.

- [ ] **Step 1: Add failing state-classifier tests**

Add one ERT table that exercises the exact precedence:

    (dolist (case
             '(((:demanded nil) nil cold)
               ((:demanded t) nil dead)
               ((:demanded nil) t alive)
               ((:demanded t :hung-checks 1) t unresponsive)
               ((:demanded t :hung-checks 1 :busy t) t busy)))
      (pcase-let ((`(,worker ,endpoint ,expected) case))
        (should (eq expected
                    (anvil-worker--reported-state worker endpoint)))))

Add a probe regression with a cold pool. Stub anvil-worker--quick-alive-p to nil, anvil-worker-spawn and anvil-worker--worker-alive-p to fail if called, then assert the rendered text contains cold and demanded=no and contains no worker row labeled dead.

- [ ] **Step 2: Run the focused worker tests and witness RED**

Run:

    emacs --batch -Q -L . -L tests       -l tests/anvil-worker-test.el       --eval "(ert-run-tests-batch-and-exit "^anvil-worker-test-.*reported\|^anvil-worker-test-.*probe")"

Expected failure: void-function anvil-worker--reported-state or cold probe output still rendered as dead.

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

Add a formatter shared by anvil-worker-status and anvil-worker--tool-probe. It emits state, demanded=yes or no, last=alive, dead, or unknown, conditional PID, and conditional probe-failures=N/LIMIT. Both callers compute endpoint-alive once with anvil-worker--quick-alive-p and pass it to the helper.

Update the registered probe description to name all five states. Do not change health, spawn, ownership, or recovery functions.

- [ ] **Step 4: Update the worker-pool observability requirement**

Add an acceptance scenario to worker-pool-spawn-deferred/spec.md:

- never-demanded plus absent endpoint reports cold;
- demanded plus absent endpoint reports dead;
- reachable endpoint reports alive, busy, or unresponsive;
- status reporting performs no spawn or full liveness probe.

- [ ] **Step 5: Verify GREEN and the full worker suites**

Run the focused command from Step 2, then:

    emacs --batch -Q -L . -L tests       -l tests/anvil-worker-test.el       -f ert-run-tests-batch-and-exit

    emacs --batch -Q -L . -L tests       -l tests/anvil-worker-pool-test.el       -f ert-run-tests-batch-and-exit

Expected: all selected tests pass with zero unexpected results.

- [ ] **Step 6: Commit and independently audit**

Stage only the three task files and commit:

    git commit -S -m "Report lazy Anvil workers as cold"

Dispatch a read-only fess audit for the commit using the frozen spec and this task. Verify and fix every real finding before Task 2.

---

### Task 2: Fail oversized unbounded file reads before body loading

**Files:**
- Modify: anvil-file.el
- Modify: tests/anvil-file-test.el

**Interfaces:**
- Produces: anvil-file-max-unbounded-read-bytes, default 1,048,576.
- Produces: anvil-file--guard-unbounded-read ABS LIMIT.
- Consumes: file-attribute-size without reading file contents.

- [ ] **Step 1: Add failing read-boundary tests**

Create an oversized temporary file while dynamically binding the threshold to 16 bytes. Around anvil-file-read, stub anvil--insert-file so any body load fails the test. Assert that both calls below signal user-error containing file size, limit, offset, and limit 200 guidance:

    (anvil-file-read path)
    (anvil-file-read path 50 nil)

Add positive cases proving a small unbounded read is unchanged, a large read with limit 1 returns one line, and a nil or zero guard restores legacy behavior.

- [ ] **Step 2: Run focused file tests and witness RED**

Run:

    emacs --batch -Q -L . -L tests       -l tests/anvil-file-test.el       --eval "(ert-run-tests-batch-and-exit "^anvil-file-test-.*unbounded\|^anvil-file-test-.*read-limit")"

Expected failure: oversized calls reach anvil--insert-file or do not signal.

- [ ] **Step 3: Implement the early guard**

Add:

    (defcustom anvil-file-max-unbounded-read-bytes (* 1024 1024)
      "Maximum regular-file size accepted when file-read has no LIMIT.
Nil or a non-positive value disables the guard."
      :type '(choice (const :tag "Disabled" nil) integer)
      :group 'anvil-file)

    (defun anvil-file--guard-unbounded-read (abs limit)
      "Reject an oversized read of ABS when LIMIT is absent."
      (let* ((cap anvil-file-max-unbounded-read-bytes)
             (attrs (file-attributes abs 'integer))
             (size (and attrs
                        (null (file-attribute-type attrs))
                        (file-attribute-size attrs))))
        (when (and (numberp cap) (> cap 0)
                   (null limit) size (> size cap))
          (user-error
           (concat "file-read refused unbounded %d-byte file "
                   "(inline limit %d); retry with offset="0" "
                   "and limit="200", then advance offset")
           size cap))))

Call this helper after anvil--prepare-path and before anvil-file-warn-if-diverged or anvil--insert-file. Offset is intentionally not part of the decision.

Update the tool docstring and registered description so clients know that large files require a positive limit.

- [ ] **Step 4: Verify GREEN and all file tests**

Run the focused command, then:

    emacs --batch -Q -L . -L tests       -l tests/anvil-file-test.el       -f ert-run-tests-batch-and-exit

Expected: all tests pass with zero unexpected results.

- [ ] **Step 5: Commit and independently audit**

Commit:

    git commit -S -m "Bound unpaginated Anvil file reads"

Run the per-commit fess audit and resolve every verified finding.

---

### Task 3: Stop oversized inline results before JSON-RPC encoding

**Files:**
- Modify: anvil-server.el
- Modify: tests/anvil-server-test.el

**Interfaces:**
- Produces: anvil-server-max-inline-result-bytes, default 2,097,152.
- Produces: anvil-server--enforce-inline-result-limit TOOL-NAME RESULT-TEXT returning RESULT-TEXT or signaling anvil-server-tool-error.
- Integration point: immediately after result-text construction and before disclosure, metrics, MCP wrapping, or anvil-server--respond-with-result.

- [ ] **Step 1: Add failing end-to-end server tests**

Register a temporary tool returning a unique payload longer than a dynamically bound 32-byte cap. Send a real tools/call JSON-RPC request through anvil-server-process-jsonrpc. Assert:

- response is valid JSON-RPC;
- tool result has isError true;
- error names the tool, observed byte count, and configured limit;
- unique payload is absent;
- disclosure and metrics hooks do not receive the rejected payload.

Add boundary tests: exactly 32 UTF-8 bytes succeeds; 33 bytes fails; a multibyte string is measured after UTF-8 encoding; nil and zero disable the guard.

- [ ] **Step 2: Run focused server tests and witness RED**

Run:

    emacs --batch -Q -L . -L tests       -l tests/anvil-server-test.el       --eval "(ert-run-tests-batch-and-exit "^anvil-server-test-.*inline-result")"

Expected failure: oversized payload is returned or the guard function is absent.

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

Apply it as a separate let-star binding before anvil-disclosure-budget-apply. Never interpolate result-text into the error.

- [ ] **Step 4: Verify GREEN and full server tests**

Run the focused command, then:

    emacs --batch -Q -L . -L tests       -l tests/anvil-server-test.el       -f ert-run-tests-batch-and-exit

Expected: all tests pass with zero unexpected results.

- [ ] **Step 5: Run all upstream Anvil gates**

Run:

    make test
    make test-all
    make lint
    make byte-compile

Expected: every command exits zero with no test failures or compile errors.

- [ ] **Step 6: Commit, audit, and push upstream Anvil**

Commit:

    git commit -S -m "Reject oversized inline Anvil results"

Run the per-commit fess audit. After verified findings are fixed, run:

    git fetch fork
    git rebase fork/fix/issue-53-interrupted-hangs
    git push -u fork fix/anvil-root-resilience
    git status --short --branch

Record the resulting Anvil revision in the handoff.

---

### Task 4: Pin the verified Anvil revision in an isolated Nix worktree

**Files:**
- Modify: packages/anvil-mcp/source.nix
- Create or update: docs/superpowers/handoffs/2026-07-22-anvil-root-resilience.md in the Anvil repository only

**Interfaces:**
- Consumes: the pushed Anvil commit from Task 3.
- Produces: matching rev and SRI sha256 in source.nix.

- [ ] **Step 1: Create an isolated Nix worktree**

From /Users/johnw/src/nix, detect worktree state. Because main contains unrelated changes, create a sibling linked worktree on branch fix/anvil-root-resilience, based on the current committed local main. Do not touch the dirty checkout.

Use:

    git fetch origin
    git worktree add /Users/johnw/src/nix-anvil-root-resilience \
      -b fix/anvil-root-resilience main

Confirm the new worktree is clean. Run every Nix command through direnv exec ..

- [ ] **Step 2: Compute the source hash**

Run:

    anvil_rev=$(git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience rev-parse HEAD)
    base32_hash=$(nix-prefetch-url --unpack \
      "https://github.com/jwiegley/anvil.el/archive/${anvil_rev}.tar.gz")
    sri_hash=$(nix hash convert --hash-algo sha256 --to sri "$base32_hash")
    printf 'rev=%s\nhash=%s\n' "$anvil_rev" "$sri_hash"

Use the printed revision and SRI hash exactly in source.nix.

- [ ] **Step 3: Update source.nix and verify evaluation**

Replace date, hash, rev, and version metadata with the pushed revision and computed hash. Keep owner and repo unchanged.

Run:

    direnv exec . nix eval --raw       .#packages.aarch64-darwin.anvil-mcp-headless.currentAnvilRev

Expected output is the exact pushed Anvil revision.

- [ ] **Step 4: Commit and audit the pin**

Commit only source.nix:

    git commit -S -m "Pin Anvil root resilience changes"

Run an independent fess audit against the pin commit before watchdog work.

---

### Task 5: Persist exact sanitized watchdog causes

**Files:**
- Modify: packages/anvil-mcp/default.nix
- Modify: packages/anvil-mcp/watchdog-test.py
- Modify: packages/anvil-mcp/timeout-ordering-test.py when generated constant coverage requires it

**Interfaces:**
- Produces in generated Python: select_deadline_cause, sanitize_activity, write_watchdog_event.
- Produces private files: .anvil-root-activity.json and .anvil-root-watchdog.json.
- Preserves timeoutPolicy values and lease generation semantics.

- [ ] **Step 1: Add failing watchdog tests**

Add deterministic tests for:

- startup-timeout;
- heartbeat-timeout;
- dispatch-timeout;
- simultaneous expiry choosing the earlier absolute deadline;
- lock-integrity-failure;
- monitor-state-invalid;
- durable-refresh-failure;
- monitor-internal-error;
- mode 0600 and stable inode;
- a diagnostic write failure still calling the kill path;
- a unique secret sentinel absent from serialized activity and event records.

Use real temporary files and os.open with O_NOFOLLOW where supported. Do not mock JSON shapes independently of the generated launcher; extract and execute the generated helper definitions as the existing test harness does.

- [ ] **Step 2: Build the focused check and witness RED**

Run:

    direnv exec . nix build       .#checks.aarch64-darwin.anvil-mcp-headless -L

Expected failure is in watchdog-test.py because the event helpers or records do not yet exist.

- [ ] **Step 3: Implement private activity and event records**

In dedicatedLockLauncher:

- create both files under the canonical 0700 runtime directory;
- open with O_CREAT, O_RDWR, O_NOFOLLOW when available, mode 0600;
- validate regular-file type, owner UID, mode, device, and inode;
- keep activity and event descriptors separate from pulse and lease descriptors;
- serialize only version, daemon_pid, enum phase/cause/method, registered tool, and integer timing fields;
- bound phase, method, tool, and cause strings before serialization;
- fsync each final event best-effort, but never allow telemetry failure to suppress SIGKILL.

Refactor kill_parent_if to accept a cause/event factory that runs only after the verifier confirms the failure still exists. In the deadline branch, compute heartbeat and dispatch absolute deadlines and select the one that expired first.

Nix-generated Emacs advice updates the activity inode at startup, parse, dispatch, tool-call, result-encode, response-write, and idle. It writes a tool only when gethash finds that identifier in the active registered tool table. It never writes params or request data.

- [ ] **Step 4: Verify GREEN**

Re-run the headless check from Step 2. The check invokes watchdog-test.py and timeout-ordering-test.py with the exact generated launcher and policy paths from the derivation.

Expected: the full headless check exits zero and both named tests report success in the build log.

- [ ] **Step 5: Commit and audit watchdog attribution**

Commit the task files:

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
- Produces: read_watchdog_event(path, expected_pid, expected_uid) returning a sanitized dict or None.
- Adds: last_watchdog to supervisor status only for a valid matching event.
- Preserves: restart_reason daemon-exited:CODE.
- Adds one Nix-local probe line with restart count, cause, phase, and registered tool.

- [ ] **Step 1: Add failing supervisor unit tests**

Use real files to cover:

- valid event adoption;
- wrong PID and stale event rejection;
- symlink rejection;
- wrong owner or mode rejection where the platform permits;
- unknown cause, phase, or method rejection;
- oversized tool rejection;
- non-integer timing rejection;
- restart_reason unchanged;
- secret sentinel absent from returned status.

- [ ] **Step 2: Add failing forced-timeout smoke assertions**

Extend the existing heartbeat and dispatch watchdog scenarios. Require:

- non-yielding root failure records heartbeat-timeout and the last phase/tool;
- recursive dispatch overrun records dispatch-timeout;
- supervisor restart_count increases;
- restart_reason remains daemon-exited:-9;
- probe output includes the bounded root summary;
- the secret sentinel is absent from activity, event, status, probe, and logs.

- [ ] **Step 3: Run the headless check and witness RED**

Run the same Nix headless check. Expected failure: missing last_watchdog or probe summary.

- [ ] **Step 4: Implement strict event ingestion**

In agent-supervisor.py:

- use os.open with O_RDONLY, O_NOFOLLOW when available;
- fstat and validate regular type, owner, and mode 0600;
- read a bounded maximum, decode UTF-8, parse JSON;
- require exact schema version, matching daemon_pid, allowed enums, bounded registered tool, and integer non-negative timings;
- return None for invalid or stale telemetry because diagnostics are optional, while preserving the process restart;
- after daemon.poll returns, read the matching event before starting the replacement root and store it as last_watchdog;
- keep restart_reason unchanged.

In default.nix, advise anvil-worker--tool-probe only in the dedicated deployment. Append a single root line that reads the supervisor status safely and prints restart count plus cause, phase, and tool. Never include paths, arguments, or raw status.

- [ ] **Step 5: Verify GREEN and complete Nix package gates**

Run:

    direnv exec . nix build       .#checks.aarch64-darwin.anvil-mcp-headless -L

    direnv exec . nix build       .#checks.aarch64-darwin.anvil-mcp-dedicated -L

    direnv exec . nix flake check -L

Expected: all commands exit zero.

- [ ] **Step 6: Commit and audit supervisor integration**

Commit:

    git commit -S -m "Expose Anvil watchdog restart diagnostics"

Run the per-commit fess audit and resolve every verified finding.

---

### Task 7: Whole-branch review, deployment, and production proof

**Files:**
- Update only the handoff document with final evidence.
- Do not create code changes unless an audit or runtime failure proves they are required.

**Interfaces:**
- Consumes: all Anvil and Nix commits.
- Produces: pushed branches, switched Hera generation, and production evidence satisfying every acceptance criterion.

- [ ] **Step 1: Drain partner observations**

In each repository, inspect doc/observations for regular non-hidden Markdown files. If present, run the partner-cleanup workflow before final review and verify its cleanup commit.

- [ ] **Step 2: Run independent whole-branch reviews**

Generate review packages from each merge base to HEAD. Dispatch the strongest available reviewer against the frozen spec, plan, test evidence, and full diffs. Fix all Critical and Important findings in one coherent fix wave, rerun covering tests, and re-review.

- [ ] **Step 3: Run final fess audits**

Audit the last work commit in each repository, even when the most recent commit only fixed earlier findings. Require explicit coverage of stubs, vacuous tests, fixture drift, error swallowing, suppressions, fallback smuggling, spec drift, scope creep, documentation drift, verification gaps, and loose ends.

- [ ] **Step 4: Rebase locally and rerun full gates**

For Anvil:

    git fetch fork
    git rebase fork/fix/issue-53-interrupted-hangs
    make test
    make test-all
    make lint
    make byte-compile

For Nix:

    git fetch origin
    git rebase main
    direnv exec . nix flake check -L
    direnv exec . ./build system

Resolve conflicts without guessing intent. Recompute and update source.nix only if rebasing Anvil changes the pinned revision.

- [ ] **Step 5: Push both branches**

In /Users/johnw/src/emacs-lisp/anvil-root-resilience:

    git pull --rebase fork fix/anvil-root-resilience
    git push -u fork fix/anvil-root-resilience
    git status --short --branch

In /Users/johnw/src/nix-anvil-root-resilience:

    git pull --rebase origin fix/anvil-root-resilience
    git push -u origin fix/anvil-root-resilience
    git status --short --branch

Both statuses must show no task-owned uncommitted changes and no ahead/behind divergence.

- [ ] **Step 6: Switch Hera**

From the clean Nix worktree:

    direnv exec . sudo darwin-rebuild switch       --flake .#hera       --override-input ai-nix /Users/johnw/src/ai-nix

Do not modify ai-nix. Confirm exit zero and record the active generation.

- [ ] **Step 7: Acquire a fresh bridge and run production smoke**

A pre-existing per-agent bridge retains its old generation. Start or reacquire a fresh bridge after the switch, then prove:

- anvil-mcp --version resolves to the newly pinned revision/version;
- a new lazy worker pool reports cold, demanded=no, and no dead rows;
- probing does not create worker processes;
- an oversized unbounded file-read returns pagination guidance without root restart;
- the same file with a positive limit returns a bounded page;
- a small normal file-read succeeds;
- ordinary emacs-eval and shell-run succeed;
- supervisor restart_count stays unchanged throughout non-faulting smoke;
- forced heartbeat and dispatch scenarios in the packaged smoke expose their exact last_watchdog causes;
- no secret sentinel appears in retained diagnostics.

- [ ] **Step 8: Final completion audit and cleanup**

Walk every acceptance criterion in the frozen design and bind it to direct evidence. Clear task-created stashes, remove the Nix worktree only after its branch is pushed and integrated state is safe, prune worktrees and remote branches that are already merged, and leave unrelated user changes untouched.

Mark the persistent goal complete only after all evidence is present.

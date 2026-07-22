# Anvil Root Resilience Design

**Date:** 2026-07-22
**Status:** Approved for implementation
**Repositories:** /Users/johnw/src/emacs-lisp/anvil.el and /Users/johnw/src/nix

## Problem

A Hera agent reported that its Anvil worker pool had died while the root Emacs remained responsive. Investigation established two distinct problems:

1. Lazy, never-demanded workers were absent by design, but anvil-worker-probe rendered every absent endpoint as dead. Resetting such a pool merely changed it from a valid cold state to a warm state.
2. The per-agent root Emacs had actually been killed and restarted repeatedly. Calls failing after 46 to 49 seconds aligned with the 45-second root heartbeat watchdog. Root termination removed any child workers, and the replacement root correctly began with a cold pool.

The clearest trigger was an unbounded file-read of a 149,239-line Rust file. Anvil constructed and encoded the entire result synchronously in root Emacs. The watchdog recorded only daemon-exited:-9, so the exact timeout class and root phase had to be inferred after the fact.

## Goals

- Report worker lifecycle state truthfully without spawning workers or performing a blocking liveness check.
- Prevent oversized unbounded file reads from reaching body loading or response construction.
- Prevent any oversized inline tool result from entering expensive final JSON response encoding.
- Record the exact watchdog cause and the last sanitized root activity phase.
- Surface restart and watchdog information through the existing administrative probe.
- Preserve per-agent isolation, lazy worker startup, current timeout ordering, and existing supervisor compatibility.
- Deploy and production-smoke the result on Hera.

## Non-goals

- Do not make workers eager.
- Do not increase watchdog deadlines to hide root starvation.
- Do not route all file or shell operations through the worker pool.
- Do not return an oversized file automatically in a subprocess; the large result would still need to cross root Emacs and the MCP transport.
- Do not record request arguments, paths, expressions, output, request identifiers, caller-derived environment values, or other potentially sensitive data. The generated non-secret run ID is the sole environment-carried protocol value.
- Do not change ai-nix; it contains no Anvil worker or watchdog implementation.
- Do not alter unrelated existing changes in /Users/johnw/src/nix/config/packages.nix or /Users/johnw/src/nix/docs/PI-AGENT-WIGGUM-PLAN.md.

## Approach

Use deterministic bounds and structured diagnostics.

Transparent offloading was rejected because it preserves the oversized response and still requires root Emacs to receive and encode it. Eager workers or larger watchdog windows were rejected because they disguise the observed state without fixing root starvation. The selected approach prevents the demonstrated unsafe operation, caps the remaining response boundary, and makes any future watchdog kill directly attributable.

## Component 1: Worker State Reporting

### State model

Add a pure, nonblocking worker-state classifier in anvil-worker.el. It receives the worker plist and the result of the existing quick endpoint check. It returns exactly one primary state with this precedence:

1. busy when the endpoint is reachable and the worker is marked busy.
2. unresponsive when the endpoint is reachable, the worker is not busy, and the recorded consecutive full-probe failure count is greater than zero.
3. alive when the endpoint is reachable.
4. cold when the endpoint is absent and the worker has never been demanded.
5. dead when the endpoint is absent and the worker has been demanded.

Endpoint reachability takes precedence over the demanded flag so an inherited or pre-existing reachable worker cannot be described as cold. Busy takes precedence over stale probe-failure evidence. The classifier must not consult last-state as its primary signal because health checking deliberately records intermediate dead observations there.

Both anvil-worker-status and anvil-worker--tool-probe will use this classifier. Probe output will include:

- the primary state;
- demanded=yes or demanded=no;
- last=alive, last=dead, or last=unknown;
- the PID only for a reachable endpoint;
- probe-failures=N/LIMIT only when failures are recorded.

The probe remains read-only and nonblocking. It must not spawn workers and must not call the full emacsclient liveness probe.

### Compatibility

Worker spawning, health recovery, ownership checks, grace periods, and metrics remain unchanged. This is an observability correction, not a second lifecycle state machine.

## Component 2: Bounded File and Tool Results

### Oversized unbounded file reads

Add anvil-file-max-unbounded-read-bytes, defaulting to 1,048,576 bytes.

Before anvil-file-read inserts file contents, it will inspect the regular file size. When limit is omitted, regardless of whether offset is present, and the file exceeds the configured limit, it will signal a tool error containing:

- the file size;
- the configured inline limit;
- an instruction to retry with offset and limit;
- a concrete first-page example using offset 0 and limit 200.

The failure must happen before anvil--insert-file or any equivalent body-loading function runs. It must not include file contents.

A caller that supplies a positive limit retains the current paginated behavior. Supplying offset alone does not bypass the guard because the result remains unbounded. Small unbounded reads retain their current behavior. A nil or non-positive configured limit disables this file-specific guard for installations that explicitly require legacy behavior.

The file-read tool description will state that large files require pagination.

### Generic inline-result boundary

Add anvil-server-max-inline-result-bytes, defaulting to 2,097,152 bytes.

After a handler result has become result-text but before disclosure processing, metrics payload recording, MCP wrapping, and final JSON-RPC encoding, measure its encoded UTF-8 size. If it exceeds the limit, return a bounded tool error naming only:

- the registered tool identifier;
- the observed byte count;
- the configured limit;
- guidance to use a paginated, filtered, tee, or asynchronous interface.

The error must never include the rejected result. A nil or non-positive configured limit disables this guard.

The file-specific guard is intentionally lower and earlier: it avoids loading and formatting the demonstrated dangerous input. The server guard protects other handlers from expensive final response encoding.

### Shell behavior

Do not reroute shell-run or shell-tee-grep. Their process wait already yields to Emacs, capture is bounded, and filtering operates on the bounded captured prefix. Native execution remains the correct path for long-lived process supervision; the Anvil worker pool is not involved in these shell tools.

## Component 3: Structured Watchdog Attribution

### Root activity record

The launcher creates a private Unix-domain stream endpoint plus separate
fixed-inode activity and event records in the canonical mode-0700 per-agent
runtime directory.  The monitor exclusively owns the two regular-file
descriptors.  Root Emacs receives only the endpoint pathname and a run
identifier, connects once with `make-network-process`, and sends bounded
activity messages over that connection.  After accepting the root connection,
the monitor unlinks the socket pathname.  Consequently all later updates use
the established connection, pathname replacement cannot redirect them, and no
raw diagnostic descriptor is inherited by root subprocesses.

The activity protocol and persisted record use schema version 1 with exactly
these JSON keys:

- `schema_version`: integer `1`;
- `run_id`: exactly 32 lowercase hexadecimal characters, freshly generated
  by the supervisor for this daemon launch;
- `daemon_pid`: the positive root daemon PID;
- `sequence`: a non-negative integer that strictly increases per root;
- `phase`: one of `startup`, `parse`, `dispatch`, `tool-call`,
  `result-encode`, `response-write`, or `idle`;
- `method`: one of `none`, `initialize`,
  `notifications/initialized`, `ping`, `tools/list`, `tools/call`,
  `resources/list`, `resources/read`, `resources/templates/list`, or
  `other`;
- `tool`: JSON null, or a currently registered tool identifier no longer
  than 128 UTF-8 bytes;
- `phase_started_unix_ms` and `observed_at_unix_ms`: non-negative integer
  Unix milliseconds.

One newline-terminated activity message is at most 1,024 UTF-8 bytes.  The
monitor accepts only the expected run identifier and daemon PID, a sequence
greater than the last accepted sequence, exact keys, valid enum values, and
valid integer fields.  It serializes accepted metadata back to at most 1,024
UTF-8 bytes through its already-open activity descriptor, truncates at byte
zero, and fsyncs it.  An invalid or partial message is discarded while the
last valid activity remains authoritative.

Nix-local advice around the Anvil request boundary emits these messages.
Unrecognized methods become `other`; a tool becomes null unless it exists in
the active registered-tool table.  Arguments, request IDs, paths, expressions,
raw JSON, results, and caller-derived environment values are prohibited; the
generated non-secret run identifier is the sole environment-carried protocol
value.

The activity record is separate from the synchronization lease. Rewriting the lease would advance its generation and could reset dispatch_started, weakening the dispatch deadline.

### Watchdog event record

Immediately after a failure is revalidated and immediately before SIGKILL, the
monitor writes one schema-version-1 event through its private fixed-inode
descriptor.  Every event contains exactly:

- `schema_version`, `run_id`, and `daemon_pid` under the same constraints
  as the activity record;
- `cause`: one of `startup-timeout`, `heartbeat-timeout`,
  `dispatch-timeout`, `lock-integrity-failure`,
  `monitor-state-invalid`, `durable-refresh-failure`, or
  `monitor-internal-error`;
- `phase`: an activity phase or `unknown`;
- `method`: one of the activity method enum values;
- `tool`: null or at most 128 UTF-8 bytes;
- `observed_at_unix_ms` and `daemon_uptime_ms`: non-negative integers;
- `heartbeat_age_ms`, `heartbeat_limit_ms`, `dispatch_age_ms`, and
  `dispatch_limit_ms`: non-negative integers when that deadline has an
  anchor, otherwise null.

The event is canonical compact UTF-8 JSON no larger than 4,096 bytes.  All
durations are integer milliseconds derived from monotonic time; only
`observed_at_unix_ms` is wall-clock time.  The monitor truncates and rewinds
the fixed inode before writing, fsyncs best-effort, and never allows a
diagnostic-write failure to suppress SIGKILL.

When heartbeat and dispatch deadlines have both expired, classify the cause
whose absolute deadline elapsed first.  Integrity, refresh, and internal
failure sites use their exact named causes rather than a timeout fallback.

### Supervisor status

The supervisor generates the run identifier before each daemon launch, passes
it in the launch environment, and retains it on that process object.  On exit,
`read_watchdog_event(runtime_dir, expected_pid, expected_uid,
expected_run_id)` opens the event with mandatory `O_NOFOLLOW`, reads at most
4,097 bytes, and validates the exact schema, regular-file ownership and mode
0600, enums, bounds, integer types, PID, and run identifier.  A record is stale
exactly when its `run_id` differs from `expected_run_id`; no wall-clock
heuristic is used.

A valid event becomes `last_watchdog` in the existing supervisor status.
Invalid, stale, oversized, or wrong-PID records are ignored.
`restart_reason` remains `daemon-exited:CODE` for backward compatibility.

The Nix deployment layer will append one bounded root line to anvil-worker-probe containing restart count and the last watchdog cause, phase, and tool. Upstream anvil-worker.el will not depend on the deployment-specific supervisor schema.

## Security and Privacy

No diagnostic artifact may contain user-controlled argument values. Tests will use a unique sentinel in a path, expression, and request argument and assert that the sentinel is absent from the activity record, watchdog record, supervisor status, probe output, and diagnostic logs.

Both diagnostic records are private regular files with stable identities; the transient socket is private and is unlinked after the root connects. `O_NOFOLLOW` is mandatory for every diagnostic open on supported Darwin and Linux deployments, and launcher configuration fails if it is unavailable. Symlinks, unexpected ownership, permissive modes, wrong daemon PIDs, stale run identifiers, unknown enum values, and oversized fields fail closed. Diagnostic validation failure affects observability only; it must not weaken watchdog termination or supervisor restart behavior.

## Testing Strategy

Implementation follows test-first development.

### Anvil tests

In tests/anvil-worker-test.el:

- table-test all five worker states;
- verify reachable plus undemanded reports alive;
- verify busy takes precedence over recorded failures;
- invoke both real status paths, fail if either spawns or calls the full liveness probe, and assert the quick endpoint check runs exactly once per worker in each path;
- assert cold, demanded=no, no false dead row, and exact last=alive/dead/unknown rendering;
- verify recorded failures render unresponsive with the bounded failure count and busy takes precedence.

In tests/anvil-file-test.el:

- create an oversized temporary file;
- stub the body-loading primitive;
- assert an unbounded read fails with pagination guidance before the stub runs;
- assert a bounded page still succeeds;
- assert small unbounded reads remain unchanged;
- assert disabling the configured limit retains legacy behavior.

In tests/anvil-test.el:

- register a handler whose result exceeds the configured inline limit;
- assert the response is bounded and omits the rejected payload;
- assert a result at the boundary succeeds;
- assert disabling the limit retains legacy behavior.

Run focused ERT during each red-green cycle, followed by make test, make test-all, make lint, and make byte-compile.

### Nix tests

Extend watchdog-test.py to cover:

- each cause classification;
- simultaneous deadline precedence;
- private file mode, stable inode handling, one-shot socket connection, and socket-path replacement after acceptance;
- exact schema validation, 1,024-byte activity messages, and 4,096-byte event records;
- proof that a root subprocess does not inherit a writable activity-record descriptor;
- diagnostic-write failure still leading to kill;
- secret sentinel non-retention.

Extend agent-supervisor-test.py to cover:

- valid event adoption;
- stale and wrong-PID rejection;
- symlink, owner, mode, enum, and length rejection;
- restart_reason compatibility.

Extend the existing forced watchdog scenarios in agent-supervisor-smoke.py:

- non-yielding heartbeat failure reports heartbeat-timeout with the expected phase and registered tool;
- recursive dispatch overrun reports dispatch-timeout;
- the secret sentinel is absent from all retained artifacts;
- worker probe reports the supervisor restart summary.

Run the focused Python and ERT tests, the complete packages/anvil-mcp checks, then ./build system before switching.

## Delivery Sequence

1. Commit, independently audit, and push this corrected specification and plan.
2. Create the isolated Anvil worktree from the verified remote planning commit and prove the baseline.
3. Add failing Anvil tests and confirm their expected failures.
4. Implement worker reporting and bounded result behavior; run all Anvil gates.
5. Commit and audit the Anvil changes, rebase once onto the fetched parent, rerun the full gates, and push the definitive Anvil revision.
6. Pin that published revision and archive hash in an isolated Nix worktree, using its committer timestamp and package header version as metadata provenance.
7. Add failing Nix watchdog and supervisor tests, then implement the generated diagnostics.
8. Rebase the Nix branch onto origin/main, run all Anvil-MCP checks and ./build system, audit, and push only task-owned changes.
9. Switch the Hera Darwin configuration.
10. Restart or reacquire an agent bridge so it receives the new generation.
11. Production-smoke cold worker reporting, paginated-read rejection, watchdog status schema, and normal tool execution.
12. Confirm both repositories are clean or contain only the user's pre-existing changes and are up to date with their remotes.

## Acceptance Criteria

- A fresh lazy pool reports cold rather than dead.
- A demanded worker with no reachable endpoint reports dead.
- A reachable worker reports exactly one of alive, busy, or unresponsive.
- Probing never starts workers or performs a blocking full liveness check.
- An oversized unbounded file-read fails before body loading and gives explicit pagination instructions.
- Oversized generic results do not enter final MCP JSON encoding and never appear in the error.
- Every watchdog kill records a validated cause and last sanitized activity when diagnostics are writable.
- Supervisor status preserves daemon-exited compatibility and adds last_watchdog only for a valid matching event.
- No request content or secret sentinel is retained.
- The Anvil and Nix quality gates pass.
- Hera runs the new pinned Anvil generation and production probes demonstrate the corrected behavior.

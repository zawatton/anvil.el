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
- Do not record request arguments, paths, expressions, output, request identifiers, environment values, or other potentially sensitive data.
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

The Nix-generated dedicated root will maintain a private fixed-inode activity record in its per-agent runtime directory. The file is mode 0600 and owned by the user. Updates overwrite the already-open inode; they do not replace the file or follow a new pathname.

The root records only bounded metadata:

- schema version;
- daemon PID;
- phase: startup, parse, dispatch, tool-call, result-encode, response-write, or idle;
- JSON-RPC method from a fixed allowlist;
- tool identifier only when it matches a currently registered tool;
- phase start time and observation time in Unix milliseconds.

Nix-local advice around the Anvil request boundary updates this record. Arguments, request IDs, paths, expressions, raw JSON, results, and environment values are prohibited.

The activity record is separate from the synchronization lease. Rewriting the lease would advance its generation and could reset dispatch_started, weakening the dispatch deadline.

### Watchdog event record

The watchdog monitor receives a pre-opened private result inode. Immediately after a failure is revalidated and immediately before SIGKILL, it writes one sanitized event with:

- schema version and daemon PID;
- cause: startup-timeout, heartbeat-timeout, dispatch-timeout, lock-integrity-failure, monitor-state-invalid, durable-refresh-failure, or monitor-internal-error;
- last activity phase, method, and registered tool identifier;
- observation time and daemon uptime;
- heartbeat age and limit when relevant;
- dispatch age and limit when relevant.

When heartbeat and dispatch deadlines have both expired, classify the cause whose absolute deadline elapsed first. Diagnostic-write failure must never prevent the required kill.

### Supervisor status

On daemon exit, agent-supervisor.py will read the event through a safe, non-symlink path and validate:

- file ownership and mode;
- schema and enum values;
- bounded string lengths;
- daemon PID equality with the process that exited;
- integer timestamp and duration fields.

A valid event becomes last_watchdog in the existing supervisor status. Invalid, stale, or wrong-PID records are ignored. restart_reason remains daemon-exited:CODE for backward compatibility.

The Nix deployment layer will append one bounded root line to anvil-worker-probe containing restart count and the last watchdog cause, phase, and tool. Upstream anvil-worker.el will not depend on the deployment-specific supervisor schema.

## Security and Privacy

No diagnostic artifact may contain user-controlled argument values. Tests will use a unique sentinel in a path, expression, and request argument and assert that the sentinel is absent from the activity record, watchdog record, supervisor status, probe output, and diagnostic logs.

All diagnostic files are private regular files with stable identities. Symlinks, unexpected ownership, permissive modes, wrong daemon PIDs, unknown enum values, and oversized fields fail closed. Diagnostic validation failure affects observability only; it must not weaken watchdog termination or supervisor restart behavior.

## Testing Strategy

Implementation follows test-first development.

### Anvil tests

In tests/anvil-worker-test.el:

- table-test all five worker states;
- verify reachable plus undemanded reports alive;
- verify busy takes precedence over recorded failures;
- invoke the real probe on a cold pool and assert cold, demanded=no, no dead row, no worker spawn, and no full liveness probe;
- verify recorded failures render unresponsive and include the bounded failure count.

In tests/anvil-file-test.el:

- create an oversized temporary file;
- stub the body-loading primitive;
- assert an unbounded read fails with pagination guidance before the stub runs;
- assert a bounded page still succeeds;
- assert small unbounded reads remain unchanged;
- assert disabling the configured limit retains legacy behavior.

In tests/anvil-server-test.el:

- register a handler whose result exceeds the configured inline limit;
- assert the response is bounded and omits the rejected payload;
- assert a result at the boundary succeeds;
- assert disabling the limit retains legacy behavior.

Run focused ERT during each red-green cycle, followed by make test, make test-all, make lint, and make byte-compile.

### Nix tests

Extend watchdog-test.py to cover:

- each cause classification;
- simultaneous deadline precedence;
- private file mode and stable inode handling;
- bounded schema validation;
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

1. Commit and push this approved specification in the Anvil repository.
2. Add failing Anvil tests and confirm their expected failures.
3. Implement worker reporting and bounded result behavior; run all Anvil gates.
4. Commit and push the Anvil changes.
5. Update /Users/johnw/src/nix/packages/anvil-mcp/source.nix to the pushed Anvil revision and new fixed-output hash.
6. Add failing Nix watchdog and supervisor tests, then implement the generated diagnostics.
7. Run all Anvil-MCP package tests and ./build system.
8. Commit and push only task-owned Nix files, preserving unrelated working-tree changes.
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

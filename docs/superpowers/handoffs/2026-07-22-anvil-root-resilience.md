# Anvil Root Resilience Wiggum Handoff

**Objective:** Implement and deploy the approved Anvil root resilience design.
**Mode:** Autonomous Wiggum continuation.
**Frozen design:** docs/superpowers/specs/2026-07-22-anvil-root-resilience-design.md
**Frozen plan:** docs/superpowers/plans/2026-07-22-anvil-root-resilience.md
**Current repository:** /Users/johnw/src/emacs-lisp/anvil-root-resilience
**Current branch:** fix/anvil-root-resilience
**Nix deployment repository:** /Users/johnw/src/nix
**Nix implementation worktree:** /Users/johnw/src/nix-anvil-root-resilience
**Journal:** not in use
**PAL consensus:** unavailable in the advertised tool surface

## Completed

- Diagnosed false dead worker reporting separately from real 45-second root
  watchdog restarts.
- Verified ai-nix contains no Anvil lifecycle implementation and needs no task
  change.
- Received design approval and pushed initial design commit
  4e10d7e4101610e0ca895c474b568136b11c5694.
- User selected fail-closed pagination for oversized unbounded file reads.
- Activated Wiggum autonomous continuation and drafted the test-first plan.
- First fess audit of b0bd6d7 found incorrect Darwin attributes, broken focused
  selectors, incomplete worker coverage, telemetry gaps, and publication-order
  contradictions.
- Correction commit 9634dba fixed those findings and was independently audited.
- Packaged macOS Emacs experiments proved local Unix sockets survive pathname
  unlink/rebind, omit explicit `:type 'stream`, and are close-on-exec.
- A second audit and focused contract reviews found blocking socket/file I/O,
  unauthenticated event paths, response-builder gaps, fractional-limit bypass,
  TOCTOU file loading, mutating worker probes, JSON escape expansion, stale
  watchdog attribution, unbounded probe helpers, and post-gate rebases.
- The frozen design now removes watchdog-path record files entirely: activity is
  in monitor memory and each launch uses one private nonblocking event pipe.
- The file guard now performs an authoritative maximum-plus-one-byte read, the
  generic cap counts projected JSON-string bytes across every tool error path,
  reporting uses a non-mutating endpoint observer, and the supervisor helper is
  nonblocking, bounded, and timeout-contained.
- The plan now freezes runnable packaged-Emacs commands, named red-green Python
  groups, exact support/launcher provenance, socket-path preflight, startup and
  cached-response phases, per-exit watchdog clearing, revision-bearing version
  output, and no post-gate history mutation.
- A capability-topology audit then closed the final lifecycle gaps: shared-host
  mode is explicitly compatibility-only, parent-guard containment is preserved
  through strict next-launch stale-socket recovery, and one exit finalizer now
  clears watchdog attribution after both natural and intentional daemon exits.
- Final upstream review moved the sanitizer boundary ahead of params/lazy
  loading, protected the persistent harness hook with safe condition/tool
  allowlists, added a second post-disclosure guard, and covered Emacs unibyte
  JSON escaping without materializing encoder output.
- Final cross-repository review replaced unsafe full-loader pagination with a
  chunked, byte-capped, timer-yielding page scan; suppressed semantic duplicate
  activity phases; and made terminal supervisor shutdown invalidate stale
  status on stop, finalizer, or publication failure. Both focused and whole-
  plan audits now report no Critical or Important findings.
- Created the isolated Anvil implementation worktree on
  `fix/anvil-root-resilience` from signed, pushed planning commit `41a0145`.
- Task 0's exact packaged-Emacs baseline passed `make test` 40/40. The broader
  `make test-all` selected 2,507 of 2,745 tests across 109 files and found three
  unexpected results confined to `anvil-stdio-postdispatch-test.el` and
  `anvil-stdio-readiness-test.el`; every other suite passed.
- Task 1's non-vacuous three-test RED run failed for the intended missing
  classifier and legacy blocking/mutating reporting calls. After the minimal
  worker-reporting implementation, the same selector passed 3/3.
- Task 1's full packaged-Emacs verification passed the worker suite 68/68 and
  the worker-pool suite 16/16, both with zero unexpected results.
- Task 1 is committed as `f9b2b9e`, `4f0abf4`, and `a5c400c`; lazy workers
  report cold without forcing initialization, endpoint observation is
  nonblocking, and unexpected reporting errors remain visible.
- Task 2 is committed as `2dd45f6` and `ca7b793`; unbounded whole-file reads
  fail with pagination instructions above one MiB, while paged reads stream a
  maximum-plus-one-byte window without materializing the file. Audit repair
  `b525ef6` stops immediately after a selected page overflows, and `2a4ebbf`
  proves changed-file precedence on that immediate exit.
- Task 3's generic result boundary is committed as `7712a01` and `829f262`;
  projected JSON-string bytes are capped at two MiB across success and error
  paths, request-owned properties are stripped, spoofed overflow conditions
  are sanitized, and every exposed fallback value is fresh. Audit repair
  `72b44aa` rejects malformed cap types on every path, and `2ec574f` documents
  the enabled, disabled, and invalid cases explicitly.
- Package lint compatibility is committed as `de735e3`. The latest exact file
  suite passed 68/68 before the added race case, which then passed focused;
  the latest core run passed all 51 functional cases except the inherited
  two-second offload checkpoint under scheduler starvation, and that exact
  checkpoint passed immediately in isolation.
- The inherited stdio race was fixed in signed commit `49246b0`. Runner
  publication now uses an atomic bounded heartbeat and explicit ACK custody,
  with no arbitrary acknowledgement deadline or pre-publication PID signal.
  Full Bash 3.2 readiness and postdispatch suites pass; targeted custody,
  saturation, delayed-ACK, publication, and cleanup-backlog regressions pass
  on Bash 3.2 and Bash 5.3. A final full Bash 5.3 run was scheduler-starved
  before its shell-only probe received CPU and left no child behind; no
  deadline was weakened or retry added.
- Nix Task 5 closes the five independent audit findings: enum inputs are
  type-checked, the real clean-wrapper through real-Emacs capability chain is
  proven, duplicate boundary calls emit one transition, all eleven telemetry
  ERTs are gated, and capability keys are absent from every descendant class.
  A second audit added a scalar-params telemetry guard and exact-runtime
  activity-socket preflight. The realized focused supervisor set passes 5/5,
  and a generated-init unit probe accepts object alists while rejecting scalar,
  vector, improper, and dotted inputs without changing request results.

## Current state

- The isolated upstream branch is `fix/anvil-root-resilience` at signed HEAD
  `2a4ebbf`, with its committed implementation series above the pushed PR #55
  planning head `41a0145`. It has not been pushed or opened as a PR yet.
- All implementation code paths are committed; only this handoff update is
  pending. Generated Python bytecode was removed. Dedicated Anvil reported no
  modified file buffers before both final code commits.
- The independent whole-branch review found only the immediate-overflow and
  malformed-limit issues above. Both repairs and their follow-up review
  findings are closed. The one planned rebase onto the fetched, unchanged
  `fix/issue-53-interrupted-hangs` parent remains before the clean upstream gate
  and definitive push.
- The Nix implementation worktree is `fix/anvil-root-resilience` from
  `facb6353740253d76e15d300c65b136f06a675b9`. Task 5 changes are deliberately
  uncommitted until the definitive upstream pin lands.
- The current realized Nix Anvil pin is `01eecf6`. Its focused package build
  passes clean-env 13/13 and watchdog 33/33, then expectedly stops at telemetry
  7/10 because that revision predates the three Task 3 result boundaries.
  The prior ten-test generated fixture is 10/10 against local upstream HEAD;
  the new scalar-params case will join the authoritative run after pinning.
- `/Users/johnw/src/nix` remains synchronized with `origin/main` at `facb635`;
  its untracked fractal design document is unrelated user work and must remain
  untouched.
- `/Users/johnw/src/ai-nix` remains read-only at `edae388`; its two modified
  planning/design documents are unrelated user work and must remain untouched.
- Task 6 event ingestion, terminal finalization, bounded probe summary,
  revision-bearing version, and forced attribution smoke are not implemented
  yet. No Hera switch or production smoke has occurred.
- Per user direction, the second upstream PR must be stacked on PR #55 and
  opened only after every gate and the deployed Hera proof pass.

## Stop-and-escalate counters

- Repeated failing gate signature: 0 of 3.
- Unusable subagent output: 0 of 2.
- Unresolved rebase conflict: 0.
- Requirement ambiguity: 0 active.
- Destructive action required: no.

## Resume procedure

1. Re-read the Wiggum skill, frozen design, plan, and this handoff.
2. Verify live Anvil, Nix, and ai-nix state; live artifacts override this
   snapshot.
3. Commit this handoff, rebase exactly once onto the already-fetched PR #55
   head, and rerun the clean upstream test, lint, byte-compile, and `test-all`
   gates.
4. Push `fix/anvil-root-resilience` to the fork without opening the PR. Pin that
   exact revision, archive hash, committer date, and package header version in
   the Nix worktree as the first Nix commit.
5. Commit the verified Task 5 capability/telemetry work, then implement Task 6
   event ingestion, sole exit finalization, probe summary/advice, version proof,
   and forced attribution smoke test-first.
6. Run the complete focused and full Nix gates, independently review the whole
   branch, commit/push it, and switch `.#hera` with the explicit local ai-nix
   override.
7. From a fresh client, prove deployed version, watchdog attribution, restart,
   ordinary tool use, worker liveness, and sentinel absence. Only then open the
   second Anvil PR with base `fix/issue-53-interrupted-hangs` (or retarget to
   `master` if PR #55 merged first), clean worktrees/stashes, and hand off.

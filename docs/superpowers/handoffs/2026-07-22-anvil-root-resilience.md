# Anvil Root Resilience Wiggum Handoff

**Objective:** Implement and deploy the approved Anvil root resilience design.
**Mode:** Autonomous Wiggum continuation.
**Frozen design:** docs/superpowers/specs/2026-07-22-anvil-root-resilience-design.md
**Frozen plan:** docs/superpowers/plans/2026-07-22-anvil-root-resilience.md
**Current repository:** /Users/johnw/src/emacs-lisp/anvil-root-resilience
**Current branch:** fix/anvil-root-resilience
**Nix deployment repository:** /Users/johnw/src/nix
**Nix implementation worktree:** not created yet
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

## Current state

- Task 1 implementation, tests, and documentation are present in the isolated
  worktree with its focused and full worker verification green.
- The signed planning HEAD `41a0145` is pushed and the Anvil parent checkout is
  clean.
- Dedicated Anvil reported no modified file buffers before the latest edit
  batches; this does not cover the separate interactive development Emacs.
- /Users/johnw/src/nix main is synchronized with origin/main at
  7bf56931bd00c9f546cae8e64147d825661d0da7 and currently has a user-owned
  `config/packages.nix` modification that must remain untouched.
- /Users/johnw/src/ai-nix is read-only for this task at
  0610fd1283cf5ee52a5c71cbc8411a647b37dd7c and now has unrelated user-owned
  modifications/untracked files that must remain untouched.
- Task 0's baseline is characterized. The deterministic stale post-dispatch
  source-contract assertion was repaired in signed commit `a870ddf`; the two
  longer isolated stdio failures reproduced as scheduler/order-sensitive
  inherited failures whose exact subcases pass independently. The inherited
  package-lint baseline remains to be reconciled before the final full gate.

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
3. Reverify the Task 1 audit repair after signed commits `f9b2b9e` and its
   reporting-only `:nowait` follow-up; repeat the focused and full worker gates.
4. Independently re-audit the exact Task 1 follow-up commit and resolve every
   real finding before proceeding past Task 1.
5. Reconcile the concurrently implemented Tasks 2 and 3 without mixing their
   disjoint file ownership into Task 1's commit.
6. Continue each test-first task with per-commit fess audit, partner-observation
   scan, and progress-ledger update.
7. Create Nix implementation worktree
   /Users/johnw/src/nix-anvil-root-resilience and
   preserve unrelated user state.

# OpenRouter free-model scoreboard

What each free model actually produced when the Anvil OpenRouter provider was
used to write code. Every row is one worker run that wrote at least one file.
Add a row after every run; a model's reputation here is the measured record,
not an impression.

The columns that matter:

- **reader gate** — `emacs --batch -l tools/anvil-elisp-read-check.el -- FILE`.
  PASS means the real Lisp reader read every top-level form. Nothing a worker
  writes counts as a deliverable before this passes.
- **verified itself** — whether the worker ran the verification command its
  prompt required. Prompts have made this mandatory since run 2.
- **logic** — what review or execution found once the file could be read.

## Runs

| date | model | task | file | reader gate | verified itself | logic |
| --- | --- | --- | --- | --- | --- | --- |
| 2026-09-23 | `nex-agi/nex-n2.5-pro:free` | kev-audit (read-only) | — | — | — | wall-clock timeout at 300 s, no output |
| 2026-09-23 | `nex-agi/nex-n2.5-pro:free` | kev-tests | `tests/anvil-nso-kev-test.el` | FAIL, unbalanced after 11 forms at line 48 | no | also let-bound the canned transport around the *construction* of the backend, not the call, so the tests would have reached the network |
| 2026-09-23 | `nex-agi/nex-n2.5-pro:free` | nso-cost-plan | `nso-cost.el` | PASS | no | 3 spec violations: infinity accepted (tested `eq` against `most-positive-fixnum`), required candidates filtered out before selection, `member` compared originals against `:position`-augmented copies so `:selected` was always nil. Skeleton, validation order and the non-optimality note were right |
| 2026-09-23 | `nex-agi/nex-n2.5-pro:free` | nso-cost-plan | `tests/nso-cost-test.el` | FAIL, unbalanced at line 14 | no | every `ert-deftest` missing exactly one closing paren |
| 2026-09-23 | `nex-agi/nex-n2.5-pro:free` | fix-nso-cost | `nso-cost.el` | PASS | no | fixed 1 of 3 defects; the fix for the second introduced `(void-variable selected)` by pushing to a `let*` binding declared later in the same form, breaking every call carrying `:required` — the exact acceptance case the prompt named |
| 2026-09-23 | `nex-agi/nex-n2.5-pro:free` | fix-kev-tests | `tests/anvil-nso-kev-test.el` | FAIL, `invalid-read-syntax ")"` at line 59 | no | — |
| 2026-09-23 | `poolside/laguna-s-2.1:free` | fix-nso-cost-v2 | `nso-cost.el` | FAIL, one missing paren closing a `lambda` | no (task exited 1) | **the logic was correct**: it restructured the `let*` so `selected` starts as the required candidates and removed the push from the `mapcar` lambda, which is the right shape. Only the paren was wrong |
| 2026-09-23 | `cohere/north-mini-code:free` | nso-cost-tests | `tests/nso-cost-test.el` | FAIL, unbalanced after 15 forms at line 35 | no | — |
| 2026-09-23 | `thinkingmachines/inkling:free` | kev-tests-v3 | `tests/anvil-nso-kev-test.el` | **PASS** | — | avoided all three traps the prompt named (dynamic-binding scope, answers-is-an-object, the anvil-http stub). 2 of 8 tests passed; the 6 failures came from one repeated mistake — extracting the answer as if the backend returned `nso-batch-decide`'s shape — and not from the module. Running these tests is what exposed a real defect in `anvil-nso-kev.el` |

| 2026-09-23 | `cohere/north-mini-code:free` | nso-cost-tests (2nd attempt, gate command in the prompt) | `tests/nso-cost-test.el` | FAIL | no | worse than a missing paren: three tests began with a bare `deftest` instead of `(ert-deftest`, and each was missing its closing paren. Not repairable by one edit; rewritten here |
| 2026-09-23 | `thinkingmachines/inkling:free` | kev-key-type-fix | none | — | — | 900 s wall-clock timeout with **both input files unchanged**. The model that had just produced the only passing file returned nothing on the next task |

| 2026-09-23 | `thinkingmachines/inkling:free` | nso-decide | `nso-decide.el` | **PASS** | — | **correct on the first try.** All 18 derived values match reference figures computed independently from the published formulas, all four answer shapes pass NSO's own type gate, ties resolve to the first option as the gate requires, and all three malformed inputs signal. Its validation also refused a distribution summing to 0.9991 — a real server's four-decimal output — which is the right answer and caught a fault in my test data, not in its code. The companion test file was never written; the task was still running past its wall clock |

| 2026-09-23 | `thinkingmachines/inkling:free` | nso-render | `nso-render.el` | PASS | no | read fine and did not load: called `cl-every` with no `(require 'cl-lib)`. Also treated every symbol as a nested value, so `:active t` rendered on its own indented line. The verification command in the prompt would have caught both on its first line |
| 2026-09-23 | `thinkingmachines/inkling:free` | nso-render-fix | `nso-render.el` | PASS | — | **first task today to reach `done` rather than a wall-clock timeout.** Fixed both named defects and went from not loading to 13 of 16 worked examples. The three failures were one expression: `(make-string indent ?\s)` gives one space per nesting level where the contract is two, so every nested case rendered plausibly and none rendered correctly. Fixed here, one edit |

| 2026-09-23 | `thinkingmachines/inkling:free` | nso-dates | `nso-dates.el` | FAIL, one missing paren | no | the requires were all there this time, which the previous round's feedback had asked for. Four defects behind the paren, each invisible to a reader-only check: `\{4\}` inside a string literal yields the regexp `{4}`, a literal brace, so nothing matched and every case returned the empty string; `\\(` opened groups that `\)` never closed, so the regexps did not even compile; `(member raw seen :test #'equal)` passes cl-lib keywords to the two-argument `member`; and `calendar-last-day-of-month` was called before the month range was checked, so a shape-matching non-date like 2026-13-45 indexed a twelve-element table at 13. Fixed here, then 15 of 15 captured examples matched |

| 2026-09-23 | `thinkingmachines/inkling:free` | nso-prefix | `nso-prefix.el` | PASS | no | balanced, and two defects a reader cannot see: the producer and the admit predicate were passed to `nso-cache-call` the wrong way round, so every miss rendered twice and admission was decided from a boolean's length; and the header cookie was written `--- lexical-binding: t; -*-` instead of `-*- lexical-binding: t; -*-`, so the file silently loaded under dynamic binding while its lambdas capture two locals. Both fixed here; the Commentary it wrote about what the file must not claim was good and was kept verbatim |

## What the record says so far

Eleven worker runs, nine Elisp files written, and **eight of nine failed the
reader gate**. The single pass came from `thinkingmachines/inkling:free`, which
then returned nothing at all on its next task. Not one worker ran the
verification command its prompt required, including the runs that were given
`terminal` and `file` tools and could have.

What the one passing file bought was worth the whole exercise anyway: running
those tests exposed a real defect in `anvil-nso-kev.el`, where every `choice`
and `score` answer silently abstained because the decoder looked up symbol JSON
keys with a string. `noul` reads one scalar and kept working, so the defect was
invisible from the outside.

The logic is usually better than the syntax. Two of the three modules were
structurally right and failed only on parentheses or on one repeated shape
mistake. That is worth knowing, because it means the useful work is real and
the cheap fix is a deterministic gate, not a better prompt.

`thinkingmachines/inkling:free` has now written the only two files that read,
and the second was functionally correct on the first try. Neither run produced
the test file it was also asked for. Read that as: it can write a module worth
keeping, it cannot be relied on to finish a two-file task, and the reader gate
is what makes sending it work safe rather than a gamble.

Three practical conclusions:

1. Never accept a generated Elisp file before the reader gate passes. The
   prompt cannot be trusted to make the worker check, so the checking has to
   happen here.
2. **The reader gate is necessary and nowhere near sufficient.** Across the
   modules that read cleanly, the defects that survived it were: a missing
   `require` so the file did not load at all; a nesting pad of one space where
   the contract says two, which rendered every nested case plausibly and every
   one wrongly; `\{4\}` in a string literal, which is a literal brace and not a
   repetition, so nothing matched; groups opened with `\\(` and closed with
   `\)`; and a table indexed before its bound was checked. Only executing the
   code against captured reference output finds these. Capture the reference
   output first; it is what turns a review into a measurement.
3. Prefer `thinkingmachines/inkling:free` for Elisp until another model earns a
   PASS row. Record the outcome either way.

## Work deliberately not delegated

Record these too, so the scoreboard is not quietly flattering by only counting
the tasks that were sent out.

| date | work | why it stayed here |
| --- | --- | --- |
| 2026-09-23 | the NeLisp reader fix (`scripts/nelisp-stdlib-prelude.el`, `nelisp--rd-one`) | five surgical edits inside a 12,000-line bootstrap file that the standalone binary is built from, in a repository whose own rules forbid accepting a claim that is not in a gate report. On a record of one passing Elisp file in nine, handing a worker that file risks corrupting the bootstrap to save a few minutes of typing. The edits were stated exactly before being made and verified against a control build taken before any change |
| 2026-09-23 | `tests/nso-cost-test.el`, and repairing the worker-written `tests/anvil-nso-kev-test.el` | finished here after four worker rounds produced no readable file for the first and the second came back with one repeated shape mistake. Both are transcriptions of values already measured |

## Related

- `tools/anvil-elisp-read-check.el` — the gate, and why it refuses to run under
  NeLisp.
- `docs/code-context-workflow.ja.md`, section
  「生成されたElispを本物のリーダーで検査する」.

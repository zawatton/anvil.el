;;; anvil-fusion-verify.el --- Contested-claim extraction for Fusion panels -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Phase 6a of docs/design/61-fusion-verify.org ("Verifier-grounded
;; judge").  The 2026-06-16 panel-compare eval recorded a judge that
;; discarded a correct candidate answer (0.1 MOhm) in favor of a
;; hallucinated one (100 MOhm) from a different candidate: synthesis
;; without grounding picks on prose plausibility, not verified truth.
;;
;; This module is the first half of the fix: after a fusion fan-out,
;; mine the candidate answers for *contested claims* — factual
;; assertions the candidates disagree on, or load-bearing assertions
;; only one candidate makes — so a later phase (6b, not yet built) can
;; check them against evidence before the judge synthesizes.
;;
;; A claim is a plist:
;;   (:claim STRING :kind SYMBOL :candidates (STRING...))
;; where :kind is one of `fact' / `number' / `code' / `citation'.
;;
;; Like `anvil-fusion.el', the prompt-building layer
;; (`anvil-fusion-verify--extract-prompt',
;; `anvil-fusion-verify--parse-claims') is pure and depends on nothing
;; at load time beyond `anvil-fusion' (itself load-time pure), so it is
;; unit-testable without `anvil-orchestrator'.
;; `anvil-fusion-verify-extract-claims' is the thin orchestration
;; wrapper: it lazily requires `anvil-orchestrator' and uses only its
;; public API (`anvil-orchestrator-submit', `-collect', `-status' via
;; `anvil-fusion--batch-first-task-id', `-extract-result') — the same
;; single-task submit -> wait -> extract-result pattern
;; `anvil-fusion-ask.el' uses for its judge task.
;;
;; Extraction is best-effort: on task failure or timeout this returns
;; nil (with a `message' warning) rather than signaling, so a caller
;; can treat "no claims" the same as "nothing to verify".
;;
;; Phase 6b ("evidence check per claim", docs/design/61-fusion-verify.org
;; §2) extends this module with two evidence sources fed per-claim,
;; BEFORE judge synthesis:
;;
;;   1. Local KB grep (`anvil-fusion-verify--kb-search') — greps
;;      `anvil-fusion-verify-kb-roots' for claim-derived search terms.
;;      Zero content egress, so it runs for sovereign panels too.
;;   2. An adversarial skeptic vote (`anvil-fusion-verify-claims') —
;;      N independent tasks per claim, each told to try to refute it;
;;      `anvil-fusion-verify--aggregate-verdicts' majority-decides
;;      `confirmed' / `refuted' / `unverified'.
;;
;; `anvil-fusion-verify-claims' enforces the same sovereignty
;; discipline as `anvil-fusion-ask': for a `local-only' egress request
;; it refuses (via `user-error') a non-local skeptic provider BEFORE
;; submitting anything, reusing `anvil-fusion-provider-local-p' (the
;; predicate `anvil-fusion-panels' uses to validate sovereign panels).
;; The KB grep half stays pure (no orchestrator dependency); the
;; skeptic-vote half lazily requires `anvil-orchestrator' and follows
;; the same submit -> collect(:wait) -> extract-result pattern as
;; Phase 6a, batched (all skeptic tasks for all claims submitted as
;; ONE batch).
;;
;; Phase 6d ("verdict-annotated synthesis", docs/design/61-fusion-verify.org
;; §2 6d) adds the last piece: a verified variant of
;; `anvil-fusion-judge-template' (`anvil-fusion-verify-judge-template')
;; that instructs the judge how to read the Phase 6b verdict table and
;; forbids adopting a REFUTED claim, plus the pure renderer/builder
;; (`anvil-fusion-verify--format-claims-block',
;; `anvil-fusion-verify-judge-template-for') that turns annotated
;; claims into a ready-to-use :template for the EXISTING, UNCHANGED
;; `anvil-fusion-judge-consensus' / `anvil-fusion-build-judge-prompt'
;; (anvil-fusion.el).  The seam that keeps anvil-fusion.el untouched is
;; a literal `{{CLAIMS}}' marker rather than a third %s slot -- see
;; `anvil-fusion-verify-judge-template''s docstring for the full
;; rationale, including why the rendered block must have its `%'
;; characters doubled before substitution.

;;; Code:

(require 'cl-lib)
(require 'anvil-fusion)
(require 'anvil-fusion-panels)

;; Public anvil-orchestrator functions used only at call time.  Declared
;; (not required) so the pure prompt layer loads without dragging in the
;; whole orchestrator stack -- mirrors `anvil-fusion.el' / `anvil-fusion-ask.el'.
(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-collect "anvil-orchestrator" (batch-id &rest _))
(declare-function anvil-orchestrator-status "anvil-orchestrator" (id))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))
(declare-function anvil-state-get "anvil-state" (key &rest args))
(declare-function anvil-state-set "anvil-state" (key val &rest args))
(declare-function anvil-state-delete "anvil-state" (key &rest args))

(defgroup anvil-fusion nil
  "Fusion judge harness layered over anvil-orchestrator."
  :group 'tools
  :prefix "anvil-fusion-")

;;;; --- customization -------------------------------------------------------

(defcustom anvil-fusion-verify-max-claims 5
  "Maximum number of contested claims to extract per fan-out.
Only the claims most likely to change the final answer should be
kept (most load-bearing first); this both caps verification cost
in Phase 6b and clamps `anvil-fusion-verify--parse-claims' output."
  :type 'integer
  :group 'anvil-fusion)

(defcustom anvil-fusion-verify-extract-provider 'claude
  "Provider used for the claim-extraction pass.
Doc 61 §9 leaves haiku-vs-sonnet extraction fidelity an open
question; `claude' is the safe default (matches
`anvil-fusion-judge-default-provider').  Override per call with
the :provider argument to `anvil-fusion-verify-extract-claims'."
  :type 'symbol
  :group 'anvil-fusion)

(defcustom anvil-fusion-verify-extract-model "haiku"
  "Model for the claim-extraction pass, applied ONLY when the effective
provider is `claude'.
Doc 61 §9's haiku-vs-sonnet fidelity spike
(benchmarks/doc61-phase6a/, 2026-07-03) measured haiku-tier
(`claude-haiku-4-5-20251001') against sonnet-tier on 8 synthetic
fan-out cases: 79% gold recall vs sonnet's 86%, zero format collapse
on either tier (8/8 well-formed responses each), and haiku at ~0.6x
sonnet's cost.  The spike's Recommendation pins haiku as the default
extraction tier -- the 7-point recall gap is within the ~10% bar
Doc 61 §9 set as the pinning criterion, and sonnet's own worst
failure (a false NONE, case 8) is arguably worse for Phase 6b than
haiku's one-sided disagreement merges, which at least surface the
contested topic.
This is a claude-CLI model alias (\"haiku\"), so it is applied ONLY
when the effective provider resolves to `claude' -- see
`anvil-fusion-verify-extract-claims' for the exact provider-scoped
resolution logic (an alias like this would break e.g. an `ollama'
provider).  Set to nil to let the claude CLI pick its own default
model instead.  Override per call via the :model argument to
`anvil-fusion-verify-extract-claims'."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-fusion)

(defcustom anvil-fusion-verify-extract-template
  "あなたは、複数の AI 候補回答から「係争中の主張」を洗い出す監査役です。
係争中の主張とは、(a) 候補間で内容が食い違っている主張、または
(b) 単一の候補のみが述べている、結論を左右し得る重要な主張です。

# 原問
%s

# 候補回答
%s

# 指示
- 上記の基準に従い、係争中の主張を、結論への影響が大きい順に列挙してください。
- 候補間で見解が分かれている論点は、一つの CLAIM 行にまとめず、立場ごとに別々の CLAIM 行として出力してください。
- 各 CLAIM 行の CANDIDATES には、その立場を実際に述べた候補だけを列挙し、他の立場の候補を含めないでください（帰属は正確に）。
- 同じ論点の両側は隣接する行にまとめ、両方とも上限件数の中に数えてください（片側だけの論点を多く挙げるより、少ない論点を両側とも漏れなく挙げることを優先してください）。
- NONE の判定は厳格に行ってください。全候補が実質的に同一の主張をしている場合に限り NONE としてください。言い回しの違い・説明の詳しさや網羅性の差・周辺的な論点への言及の有無は、係争中の主張とはみなしません。結論を左右しない周辺的な差異についての CLAIM は出力しないでください。
- 最大 %d 件まで。
- 係争中の主張が一つも無ければ、他には何も書かず NONE とだけ出力してください。
- 各主張は 1 行、以下の形式に厳密に従ってください（番号付けや装飾、余計な前置きは禁止）:
CLAIM: <一文で自己完結した主張> | KIND: <fact|number|code|citation> | CANDIDATES: <name1,name2>"
  "Extraction prompt template for the claim-mining pass (Doc 61 Phase 6a).
Three placeholders filled in order by `format' via
`anvil-fusion-verify--extract-prompt': (1) the original question,
(2) the formatted candidate block (see
`anvil-fusion-verify--format-candidates'), (3)
`anvil-fusion-verify-max-claims'.  The exact `CLAIM: ... | KIND:
... | CANDIDATES: ...' line format is required by
`anvil-fusion-verify--parse-claims'; keep the tail of the template
in sync with the parser's regexp if you customize it.

Hardened per the Doc 61 §9 haiku-vs-sonnet spike
(benchmarks/doc61-phase6a/results-extract-fidelity-2026-07-03.org)
against two failure modes both tiers showed: (1) collapsing a
two-sided disagreement into one CLAIM line, sometimes with wrong
CANDIDATES attribution -- fixed by the per-side-extraction
instruction (each contested position gets its own CLAIM line, with
CANDIDATES restricted to the candidates that actually asserted that
position); (2) 2-3 false-positive CLAIMs on an all-agreeing panel --
fixed by tightening the NONE criterion to require substantively
identical assertions (wording / coverage / peripheral-point
differences no longer qualify as contested)."
  :type 'string
  :group 'anvil-fusion)

;;;; --- pure prompt-building layer (no orchestrator load needed) ------------

(defun anvil-fusion-verify--candidate-label (candidate index)
  "Return a stable label for CANDIDATE at zero-based INDEX.
Prefers the task's :name (assigned by `anvil-fusion-panel-tasks',
e.g. \"fusion-member-0-claude\", and preserved on the slim task
plist returned by `anvil-orchestrator-status' /
`anvil-orchestrator-collect'); falls back to an index + provider
label when :name is absent (e.g. hand-built candidate plists in
tests or callers)."
  (let ((name (plist-get candidate :name)))
    (if (and (stringp name) (not (string-empty-p name)))
        name
      (format "candidate-%d-%s" (1+ index) (or (plist-get candidate :provider) "?")))))

(defun anvil-fusion-verify--format-candidates (candidates)
  "Format CANDIDATES (slim task plist list) into a labeled block.
CANDIDATES has the same shape `anvil-fusion.el' already consumes
for its judge prompt: slim task plists with keys :id :name
:provider :status :summary :error (see
`anvil-orchestrator--task-summary-plist').  Each candidate's answer
text is fetched via `anvil-fusion--candidate-text' (summary
fidelity — the extraction pass only needs to see what the judge
would see by default).  Returns \"(no candidates)\" when empty."
  (if (null candidates)
      "(no candidates)"
    (let ((i -1))
      (mapconcat
       (lambda (c)
         (setq i (1+ i))
         (format "### %s\n%s"
                 (anvil-fusion-verify--candidate-label c i)
                 (anvil-fusion--candidate-text c 'summary)))
       candidates
       "\n\n"))))

(defun anvil-fusion-verify--extract-prompt (question candidates)
  "Build the claim-extraction prompt for QUESTION and CANDIDATES.
QUESTION is the original prompt the panel answered.  CANDIDATES is
the slim task plist list (see `anvil-fusion-verify--format-candidates').
Pure function — safe to call without `anvil-orchestrator' loaded."
  (format anvil-fusion-verify-extract-template
          (or question "")
          (anvil-fusion-verify--format-candidates candidates)
          anvil-fusion-verify-max-claims))

;;;; --- tolerant claim-line parser -------------------------------------------

(defconst anvil-fusion-verify--claim-line-regexp
  (concat "\\`claim[ \t]*:[ \t]*\\(.*?\\)[ \t]*"
          "|[ \t]*kind[ \t]*:[ \t]*\\(.*?\\)[ \t]*"
          "|[ \t]*candidates[ \t]*:[ \t]*\\(.*\\)\\'")
  "Regexp matching one well-formed claim line, after bullet/number
stripping.  Matching is done case-insensitively (see
`anvil-fusion-verify--parse-claim-line') so `Claim:' / `CLAIM:' /
`claim:' label variants all parse.  Capture groups: 1 = claim
text, 2 = kind text, 3 = raw comma-separated candidate names.")

(defconst anvil-fusion-verify--known-kinds '("fact" "number" "code" "citation")
  "Valid (lowercase) :kind values for a parsed claim.")

(defun anvil-fusion-verify--strip-bullet (line)
  "Strip a leading bullet/number marker and surrounding whitespace from LINE."
  (string-trim
   (replace-regexp-in-string
    "\\`[ \t]*\\(?:[-*•][ \t]*\\|[0-9]+[.)][ \t]*\\)?" "" line)))

(defun anvil-fusion-verify--parse-claim-line (line)
  "Parse one raw LINE into a claim plist, or nil when malformed.
Tolerates leading whitespace/bullets/numbering and case variation
in the `CLAIM:' / `KIND:' / `CANDIDATES:' labels.  An unrecognized
KIND value normalizes to `fact'.  A line whose claim text is empty
after trimming returns nil (the entry is dropped)."
  (let* ((stripped (anvil-fusion-verify--strip-bullet line))
         (case-fold-search t))
    (when (string-match anvil-fusion-verify--claim-line-regexp stripped)
      (let* ((claim    (string-trim (match-string 1 stripped)))
             (kind-raw (downcase (string-trim (match-string 2 stripped))))
             (kind     (if (member kind-raw anvil-fusion-verify--known-kinds)
                           (intern kind-raw)
                         'fact))
             (cands    (split-string (match-string 3 stripped) "," t "[ \t]+")))
        (unless (string-empty-p claim)
          (list :claim claim :kind kind :candidates cands))))))

(defun anvil-fusion-verify--parse-claims (text)
  "Parse TEXT (raw extraction-pass output) into a list of claim plists.
Scans TEXT line by line via `anvil-fusion-verify--parse-claim-line';
malformed lines are skipped silently.  Returns nil when TEXT is nil,
blank, or the literal (case-insensitive) `NONE' sentinel, or when no
line matched.  Otherwise the parsed claims are clamped to
`anvil-fusion-verify-max-claims', preserving order (most load-bearing
first, per the extraction prompt's instruction)."
  (when (and (stringp text) (not (string-empty-p (string-trim text))))
    (let* ((trimmed (string-trim text))
           (case-fold-search t))
      (unless (string-match-p "\\`none\\'" trimmed)
        (let* ((lines  (split-string text "\n"))
               (claims (delq nil (mapcar #'anvil-fusion-verify--parse-claim-line lines))))
          (when claims
            (let ((n (max 0 (min (length claims) anvil-fusion-verify-max-claims))))
              (if (= n (length claims)) claims (cl-subseq claims 0 n)))))))))

;;;; --- orchestration wrapper (lazy require of anvil-orchestrator) ----------

(cl-defun anvil-fusion-verify-extract-claims
    (question candidates &key provider model timeout-sec (max-wait-sec 1800))
  "Extract contested claims from CANDIDATES' answers to QUESTION.

Builds the extraction prompt (`anvil-fusion-verify--extract-prompt')
and submits it as a single task via the same orchestrator public-API
pattern as `anvil-fusion-ask.el': `anvil-orchestrator-submit' with one
task, `anvil-orchestrator-collect' with :wait t, then
`anvil-orchestrator-extract-result' (full) on the task id (obtained
via `anvil-fusion--batch-first-task-id').  :PROVIDER defaults to
`anvil-fusion-verify-extract-provider'.  :MODEL resolution is
provider-scoped: an explicit :MODEL argument always wins; otherwise
`anvil-fusion-verify-extract-model' is applied ONLY when the
effective PROVIDER (:PROVIDER, or its default, above) is `claude' --
that default is a claude-CLI model alias (e.g. \"haiku\") and would
break a non-claude provider such as `ollama'; any other provider
gets no :model unless :MODEL was passed explicitly.  :TIMEOUT-SEC
caps the task; :MAX-WAIT-SEC caps the collect wait (default 1800s).

Extraction is best-effort: on task failure, a non-`done' terminal
status, or any signaled error, this returns nil and emits a
`message' warning instead of signaling — later phases treat \"no
claims\" the same as \"skip verification\".  Returns the parsed
claim list (see `anvil-fusion-verify--parse-claims') on success."
  (require 'anvil-orchestrator)
  (condition-case err
      (let* ((prov   (or provider anvil-fusion-verify-extract-provider))
             (mdl    (or model
                         (and (eq prov 'claude) anvil-fusion-verify-extract-model)))
             (prompt (anvil-fusion-verify--extract-prompt question candidates))
             (task   (append
                      (list :name "fusion-verify-extract"
                            :provider prov
                            :prompt prompt)
                      (and mdl (list :model mdl))
                      (and timeout-sec (list :timeout-sec timeout-sec))))
             (batch  (anvil-orchestrator-submit (list task))))
        (anvil-orchestrator-collect batch :wait t :max-wait-sec max-wait-sec)
        (let* ((tid    (anvil-fusion--batch-first-task-id batch))
               (result (anvil-orchestrator-extract-result tid t))
               (status (plist-get result :status)))
          (if (not (eq status 'done))
              (progn
                (message "anvil-fusion-verify: claim extraction task %s ended %S (%s)"
                         tid status (or (plist-get result :error) "no error detail"))
                nil)
            (anvil-fusion-verify--parse-claims (plist-get result :summary)))))
    (error
     (message "anvil-fusion-verify: claim extraction failed: %s"
              (error-message-string err))
     nil)))

;;;; ============================================================
;;;; Phase 6b — evidence check per claim
;;;; ============================================================

;;;; --- customization (6b) ---------------------------------------------------

(defcustom anvil-fusion-verify-skeptics 2
  "Number of adversarial skeptic tasks fanned out per claim.
Doc 61 §2 (6b) notes 3 skeptics for a \"rigorous\" verification pass;
2 is the default cost/signal balance.  Override per call with the
:skeptics argument to `anvil-fusion-verify-claims'."
  :type 'integer
  :group 'anvil-fusion)

(defcustom anvil-fusion-verify-skeptic-provider 'claude
  "Default provider for the Phase 6b adversarial skeptic vote.
Override per call with the :provider argument to
`anvil-fusion-verify-claims' — sovereign / local-only flows pass a
provider satisfying `anvil-fusion-provider-local-p'."
  :type 'symbol
  :group 'anvil-fusion)

(defcustom anvil-fusion-verify-skeptic-model nil
  "Model for the Phase 6b skeptic vote, or nil for the provider default.
Override per call with the :model argument to
`anvil-fusion-verify-claims'."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-fusion)

(defcustom anvil-fusion-verify-kb-roots nil
  "Directories to grep for local KB evidence (Phase 6b).
Nil (the default) disables the KB backend entirely — this module
ships with no personal paths baked in (anvil.el is OSS-bound); wire
your own knowledge-base roots (org / notes / docs directories) in
your init.  See `anvil-fusion-verify--kb-search'."
  :type '(repeat directory)
  :group 'anvil-fusion)

(defcustom anvil-fusion-verify-cache-ttl-sec 604800
  "Seconds a decisive verification verdict stays reusable locally.
The cache is strictly local storage (`anvil-state' when available,
else an in-session hash table fallback), so reads and writes are
allowed even under EGRESS `local-only'.  Nil disables the cache
entirely for both reads and writes, restoring the pre-cache behavior.
Only skeptic-aggregated decisive verdicts are cached; mechanical
checker confirmations stay uncached because they are already cheap and
always fresh."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'anvil-fusion)

(defcustom anvil-fusion-verify-max-evidence 3
  "Maximum KB snippets kept as evidence per claim (Phase 6b).
Caps both what `anvil-fusion-verify--kb-search' returns and what is
fed into the skeptic prompt's evidence block."
  :type 'integer
  :group 'anvil-fusion)

(defvar anvil-fusion-verify--cache-now-fn #'float-time
  "Function returning the current unix timestamp as a float.
Tests bind this to a fake clock so cache expiry can be exercised
without sleeping.")

(defvar anvil-fusion-verify--cache-session-store (make-hash-table :test 'equal)
  "Session-local fallback verdict cache used when `anvil-state' is unavailable.")

(defconst anvil-fusion-verify--cache-namespace "fusion-verify-cache"
  "Namespace used for verified-claim cache entries in `anvil-state'.")

(defcustom anvil-fusion-verify-mechanical-checkers nil
  "List of best-effort mechanical claim checkers.
Each function is called as (FN CLAIM-PLIST) before the skeptic batch is
built and must return one of:

  nil
    The checker does not apply to this claim.
  (:verdict confirmed :evidence STRING)
    A decisive positive result; the claim is annotated confirmed
    immediately and excluded from the skeptic batch.
  (:evidence STRING)
    A non-decisive evidence hint injected into the skeptic prompt as a
    synthetic evidence item with source \"mechanical\".

Checkers are strictly additive: they must never signal and must never
return :verdict `refuted' or `unverified'.  Mechanical checks may only
confirm or hint; anything negative is left to the skeptic vote, which
can judge the hint in context."
  :type '(repeat function)
  :group 'anvil-fusion)

(defvar anvil-fusion-verify-repo-root nil
  "Dynamic repository root used by repo-reality mechanical checks.
When nil, `anvil-fusion-verify-checker-repo-reality' is inert and
returns nil.")
(defcustom anvil-fusion-verify-skeptic-template
  "あなたは、AI 候補回答から抽出された主張を検証する懐疑的な検証者です。
与えられた主張を鵜呑みにせず、積極的に反証を試みてください。

# 原問（文脈）
%s

# 検証対象の主張
%s

# 証拠（ローカル KB 検索結果。file:line 付き。無ければ「(証拠なし)」）
%s

# 指示
- この主張を反証できないか、批判的に検討してください。
- 判定は厳格に行うこと。確信が持てない場合は CONFIRMED ではなく REFUTED 寄りに
  判定する。
- CONFIRMED は、証拠または確実な知識により積極的に裏付けられる場合のみ選ぶこと。
- 主張の真偽そのものが根拠不足で判断不能な場合に限り UNVERIFIED とする。

# 出力形式
以下の 2 行のみを、この形式に厳密に従って出力してください（前置き・後書き・
追加の説明は一切禁止）:
VERDICT: <CONFIRMED か REFUTED か UNVERIFIED のいずれか一語>
REASON: <一行の理由>"
  "Skeptic-vote prompt template (Doc 61 Phase 6b).
Three %s placeholders filled in order by `format' via
`anvil-fusion-verify--skeptic-prompt': (1) the original question,
(2) the claim text, (3) the formatted KB evidence block (see
`anvil-fusion-verify--format-evidence').  The exact two-line
`VERDICT: ...' / `REASON: ...' output format is required by
`anvil-fusion-verify--parse-verdict'; keep the tail of the template
in sync with the parser's regexps if you customize it."
  :type 'string
  :group 'anvil-fusion)

;;;; --- pure heuristic term extraction (6b) ----------------------------------

(defconst anvil-fusion-verify--max-search-terms 8
  "Hard cap on the number of terms `anvil-fusion-verify--claim-search-terms'
returns for one claim.")

(defconst anvil-fusion-verify--search-term-regexp
  (concat "[0-9]+\\(?:\\.[0-9]+\\)?[A-Za-zΩµμ℃°%]*"  ; (a) number [+ unit]
          "\\|[ァ-ヶー]\\{2,\\}"                        ; (b) katakana run
          "\\|[一-鿿々]\\{2,\\}"                        ; (b) kanji run
          "\\|[A-Za-z][A-Za-z0-9_-]\\{2,\\}")           ; (c) ASCII word, len>=3
  "Alternation matching one KB search term at a time.
Mirrors `anvil-semantic--term-regexp''s katakana/kanji ranges.  Order
matters: a digit-led match tries the number[+unit] branch first, so
e.g. \"150V\" or \"0.1MΩ\" is captured whole rather than split.")

(defun anvil-fusion-verify--claim-search-terms (claim-text)
  "Extract up to `anvil-fusion-verify--max-search-terms' KB search terms
from CLAIM-TEXT.  Collects, in order of first appearance:
  (a) numbers optionally followed by (space-free) unit characters,
      e.g. \"0.1MΩ\", \"150V\";
  (b) katakana or kanji runs of length >= 2;
  (c) ASCII word runs of length >= 3.
Deduplicated (case-sensitive, first occurrence wins), capped at 8.
Pure — no I/O.  Nil/empty CLAIM-TEXT yields nil."
  (let ((acc nil) (start 0) (text (or claim-text "")))
    (while (string-match anvil-fusion-verify--search-term-regexp text start)
      (cl-pushnew (match-string 0 text) acc :test #'string=)
      (setq start (match-end 0)))
    (let ((ordered (nreverse acc)))
      (if (> (length ordered) anvil-fusion-verify--max-search-terms)
          (cl-subseq ordered 0 anvil-fusion-verify--max-search-terms)
        ordered))))

;;;; --- local KB grep backend (6b, zero egress) ------------------------------

(defun anvil-fusion-verify--kb-grep-program ()
  "Return \"rg\" when ripgrep is on PATH, else \"grep\"."
  (if (executable-find "rg") "rg" "grep"))

(defun anvil-fusion-verify--truncate-evidence (text)
  "Trim TEXT and clamp it to ~200 chars for use as an evidence snippet."
  (let ((s (string-trim (or text ""))))
    (if (> (length s) 200) (concat (substring s 0 200) "…") s)))

(defun anvil-fusion-verify--kb-search-term (root term program)
  "Fixed-string-grep ROOT for TERM, restricted to *.org/*.md/*.txt.
PROGRAM is \"rg\" or \"grep\" (see `anvil-fusion-verify--kb-grep-program').
Returns a list of (:source \"PATH:LINE\" :text SNIPPET) plists in the
order the tool reported them, or nil on no matches / any process
error — this never signals, matching the KB backend's \"must not
error\" contract."
  (condition-case nil
      (with-temp-buffer
        (let ((args (if (string= program "rg")
                        (list "-n" "-F" "--no-heading"
                              "-g" "*.org" "-g" "*.md" "-g" "*.txt"
                              "--" term root)
                      (list "-r" "-n" "-F"
                            "--include=*.org" "--include=*.md" "--include=*.txt"
                            "--" term root))))
          (apply #'call-process program nil t nil args)
          (goto-char (point-min))
          (let (hits)
            (while (re-search-forward "^\\(.*?\\):\\([0-9]+\\):\\(.*\\)$" nil t)
              (push (list :source (format "%s:%s" (match-string 1) (match-string 2))
                          :text (anvil-fusion-verify--truncate-evidence (match-string 3)))
                    hits))
            (nreverse hits))))
    (error nil)))

(defun anvil-fusion-verify--kb-search (claim-text &optional roots)
  "Search local KB ROOTS for evidence relevant to CLAIM-TEXT.
ROOTS defaults to `anvil-fusion-verify-kb-roots'; nil (the default)
disables the KB backend entirely and this returns nil immediately —
zero I/O, zero egress.  Otherwise: extract search terms via
`anvil-fusion-verify--claim-search-terms', grep each existing
directory in ROOTS for each term (ripgrep when available, else
`grep -rn', both restricted to *.org/*.md/*.txt), rank the matching
lines by how many DISTINCT terms they hit (descending; ties broken
by first-seen order for determinism regardless of `sort' stability),
and return up to `anvil-fusion-verify-max-evidence' hits as
\(:source \"PATH:LINE\" :text SNIPPET) plists.  A nonexistent /
non-directory root is skipped silently.  Never signals."
  (let ((rs (or roots anvil-fusion-verify-kb-roots)))
    (when rs
      (let ((terms (anvil-fusion-verify--claim-search-terms claim-text)))
        (when terms
          (let ((program (anvil-fusion-verify--kb-grep-program))
                (table (make-hash-table :test 'equal))
                (order nil)
                (idx 0))
            (dolist (root rs)
              (when (and (stringp root) (file-directory-p root))
                (dolist (term terms)
                  (dolist (hit (anvil-fusion-verify--kb-search-term root term program))
                    (let* ((src (plist-get hit :source))
                           (entry (gethash src table)))
                      (if entry
                          (unless (member term (plist-get entry :terms))
                            (plist-put entry :terms (cons term (plist-get entry :terms))))
                        (setq idx (1+ idx))
                        (puthash src (list :source src :text (plist-get hit :text)
                                            :terms (list term) :order idx)
                                 table)
                        (push src order)))))))
            (let* ((entries (mapcar (lambda (s) (gethash s table)) (nreverse order)))
                   (ranked (sort entries
                                 (lambda (a b)
                                   (let ((la (length (plist-get a :terms)))
                                         (lb (length (plist-get b :terms))))
                                     (if (= la lb)
                                         (< (plist-get a :order) (plist-get b :order))
                                       (> la lb)))))))
              (mapcar (lambda (e) (list :source (plist-get e :source) :text (plist-get e :text)))
                      (cl-subseq ranked 0 (min (length ranked)
                                                anvil-fusion-verify-max-evidence))))))))))

(defconst anvil-fusion-verify--repo-path-token-regexp
  "\\(?:[[:alnum:]_.-]+/\\)+[[:alnum:]_.-]+\\.\\(?:el\\|org\\|md\\|txt\\|py\\|ts\\|js\\|json\\|sh\\|yml\\)\\b"
  "Regexp matching path-like claim tokens for repo-reality checks.")

(defconst anvil-fusion-verify--repo-symbol-token-regexp
  "`\\([a-z][a-z0-9]*\\(?:-[a-z0-9]+\\)+\\)`\\|\\b\\([a-z][a-z0-9]*\\(?:-[a-z0-9]+\\)+\\)\\b"
  "Regexp matching Elisp-looking identifier tokens for repo-reality checks.")

(defun anvil-fusion-verify--collect-regexp-matches (regexp text)
  "Collect unique REGEXP matches from TEXT in first-seen order.
When REGEXP has capture groups, the first non-empty capture is used;
otherwise the full match is used."
  (let ((start 0)
        (source (or text ""))
        (acc nil))
    (while (string-match regexp source start)
      (let ((token (or (match-string 1 source)
                       (match-string 2 source)
                       (match-string 0 source))))
        (unless (member token acc)
          (setq acc (append acc (list token)))))
      (setq start (match-end 0)))
    acc))

(defun anvil-fusion-verify--repo-grep-first-fixed (root term program globs)
  "Return the first fixed-string grep hit for TERM under ROOT.
PROGRAM is \"rg\" or \"grep\".  GLOBS is the filename-glob list to
search.  Returns a \"PATH:LINE\" string or nil, and never signals."
  (condition-case nil
      (with-temp-buffer
        (let ((args (if (string= program "rg")
                        (append (list "-n" "-F" "--no-heading")
                                (cl-mapcan (lambda (glob) (list "-g" glob)) globs)
                                (list "--" term root))
                      (append (list "-r" "-n" "-F")
                              (mapcar (lambda (glob)
                                        (concat "--include=" glob))
                                      globs)
                              (list "--" term root)))))
          (apply #'call-process program nil t nil args)
          (goto-char (point-min))
          (when (re-search-forward "^\\(.*?\\):\\([0-9]+\\):" nil t)
            (format "%s:%s" (match-string 1) (match-string 2)))))
    (error nil)))

(defun anvil-fusion-verify--repo-symbol-hit (root token program)
  "Return the first definition location for TOKEN under ROOT, or nil.
Searches *.el files using fixed-string probes for common definition
forms.  PROGRAM is \"rg\" or \"grep\"."
  (catch 'hit
    (dolist (prefix '("(defun " "(defmacro " "(defvar " "(defcustom "
                      "(defconst " "(cl-defun "))
      (let ((loc (anvil-fusion-verify--repo-grep-first-fixed
                  root (concat prefix token) program '("*.el"))))
        (when loc
          (throw 'hit loc))))
    nil))
;;;; --- skeptic prompt + tolerant verdict parser (6b) ------------------------

(defun anvil-fusion-verify--format-evidence (evidence)
  "Format EVIDENCE (a `anvil-fusion-verify--kb-search' result) into a
block for the skeptic prompt.  Returns \"(証拠なし)\" when empty."
  (if (null evidence)
      "(証拠なし)"
    (mapconcat (lambda (e) (format "- %s — %s"
                                   (plist-get e :source) (plist-get e :text)))
               evidence "\n")))

(defun anvil-fusion-verify--skeptic-prompt (question claim evidence)
  "Build one skeptic-vote prompt for CLAIM.
QUESTION is the original question the claim's candidates answered
(context).  EVIDENCE is a `anvil-fusion-verify--kb-search' result (or
nil).  Pure — safe to call without `anvil-orchestrator' loaded."
  (format anvil-fusion-verify-skeptic-template
          (or question "") (or claim "")
          (anvil-fusion-verify--format-evidence evidence)))

(defconst anvil-fusion-verify--verdict-line-regexp
  "verdict[ \t]*:[ \t]*\\(confirmed\\|refuted\\|unverified\\)"
  "Matches a `VERDICT: ...' label + value, case-insensitively.
See `anvil-fusion-verify--parse-verdict'.")

(defconst anvil-fusion-verify--reason-line-regexp
  "reason[ \t]*:[ \t]*\\(.*\\)"
  "Matches a `REASON: ...' label + one-line text, case-insensitively.
See `anvil-fusion-verify--parse-verdict'.")

(defun anvil-fusion-verify--parse-verdict (text)
  "Tolerantly parse TEXT (raw skeptic-task output) into a verdict plist.
Searches TEXT for `VERDICT: CONFIRMED|REFUTED|UNVERIFIED' and
`REASON: ...' labels anywhere (case-insensitive), tolerating
surrounding prose and label-case variation.  An unrecognized or
absent VERDICT normalizes to `unverified'; an absent REASON
normalizes to \"\".  Returns (:verdict SYM :reason STR).  Never
signals — nil/non-string TEXT also yields the unverified default."
  (let ((case-fold-search t)
        (verdict 'unverified)
        (reason ""))
    (when (stringp text)
      (when (string-match anvil-fusion-verify--verdict-line-regexp text)
        (setq verdict (intern (downcase (match-string 1 text)))))
      (when (string-match anvil-fusion-verify--reason-line-regexp text)
        (setq reason (string-trim (match-string 1 text)))))
    (list :verdict verdict :reason reason)))

(defun anvil-fusion-verify--aggregate-verdicts (verdicts)
  "Majority-aggregate VERDICTS (a list of (:verdict SYM :reason STR)).
A strict majority (> half) of `refuted' votes wins as `refuted'; else
a strict majority of `confirmed' votes wins as `confirmed'; anything
else — a tie, all-`unverified', or an empty list — yields
`unverified'.  Returns (:verdict SYM :reason STR), REASON being the
first reason from the winning side (list order preserved), or \"\"
when there is no winning side.  Pure."
  (let* ((n (length verdicts))
         (refuted (cl-remove-if-not
                   (lambda (v) (eq (plist-get v :verdict) 'refuted)) verdicts))
         (confirmed (cl-remove-if-not
                     (lambda (v) (eq (plist-get v :verdict) 'confirmed)) verdicts)))
    (cond
     ((zerop n) (list :verdict 'unverified :reason ""))
     ((> (length refuted) (/ n 2))
      (list :verdict 'refuted :reason (or (plist-get (car refuted) :reason) "")))
     ((> (length confirmed) (/ n 2))
      (list :verdict 'confirmed :reason (or (plist-get (car confirmed) :reason) "")))
     (t (list :verdict 'unverified :reason "")))))

(defun anvil-fusion-verify--cache-enabled-p ()
  "Return non-nil when the verified-claim cache is enabled."
  (numberp anvil-fusion-verify-cache-ttl-sec))

(defun anvil-fusion-verify--cache-normalize-text (text)
  "Normalize claim TEXT for cache keys.
Trims leading/trailing whitespace and collapses internal whitespace
runs to a single ASCII space."
  (replace-regexp-in-string "\\s-+" " " (string-trim (or text ""))))

(defun anvil-fusion-verify--cache-key (claim)
  "Return the cache key for CLAIM.
The key is the sha1 of normalized claim text, a NUL separator, and the
claim kind symbol name.  Whitespace-only text differences therefore
collide, while different claim kinds do not."
  (secure-hash
   'sha1
   (concat (anvil-fusion-verify--cache-normalize-text (plist-get claim :claim))
           "\0"
           (symbol-name (or (plist-get claim :kind) 'fact)))))

(defun anvil-fusion-verify--cache-kb-stamp ()
  "Return the current KB-root freshness stamp, or nil.
This is the maximum directory mtime across
`anvil-fusion-verify-kb-roots' themselves, non-recursively.  This is
only a best-effort invalidation signal: content edits that do not
change a root directory's own mtime are not detected."
  (when anvil-fusion-verify-kb-roots
    (let ((stamp nil))
      (dolist (root anvil-fusion-verify-kb-roots)
        (when (and (stringp root) (file-directory-p root))
          (let ((mtime (condition-case nil
                           (float-time
                            (file-attribute-modification-time
                             (file-attributes root)))
                         (error nil))))
            (when (numberp mtime)
              (setq stamp (if stamp (max stamp mtime) mtime))))))
      stamp)))

(defun anvil-fusion-verify--cache-delete-raw (key)
  "Delete cached KEY best-effort from the active backend."
  (condition-case nil
      (if (require 'anvil-state nil t)
          (anvil-state-delete key :ns anvil-fusion-verify--cache-namespace)
        (remhash key anvil-fusion-verify--cache-session-store))
    (error
     (remhash key anvil-fusion-verify--cache-session-store)
     nil)))

(defun anvil-fusion-verify--cache-read-raw (key)
  "Read cached KEY from the active backend, or nil on any error."
  (condition-case nil
      (if (require 'anvil-state nil t)
          (anvil-state-get key :ns anvil-fusion-verify--cache-namespace)
        (gethash key anvil-fusion-verify--cache-session-store))
    (error
     (gethash key anvil-fusion-verify--cache-session-store))))

(defun anvil-fusion-verify--cache-write-raw (key value)
  "Write VALUE under cached KEY best-effort to the active backend."
  (condition-case nil
      (if (require 'anvil-state nil t)
          (anvil-state-set key value :ns anvil-fusion-verify--cache-namespace)
        (puthash key value anvil-fusion-verify--cache-session-store))
    (error
     (puthash key value anvil-fusion-verify--cache-session-store)
     nil)))

(defun anvil-fusion-verify--cache-get (claim)
  "Return a cached decisive verdict plist for CLAIM, or nil.
Nil is returned when the cache is disabled, the entry is absent,
expired, stale against the current KB stamp, malformed, or any backend
error occurs.  Expired/stale entries are deleted best-effort."
  (when (anvil-fusion-verify--cache-enabled-p)
    (condition-case nil
        (let* ((key (anvil-fusion-verify--cache-key claim))
               (value (anvil-fusion-verify--cache-read-raw key)))
          (when (and (listp value)
                     (memq (plist-get value :verdict) '(confirmed refuted))
                     (stringp (plist-get value :evidence))
                     (numberp (plist-get value :ts)))
            (let* ((now (funcall anvil-fusion-verify--cache-now-fn))
                   (age (- now (plist-get value :ts)))
                   (stored-stamp (plist-get value :kb-stamp))
                   (current-stamp (anvil-fusion-verify--cache-kb-stamp)))
              (cond
               ((> age anvil-fusion-verify-cache-ttl-sec)
                (anvil-fusion-verify--cache-delete-raw key)
                nil)
               ((and (numberp current-stamp)
                     (or (null stored-stamp) (> current-stamp stored-stamp)))
                (anvil-fusion-verify--cache-delete-raw key)
                nil)
               (t value)))))
      (error nil))))

(defun anvil-fusion-verify--cache-put (claim verdict evidence)
  "Store a decisive VERDICT/EVIDENCE pair for CLAIM best-effort.
This never signals.  `unverified' verdicts are intentionally not
cached so inconclusive claims stay retryable."
  (when (and (anvil-fusion-verify--cache-enabled-p)
             (memq verdict '(confirmed refuted))
             (stringp evidence))
    (condition-case nil
        (anvil-fusion-verify--cache-write-raw
         (anvil-fusion-verify--cache-key claim)
         (list :verdict verdict
               :evidence evidence
               :ts (funcall anvil-fusion-verify--cache-now-fn)
               :kb-stamp (anvil-fusion-verify--cache-kb-stamp)))
      (error nil))))

;;;; --- claim annotation helpers (6b) -----------------------------------------

(defun anvil-fusion-verify--annotate-claim (claim verdict evidence &rest props)
  "Return a FRESH copy of CLAIM (a Phase 6a claim plist) carrying
:VERDICT VERDICT and :EVIDENCE EVIDENCE, plus optional PROPS.
Never mutates CLAIM — a new plist is allocated so
`anvil-fusion-verify-claims' never touches its CLAIMS argument."
  (append
   (list :claim (plist-get claim :claim)
         :kind (plist-get claim :kind)
         :candidates (plist-get claim :candidates)
         :verdict verdict
         :evidence evidence)
   props))

(defun anvil-fusion-verify--claim-evidence-line (kb-evidence agg)
  "Return the one-line :EVIDENCE string for a claim.
KB-EVIDENCE is `anvil-fusion-verify--kb-search''s result for the
claim (ranked (:source :text) plists, or nil).  AGG is
`anvil-fusion-verify--aggregate-verdicts''s result.  Prefers a
\"SOURCE — REASON\" pointer when KB evidence exists and the
aggregate verdict is decisive (`confirmed' / `refuted'); otherwise
falls back to the winning reason alone, then \"\"."
  (let ((verdict (plist-get agg :verdict))
        (reason  (or (plist-get agg :reason) "")))
    (cond
     ((and kb-evidence (memq verdict '(confirmed refuted)))
      (format "%s — %s" (plist-get (car kb-evidence) :source) reason))
     ((not (string-empty-p reason)) reason)
     (t ""))))

(defun anvil-fusion-verify--run-mechanical-checkers (claim)
  "Run mechanical checkers for CLAIM and return the first usable result.
Checker errors and invalid verdicts are ignored so this helper never
signals."
  (catch 'done
    (dolist (fn anvil-fusion-verify-mechanical-checkers)
      (when (functionp fn)
        (let ((result (condition-case nil
                          (funcall fn claim)
                        (error nil))))
          (when result
            (let ((verdict (plist-get result :verdict))
                  (evidence (plist-get result :evidence)))
              (cond
               ((and (eq verdict 'confirmed) (stringp evidence))
                (throw 'done (list :verdict 'confirmed :evidence evidence)))
               ((and (null verdict) (stringp evidence))
                (throw 'done (list :evidence evidence)))))))))
    nil))

(defun anvil-fusion-verify-checker-repo-reality (claim)
  "Best-effort repo-reality checker for CLAIM.
With `anvil-fusion-verify-repo-root' bound to an existing directory:

1. Path tokens like `tests/foo-test.el' are checked with
   `file-exists-p'; the first existing path confirms the claim, while a
   claim that names only missing paths yields a mechanical evidence hint.
2. If no path token exists, Elisp-looking symbols such as
   `anvil-fusion-ask' are searched in *.el definition forms under the
   root.  A found symbol confirms the claim; otherwise the missing
   symbol list is returned as a hint.

Returns nil when no applicable tokens are found, when the root is nil or
missing, or on any internal failure.  Never signals."
  (condition-case nil
      (let ((root anvil-fusion-verify-repo-root))
        (when (and (stringp root) (file-directory-p root))
          (let* ((text (plist-get claim :claim))
                 (paths (anvil-fusion-verify--collect-regexp-matches
                         anvil-fusion-verify--repo-path-token-regexp text)))
            (if paths
                (let ((found nil)
                      (missing nil))
                  (dolist (path paths)
                    (if (file-exists-p (expand-file-name path root))
                        (unless found
                          (setq found path))
                      (setq missing (append missing (list path)))))
                  (if found
                      (list :verdict 'confirmed
                            :evidence (format "exists: %s" found))
                    (list :evidence
                          (format "not found under %s: %s"
                                  root (mapconcat #'identity missing ", ")))))
              (let ((symbols (anvil-fusion-verify--collect-regexp-matches
                              anvil-fusion-verify--repo-symbol-token-regexp text)))
                (when symbols
                  (let ((program (anvil-fusion-verify--kb-grep-program))
                        (hit nil))
                    (dolist (token symbols)
                      (unless hit
                        (let ((loc (anvil-fusion-verify--repo-symbol-hit root token program)))
                          (when loc
                            (setq hit (format "defined: %s at %s" token loc))))))
                    (if hit
                        (list :verdict 'confirmed :evidence hit)
                      (list :evidence
                            (format "symbol(s) not found under %s: %s"
                                    root (mapconcat #'identity symbols ", ")))))))))))
    (error nil)))
;;;; --- orchestration wrapper (6b, lazy require of anvil-orchestrator) -------

(defun anvil-fusion-verify--task-verdict (task-id)
  "Return a verdict plist for skeptic TASK-ID.
Fetches the result via `anvil-orchestrator-extract-result' (full); a
non-`done' terminal status (failure / timeout / cancellation) counts
as an `unverified' vote rather than propagating — the skeptic vote
is best-effort per task, same discipline as Phase 6a's extraction
task."
  (let* ((result (anvil-orchestrator-extract-result task-id t))
         (status (plist-get result :status)))
    (if (eq status 'done)
        (anvil-fusion-verify--parse-verdict (plist-get result :summary))
      (list :verdict 'unverified :reason ""))))

(cl-defun anvil-fusion-verify-claims
    (claims &key question egress provider model skeptics timeout-sec (max-wait-sec 1800))
  "Annotate each claim in CLAIMS with a Phase 6b evidence verdict.

CLAIMS is the Phase 6a claim-plist list (see
`anvil-fusion-verify-extract-claims').  Returns a NEW list of claim
plists — CLAIMS is never mutated — each gaining two keys: :VERDICT
(`confirmed' / `refuted' / `unverified') and :EVIDENCE (a one-line
string, or \"\" when nothing usable was found).  An empty/nil CLAIMS
returns nil immediately, without touching the orchestrator.

Two evidence sources are combined per claim:
1. Local KB grep (`anvil-fusion-verify--kb-search' over
   `anvil-fusion-verify-kb-roots'), zero-egress and therefore always
   run — even under EGRESS `local-only'.
2. An adversarial skeptic vote: SKEPTICS (default
   `anvil-fusion-verify-skeptics') independent tasks per claim, each
   asked to try to refute the claim
   (`anvil-fusion-verify-skeptic-template'), majority-aggregated via
   `anvil-fusion-verify--aggregate-verdicts'.  ALL skeptic tasks for
   ALL claims are submitted as ONE orchestrator batch (task names
   \"fusion-verify-skeptic-<claim-index>-<k>\"), collected with
   :wait, then read back one at a time via
   `anvil-orchestrator-extract-result' — the same submit -> collect
   -> extract-result pattern `anvil-fusion-verify-extract-claims'
   already uses.

QUESTION is the original question the claims' candidates answered
(context for the skeptic prompt).  PROVIDER / MODEL override
`anvil-fusion-verify-skeptic-provider' /
`anvil-fusion-verify-skeptic-model'.  EGRESS is `external' (default)
or `local-only'; under `local-only' the *effective* skeptic provider
must satisfy `anvil-fusion-provider-local-p' (the exact predicate
`anvil-fusion-panels' uses to validate sovereign panels), or this
signals a `user-error' BEFORE anything is submitted — mirroring
`anvil-fusion-ask''s sovereignty refusal.  The decisive verdict cache
is local storage only (`anvil-state', else an in-session hash table),
so its reads/writes remain allowed under `local-only'.  TIMEOUT-SEC
caps each skeptic task; MAX-WAIT-SEC caps the batch collect wait.

Best-effort like Phase 6a: an individual skeptic task that does not
reach `done' counts as an `unverified' vote rather than signaling; a
wholesale orchestrator error (e.g. submit itself fails) annotates
EVERY claim `:verdict unverified :evidence \"\"' and emits a
`message' warning instead of propagating.  The EGRESS sovereignty
check above is the sole exception — it always signals."
  (when claims
    (let* ((eg   (or egress 'external))
           (prov (or provider anvil-fusion-verify-skeptic-provider))
           (mdl  (or model anvil-fusion-verify-skeptic-model))
           (nsk  (or skeptics anvil-fusion-verify-skeptics)))
      (when (and (eq eg 'local-only) (not (anvil-fusion-provider-local-p prov)))
        (user-error
         "anvil-fusion-verify: local-only egress refuses non-local skeptic provider %S"
         prov))
      (let ((pending nil)
            (confirmed (make-hash-table :test 'eql))
            (kb-evidence-by-idx (make-hash-table :test 'eql))
            (claim-index -1))
        (dolist (claim claims)
          (setq claim-index (1+ claim-index))
          (let* ((mechanical (anvil-fusion-verify--run-mechanical-checkers claim))
                 (hint (plist-get mechanical :evidence)))
            (if (eq (plist-get mechanical :verdict) 'confirmed)
                (puthash claim-index
                         (anvil-fusion-verify--annotate-claim claim 'confirmed hint)
                         confirmed)
              (let ((cached (anvil-fusion-verify--cache-get claim)))
                (if cached
                    (puthash claim-index
                             (anvil-fusion-verify--annotate-claim
                              claim
                              (plist-get cached :verdict)
                              (plist-get cached :evidence)
                              :cached t)
                             confirmed)
                  (let ((kb-evidence (anvil-fusion-verify--kb-search (plist-get claim :claim))))
                    (when hint
                      (setq kb-evidence
                            (append kb-evidence
                                    (list (list :source "mechanical" :text hint)))))
                    (puthash claim-index kb-evidence kb-evidence-by-idx)
                    (push (cons claim-index claim) pending)))))))
        (if (null pending)
            (let (ordered)
              (dotimes (i (length claims))
                (setq ordered (append ordered (list (gethash i confirmed)))))
              ordered)
          (require 'anvil-orchestrator)
          (condition-case err
              (let ((tasks nil)
                    (ordered-pending (reverse pending)))
                (dolist (entry ordered-pending)
                  (let* ((claim-index (car entry))
                         (claim (cdr entry))
                         (prompt (anvil-fusion-verify--skeptic-prompt
                                  question
                                  (plist-get claim :claim)
                                  (gethash claim-index kb-evidence-by-idx))))
                    (dotimes (k nsk)
                      (push (append
                             (list :name (format "fusion-verify-skeptic-%d-%d" claim-index k)
                                   :provider prov
                                   :prompt prompt)
                             (and mdl (list :model mdl))
                             (and timeout-sec (list :timeout-sec timeout-sec)))
                            tasks))))
                (setq tasks (nreverse tasks))
                (let* ((batch (anvil-orchestrator-submit tasks)))
                  (anvil-orchestrator-collect batch :wait t :max-wait-sec max-wait-sec)
                  (let* ((btasks (plist-get (anvil-orchestrator-status batch) :tasks))
                         (by-claim (make-hash-table :test 'eql))
                         (results (make-hash-table :test 'eql)))
                    (dolist (tk btasks)
                      (let ((nm (plist-get tk :name)))
                        (when (and (stringp nm)
                                   (string-match
                                    "\\`fusion-verify-skeptic-\\([0-9]+\\)-[0-9]+\\'" nm))
                          (let ((idx (string-to-number (match-string 1 nm))))
                            (puthash idx (cons (plist-get tk :id) (gethash idx by-claim))
                                     by-claim)))))
                    (dolist (entry ordered-pending)
                      (let* ((claim-index (car entry))
                             (claim (cdr entry))
                             (ids (nreverse (gethash claim-index by-claim)))
                             (verdicts (mapcar #'anvil-fusion-verify--task-verdict ids))
                             (agg (anvil-fusion-verify--aggregate-verdicts verdicts))
                             (evidence (anvil-fusion-verify--claim-evidence-line
                                        (gethash claim-index kb-evidence-by-idx) agg)))
                        (anvil-fusion-verify--cache-put
                         claim (plist-get agg :verdict) evidence)
                        (puthash
                         claim-index
                         (anvil-fusion-verify--annotate-claim
                          claim (plist-get agg :verdict) evidence)
                         results)))
                    (let (ordered)
                      (dotimes (i (length claims))
                        (setq ordered
                              (append ordered
                                      (list (or (gethash i confirmed)
                                                (gethash i results))))))
                      ordered))))
            (error
             (message "anvil-fusion-verify: verify-claims failed: %s"
                      (error-message-string err))
             (mapcar (lambda (claim)
                       (anvil-fusion-verify--annotate-claim claim 'unverified ""))
                     claims))))))))
;;;; ============================================================
;;;; Phase 6d — verdict-annotated judge synthesis
;;;; ============================================================

;;;; --- customization (6d) ---------------------------------------------------

(defcustom anvil-fusion-verify-judge-template
  "あなたは、ひとつの問いに対する複数の AI アシスタントの回答を統合する判定者です。

# 原問
%s

# 候補回答
%s

# 検証済み主張表
{{CLAIMS}}

# 手順
以下を順に行ってください。
1. 合意点 — 複数の候補が一致して主張している点を列挙する。
2. 矛盾点 — 候補間で食い違う主張を挙げ、どちらがより妥当かを根拠とともに判定する。
3. 部分カバー — 一部の候補だけが触れている重要な点。
4. 独自洞察 — 単一の候補のみが提供した価値ある視点。
5. 見落とし — どの候補も答えていないが、原問が要求している点。
6. 検証済み主張表の見方 — VERDICT は confirmed（確認済み）/ refuted（反証済み）/
   unverified（未検証）のいずれか、EVIDENCE は file:line の出典または反証理由を示す。
7. REFUTED と判定された主張を最終回答に採用してはならない。REFUTED な主張にしか
   根拠を持たない候補の結論は棄却すること。
8. CONFIRMED な主張は、その evidence とともに優先的に採用すること。UNVERIFIED な
   主張は通常の判断（上記 1〜5 の分析）で扱ってよい。
9. 全候補の結論が REFUTED な主張にのみ依存している場合は、無理に統合せず、検証
   不合格である旨と、何が反証されたかを明示した回答をすること。

上記の分析を踏まえ、それらを統合した最終回答を作成する。最終回答は、いずれの
候補単体よりも正確かつ網羅的でなければならない。失敗・空・エラーの候補は割り引く。

# 出力形式
## 分析
（手順 1〜9 を簡潔に）
## 最終回答
（原問に対する、統合された単一の回答）"
  "Verified variant of `anvil-fusion-judge-template' (Doc 61 Phase 6d,
docs/design/61-fusion-verify.org §2).  Deliberately structurally
parallel to the base template (same five analysis axes 1-5, same
`# 出力形式' contract) so the two stay visibly diffable; steps 6-9 are
the only addition, teaching the judge to read the Phase 6b verdict
table and forbidding it from adopting a REFUTED claim.

Placeholder design (read this before customizing): this template
still carries exactly the same TWO %s placeholders as
`anvil-fusion-judge-template', filled in the same order -- (1) the
original question, (2) the candidate block -- by the same `format'
call `anvil-fusion-build-judge-prompt' already makes for every
:template (anvil-fusion.el, `anvil-fusion-build-judge-prompt''s
`(format tmpl (or original-prompt \"\") (anvil-fusion--format-candidates ...))').
The claims table is NOT a third %s slot; it is the literal marker
`{{CLAIMS}}', substituted by `anvil-fusion-verify-judge-template-for'
BEFORE that `format' call ever runs.  This is the seam Doc 61 asked
for: it makes a Phase 6d template a drop-in :TEMPLATE argument for
the existing, UNCHANGED `anvil-fusion-judge-consensus' /
`anvil-fusion-build-judge-prompt' plumbing in anvil-fusion.el -- zero
changes needed there.  Precisely BECAUSE the substitution happens
before that later `format' call sees the string,
`anvil-fusion-verify-judge-template-for' must (and does) double any
literal `%' inside the rendered claims block to `%%' first -- else a
claim like \"効率は 95% です\" would be misread as a `format' directive
and corrupt (or error) the final judge prompt."
  :type 'string
  :group 'anvil-fusion)

(defconst anvil-fusion-verify--claims-marker "{{CLAIMS}}"
  "Literal marker in a judge template, substituted by
`anvil-fusion-verify-judge-template-for' with the rendered claims
block.  Deliberately NOT a %s placeholder -- see
`anvil-fusion-verify-judge-template''s docstring for why.")

;;;; --- pure rendering + template plumbing (6d) -------------------------------

(defun anvil-fusion-verify--format-claims-block (claims)
  "Render CLAIMS as a numbered table for the Phase 6d judge template.
CLAIMS is a list of Phase 6a claim plists, ideally annotated with
:VERDICT / :EVIDENCE by `anvil-fusion-verify-claims'.  A claim missing
:VERDICT renders as `unverified' (the same default
`anvil-fusion-verify--parse-verdict' uses for an absent/garbage
verdict) with \"(証拠なし)\" for a missing/empty :EVIDENCE.  Returns
\"(検証済み主張なし)\" for nil/empty CLAIMS.  Pure -- no I/O, no
orchestrator dependency."
  (if (null claims)
      "(検証済み主張なし)"
    (let ((i 0))
      (mapconcat
       (lambda (c)
         (setq i (1+ i))
         (format (concat "%d. 主張: %s\n"
                         "   KIND: %s\n"
                         "   VERDICT: %s\n"
                         "   EVIDENCE: %s\n"
                         "   主張した候補: %s")
                 i
                 (or (plist-get c :claim) "")
                 (or (plist-get c :kind) 'fact)
                 (or (plist-get c :verdict) 'unverified)
                 (let ((ev (plist-get c :evidence)))
                   (if (and (stringp ev) (not (string-empty-p ev))) ev "(証拠なし)"))
                 (let ((cands (plist-get c :candidates)))
                   (if cands (mapconcat #'identity cands ", ") "(不明)"))))
       claims "\n\n"))))

(defun anvil-fusion-verify--escape-percent (text)
  "Double every `%' in TEXT so it survives a later `format' call as a
single literal `%' instead of being read as a directive.  Pure."
  (replace-regexp-in-string "%" "%%" (or text "")))

(defun anvil-fusion-verify-judge-template-for (claims &optional base-template)
  "Return a judge :TEMPLATE with CLAIMS substituted into BASE-TEMPLATE.

BASE-TEMPLATE defaults to `anvil-fusion-verify-judge-template'.  Its
`{{CLAIMS}}' marker (`anvil-fusion-verify--claims-marker') is replaced
with CLAIMS rendered via `anvil-fusion-verify--format-claims-block'.
The result is ready to pass as the :TEMPLATE argument to
`anvil-fusion-judge-consensus' / `anvil-fusion-build-judge-prompt' --
it still has exactly the same two %s placeholders (question, then
candidate block) BASE-TEMPLATE had; the marker itself carries no %s
and is substituted as LITERAL text (`replace-regexp-in-string' called
with FIXEDCASE and LITERAL both t, so no accidental `\\&'/`\\N'
backslash expansion from the rendered block).

Because the marker substitution happens BEFORE
`anvil-fusion-build-judge-prompt' applies its `format' call to the
resulting template string, any `%' inside the rendered claims block is
first doubled to `%%' (`anvil-fusion-verify--escape-percent') so it
survives that later `format' as a single literal `%' rather than being
misread as (or overflowing) a directive -- see
`anvil-fusion-verify-judge-template''s docstring for the full
rationale of this two-stage substitute-then-format design.

Signals `user-error' when BASE-TEMPLATE lacks the marker: silently
falling back to an un-grounded judge prompt would defeat the entire
point of Phase 6d, so misconfiguration is loud rather than silent."
  (let ((base (or base-template anvil-fusion-verify-judge-template)))
    (unless (string-match-p (regexp-quote anvil-fusion-verify--claims-marker) base)
      (user-error
       "anvil-fusion-verify-judge-template-for: BASE-TEMPLATE lacks the %s marker"
       anvil-fusion-verify--claims-marker))
    (replace-regexp-in-string
     (regexp-quote anvil-fusion-verify--claims-marker)
     (anvil-fusion-verify--escape-percent (anvil-fusion-verify--format-claims-block claims))
     base t t)))

(provide 'anvil-fusion-verify)
;;; anvil-fusion-verify.el ends here

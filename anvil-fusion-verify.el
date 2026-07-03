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

(defcustom anvil-fusion-verify-max-evidence 3
  "Maximum KB snippets kept as evidence per claim (Phase 6b).
Caps both what `anvil-fusion-verify--kb-search' returns and what is
fed into the skeptic prompt's evidence block."
  :type 'integer
  :group 'anvil-fusion)

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

;;;; --- claim annotation helpers (6b) -----------------------------------------

(defun anvil-fusion-verify--annotate-claim (claim verdict evidence)
  "Return a FRESH copy of CLAIM (a Phase 6a claim plist) carrying
:VERDICT VERDICT and :EVIDENCE EVIDENCE.  Never mutates CLAIM — a new
plist is allocated so `anvil-fusion-verify-claims' never touches its
CLAIMS argument."
  (list :claim (plist-get claim :claim)
        :kind (plist-get claim :kind)
        :candidates (plist-get claim :candidates)
        :verdict verdict
        :evidence evidence))

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
`anvil-fusion-ask''s sovereignty refusal.  TIMEOUT-SEC caps each
skeptic task; MAX-WAIT-SEC caps the batch collect wait.

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
      (require 'anvil-orchestrator)
      (condition-case err
          (let* ((kb-evidence (mapcar (lambda (c)
                                         (anvil-fusion-verify--kb-search (plist-get c :claim)))
                                       claims))
                 (tasks nil)
                 (ci -1))
            (cl-mapc
             (lambda (c ev)
               (setq ci (1+ ci))
               (let ((prompt (anvil-fusion-verify--skeptic-prompt
                              question (plist-get c :claim) ev)))
                 (dotimes (k nsk)
                   (push (append
                          (list :name (format "fusion-verify-skeptic-%d-%d" ci k)
                                :provider prov
                                :prompt prompt)
                          (and mdl (list :model mdl))
                          (and timeout-sec (list :timeout-sec timeout-sec)))
                         tasks))))
             claims kb-evidence)
            (setq tasks (nreverse tasks))
            (let* ((batch (anvil-orchestrator-submit tasks)))
              (anvil-orchestrator-collect batch :wait t :max-wait-sec max-wait-sec)
              (let* ((btasks (plist-get (anvil-orchestrator-status batch) :tasks))
                     (by-claim (make-hash-table :test 'eql)))
                (dolist (tk btasks)
                  (let ((nm (plist-get tk :name)))
                    (when (and (stringp nm)
                               (string-match
                                "\\`fusion-verify-skeptic-\\([0-9]+\\)-[0-9]+\\'" nm))
                      (let ((idx (string-to-number (match-string 1 nm))))
                        (puthash idx (cons (plist-get tk :id) (gethash idx by-claim))
                                 by-claim)))))
                (let ((ci2 -1))
                  (cl-mapcar
                   (lambda (c ev)
                     (setq ci2 (1+ ci2))
                     (let* ((ids (nreverse (gethash ci2 by-claim)))
                            (verdicts (mapcar #'anvil-fusion-verify--task-verdict ids))
                            (agg (anvil-fusion-verify--aggregate-verdicts verdicts)))
                       (anvil-fusion-verify--annotate-claim
                        c (plist-get agg :verdict)
                        (anvil-fusion-verify--claim-evidence-line ev agg))))
                   claims kb-evidence)))))
        (error
         (message "anvil-fusion-verify: verify-claims failed: %s"
                  (error-message-string err))
         (mapcar (lambda (c) (anvil-fusion-verify--annotate-claim c 'unverified ""))
                 claims))))))

(provide 'anvil-fusion-verify)
;;; anvil-fusion-verify.el ends here

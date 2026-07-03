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

;;; Code:

(require 'cl-lib)
(require 'anvil-fusion)

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

(defcustom anvil-fusion-verify-extract-model nil
  "Model for the claim-extraction pass, or nil for the provider default.
Nil lets the provider pick its default model.  Set this to pin a
cheaper/faster tier (e.g. a haiku-class model) once Doc 61 §9's
haiku-vs-sonnet fidelity question is settled; left open for now so
it is overridable per call via the :model argument to
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
in sync with the parser's regexp if you customize it."
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
via `anvil-fusion--batch-first-task-id').  :PROVIDER / :MODEL default
to `anvil-fusion-verify-extract-provider' /
`anvil-fusion-verify-extract-model'.  :TIMEOUT-SEC caps the task;
:MAX-WAIT-SEC caps the collect wait (default 1800s).

Extraction is best-effort: on task failure, a non-`done' terminal
status, or any signaled error, this returns nil and emits a
`message' warning instead of signaling — later phases treat \"no
claims\" the same as \"skip verification\".  Returns the parsed
claim list (see `anvil-fusion-verify--parse-claims') on success."
  (require 'anvil-orchestrator)
  (condition-case err
      (let* ((prov   (or provider anvil-fusion-verify-extract-provider))
             (mdl    (or model anvil-fusion-verify-extract-model))
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

(provide 'anvil-fusion-verify)
;;; anvil-fusion-verify.el ends here

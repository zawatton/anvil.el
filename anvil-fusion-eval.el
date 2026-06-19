;;; anvil-fusion-eval.el --- local eval harness for fusion panels -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion, eval

;;; Commentary:
;;
;; Phase 5 of docs/design/01-fusion-harness.org: a LOCAL eval harness.
;;
;; The Fusion "Fable-class" claim must be checked against the user's own
;; tasks, not a vendor benchmark (memory: validate counts/claims with
;; real numbers).  This module runs a small set of eval cases through any
;; "ask function" (a single model, or a panel via `anvil-fusion-ask'),
;; scores each answer with a DETERMINISTIC rubric, aggregates, and
;; compares configurations so panel composition decisions rest on
;; measured numbers.
;;
;; The scoring core is pure (term-coverage against a rubric) so it is
;; fully unit-testable without any live model.  An optional LLM-judge
;; scorer can be layered on top for nuance, but the cheap deterministic
;; signal is what drives the loop.  `anvil-fusion-eval-*-ask-fn' build the
;; live closures; the runner itself only calls the closure you give it,
;; so tests inject a stub.
;;
;; Tuning: `anvil-fusion-eval-tuning-report' turns a comparison into a
;; review prompt suitable for the existing autoresearch routing flow
;; (propose-then-review; it does not auto-edit `anvil-fusion-panels').

;;; Code:

(require 'cl-lib)
(require 'anvil-fusion)

(declare-function anvil-fusion-ask "anvil-fusion-ask" (prompt &rest args))
(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-collect "anvil-orchestrator" (batch-id &rest _))
(declare-function anvil-orchestrator-status "anvil-orchestrator" (id))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))

;;;; --- eval cases ----------------------------------------------------------

(defcustom anvil-fusion-eval-seed-cases
  '((:id legal-1 :category legal
     :prompt "高圧受電設備の地絡保護に一般に用いる継電器の種類と、その選定で考慮\
する点を説明してください。"
     :must-include ("地絡" "継電器")
     :should-include ("方向" "零相" "もらい事故")
     :must-not ("地絡は過電流継電器だけで検出できる"))
    (:id report-1 :category report
     :prompt "使用前自己確認における絶縁抵抗試験について、報告書に記載すべき基本\
項目を挙げてください。"
     :must-include ("絶縁抵抗" "測定")
     :should-include ("印加電圧" "判定基準")))
  "Starter eval cases — ILLUSTRATIVE.
Replace / extend with validated rubrics for your own tasks
(法令調査 / 報告書セクション / org 要約).  Each case is a plist:

  :id            unique symbol
  :category      grouping symbol (legal / report / summary / ...)
  :prompt        the question to ask
  :must-include  terms a correct answer MUST contain (drives score)
  :should-include bonus terms (reported, not scored into :score)
  :must-not      terms whose presence signals a wrong answer (halves)
  :reference     optional reference answer (for an LLM judge)"
  :type '(repeat sexp)
  :group 'anvil-fusion)

;;;; --- deterministic rubric scorer (pure) ----------------------------------

(defun anvil-fusion-eval--hit-p (text term)
  "Return non-nil when TERM occurs (literally) in TEXT.
Matching is case-insensitive (so e.g. \"DGR\" matches \"dgr\") and
independent of the ambient `case-fold-search'."
  (and (stringp text) (stringp term)
       (let ((case-fold-search t))
         (string-match-p (regexp-quote term) text))))

(defun anvil-fusion-eval-score-answer (answer case)
  "Score ANSWER (string) against CASE's rubric.
Returns (:score FLOAT :must-hit N :must-total N :should-hit N
:should-total N :penalized BOOL :missing LIST).  Score is the
fraction of :must-include terms present (1.0 when none specified),
halved when any :must-not term appears."
  (let* ((must     (plist-get case :must-include))
         (should   (plist-get case :should-include))
         (mustnot  (plist-get case :must-not))
         (must-hit (cl-count-if (lambda (tm) (anvil-fusion-eval--hit-p answer tm)) must))
         (sh-hit   (cl-count-if (lambda (tm) (anvil-fusion-eval--hit-p answer tm)) should))
         (bad      (cl-some (lambda (tm) (anvil-fusion-eval--hit-p answer tm)) mustnot))
         (base     (if must (/ (float must-hit) (length must)) 1.0))
         (score    (max 0.0 (min 1.0 (if bad (* base 0.5) base))))
         (missing  (cl-remove-if (lambda (tm) (anvil-fusion-eval--hit-p answer tm)) must)))
    (list :score score
          :must-hit must-hit :must-total (length must)
          :should-hit sh-hit :should-total (length should)
          :penalized (and bad t)
          :missing missing)))

;;;; --- runner (pure given ASK-FN) ------------------------------------------

(defun anvil-fusion-eval-run (cases ask-fn)
  "Run CASES through ASK-FN and return a scored result list.
ASK-FN is called as (funcall ASK-FN PROMPT) and must return a
plist with at least :answer; :elapsed-ms and :egress are passed
through when present.  Each result is (:id :category :score
:detail :elapsed-ms :egress :answer)."
  (mapcar
   (lambda (case)
     (let* ((ans (funcall ask-fn (plist-get case :prompt)))
            (answer (plist-get ans :answer))
            (detail (anvil-fusion-eval-score-answer answer case)))
       (list :id        (plist-get case :id)
             :category  (plist-get case :category)
             :score     (plist-get detail :score)
             :detail    detail
             :elapsed-ms (plist-get ans :elapsed-ms)
             :egress    (plist-get ans :egress)
             :answer    answer)))
   cases))

;;;; --- aggregation + comparison (pure) -------------------------------------

(defun anvil-fusion-eval--mean (nums)
  "Mean of NUMS, or nil when empty."
  (when nums (/ (apply #'+ nums) (float (length nums)))))

(defun anvil-fusion-eval-aggregate (results)
  "Aggregate a RESULTS list into summary metrics.
Returns (:n :mean-score :mean-elapsed-ms :egress-external
:by-category ALIST) where ALIST maps category -> mean score."
  (let* ((n      (length results))
         (scores (mapcar (lambda (r) (plist-get r :score)) results))
         (elapsed (delq nil (mapcar (lambda (r) (plist-get r :elapsed-ms)) results)))
         (cats   (delete-dups (mapcar (lambda (r) (plist-get r :category)) results)))
         (by-cat (mapcar
                  (lambda (cat)
                    (cons cat
                          (anvil-fusion-eval--mean
                           (mapcar (lambda (r) (plist-get r :score))
                                   (cl-remove-if-not
                                    (lambda (r) (eq (plist-get r :category) cat))
                                    results)))))
                  cats)))
    (list :n n
          :mean-score (or (anvil-fusion-eval--mean scores) 0.0)
          :mean-elapsed-ms (anvil-fusion-eval--mean elapsed)
          :egress-external (cl-count-if
                            (lambda (r) (eq (plist-get r :egress) 'external))
                            results)
          :by-category by-cat)))

(defun anvil-fusion-eval-compare (config-results)
  "Compare configurations.
CONFIG-RESULTS is an alist (CONFIG-NAME . RESULTS).  Returns
(:ranking ((NAME . MEAN) ... desc) :winner NAME :aggregates
((NAME . AGG) ...))."
  (let* ((aggs (mapcar (lambda (cr)
                         (cons (car cr) (anvil-fusion-eval-aggregate (cdr cr))))
                       config-results))
         (ranking (sort (mapcar (lambda (a)
                                  (cons (car a) (plist-get (cdr a) :mean-score)))
                                aggs)
                        (lambda (x y) (> (cdr x) (cdr y))))))
    (list :ranking   ranking
          :winner    (car (car ranking))
          :aggregates aggs)))

(defun anvil-fusion-eval-tuning-report (compare)
  "Render COMPARE (from `anvil-fusion-eval-compare') as a review report.
Plain text summarizing each config's mean score (+ egress), the
winner, and a prompt for the autoresearch propose-then-review
flow.  Does not edit `anvil-fusion-panels'."
  (let ((ranking (plist-get compare :ranking))
        (winner  (plist-get compare :winner))
        (aggs    (plist-get compare :aggregates)))
    (concat
     "# Fusion panel eval — comparison\n\n"
     (mapconcat
      (lambda (rk)
        (let* ((name (car rk))
               (agg  (cdr (assq name aggs)))
               (ext  (plist-get agg :egress-external))
               (n    (plist-get agg :n)))
          (format "- %s: mean=%.3f  egress-external=%d/%d%s"
                  name (cdr rk) ext n
                  (if (zerop ext) "  (sovereign: 0 egress)" ""))))
      ranking "\n")
     (format "\n\nwinner: %s\n\n" winner)
     "提案: 上記の自前 eval 数値に基づき anvil-fusion-panels の構成見直しを"
     "autoresearch-routing-propose で review してください "
     "(数値が拮抗かつ sovereign の egress=0 なら sovereign を既定維持)。")))

;;;; --- live ask-fn builders (not unit-tested; need real providers) ---------

(defun anvil-fusion-eval-panel-ask-fn (panel &rest ask-args)
  "Return an eval ask-fn that answers via `anvil-fusion-ask' on PANEL.
ASK-ARGS are forwarded to `anvil-fusion-ask'.  The returned closure
times the call and normalizes to (:answer :egress :elapsed-ms)."
  (lambda (prompt)
    (let* ((t0  (float-time))
           (res (apply #'anvil-fusion-ask prompt :panel panel ask-args))
           (ms  (round (* 1000 (- (float-time) t0)))))
      (list :answer (plist-get res :answer)
            :egress (plist-get res :egress)
            :elapsed-ms ms))))

(defun anvil-fusion-eval-single-ask-fn (provider &optional model cwd)
  "Return an eval ask-fn that answers with a single PROVIDER / MODEL.
Uses the same public orchestrator calls as the panel path so the
baseline is comparable.  Optional CWD sets the task working dir —
use a neutral dir (e.g. \"/tmp\") for a claude baseline so the
nested Claude Code session does not inherit project hooks /
CLAUDE.md.  Egress is reported as `external'."
  (lambda (prompt)
    (require 'anvil-orchestrator)
    (let* ((t0 (float-time))
           (task (append (list :provider provider :prompt prompt
                               :name "fusion-eval-single")
                         (and model (list :model model))
                         (and cwd (list :cwd cwd))))
           (batch (anvil-orchestrator-submit (list task))))
      (anvil-orchestrator-collect batch :wait t)
      (let* ((status (anvil-orchestrator-status batch))
             (id     (plist-get (car (plist-get status :tasks)) :id))
             (res    (anvil-orchestrator-extract-result id t))
             (ms     (round (* 1000 (- (float-time) t0)))))
        (list :answer (plist-get res :summary)
              :egress 'external
              :elapsed-ms ms)))))

(provide 'anvil-fusion-eval)
;;; anvil-fusion-eval.el ends here

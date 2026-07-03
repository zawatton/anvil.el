;;; run.el --- Doc 61 Phase 6e verify-effect runner  -*- lexical-binding: t; -*-

;; Runs benchmarks/doc61-phase6e/dataset.el's Tier A / Tier B cases through
;; live anvil-orchestrator / anvil-fusion / anvil-fusion-verify calls and
;; scores them deterministically (regex match, no LLM judging).  See
;; README.org in this directory for the dataset, run conditions, and
;; scoring rules this implements.
;;
;; Usage (repo root as -L target; run from a SCRATCH, non-git cwd so
;; `anvil-orchestrator-worktree-auto' does not fire -- see
;; benchmarks/doc61-phase6a/run.el for the pattern this follows):
;;
;;   cd /path/to/scratch   # non-git dir
;;   emacs --batch -L /path/to/anvil.el \
;;     --eval '(setq anvil-state-db-path "/path/to/scratch/state.db")' \
;;     --eval '(setq anvil-orchestrator-work-dir "/path/to/scratch/workdir")' \
;;     -l /path/to/anvil.el/benchmarks/doc61-phase6e/dataset.el \
;;     -l /path/to/anvil.el/benchmarks/doc61-phase6e/run.el \
;;     -- TIER CONDITION [ID...]  > /path/to/scratch/tier-a-plain.log 2>&1
;;
;; TIER is "tier-a" or "tier-b".  CONDITION is "plain" / "verified" for
;; tier-a, or "single" / "fusion-plain" / "fusion-verified" for tier-b.
;; Optional trailing ID... args (e.g. "a01") restrict the run to those
;; case ids -- used for the pre-battery smoke test.
;;
;; Every case writes its raw answer text (and, for verify conditions, its
;; claim list) under raw/ immediately on completion, and the whole
;; tier+condition result list is checkpointed to
;; raw/<tier>-<condition>.checkpoint.sexp after EVERY case (not just at
;; the end) so a crash mid-battery loses at most the one in-flight case.
;;
;; Costs real money (~14 live claude calls for tier-a verified worst
;; case, ~2-15 for the others depending on condition) -- do not loop
;; this in CI.

;;; Code:

(require 'cl-lib)
(require 'anvil-orchestrator)
(require 'anvil-fusion)
(require 'anvil-fusion-panels)
(require 'anvil-fusion-verify)
(require 'anvil-fusion-ask)

;;;; --- paths -----------------------------------------------------------

(defconst anvil-b6e--self-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory this script lives in, regardless of `default-directory'
at load time (batch runs cd into a scratch dir first).")

(defconst anvil-b6e--raw-dir (expand-file-name "raw" anvil-b6e--self-dir)
  "Where per-case raw answers / claims / checkpoints are written.")

(defconst anvil-b6e--kb-roots '("/home/madblack-21/Cowork/Notes/node")
  "KB roots for the verify conditions -- read-only, outside this repo,
zero-egress local grep (see anvil-fusion-verify-kb-roots).")

;;;; --- cost-capture advice (mirrors benchmarks/doc61-phase6a/run.el) ---

(defvar anvil-b6e--captured nil
  "Accumulator of `anvil-orchestrator-extract-result' return plists,
dynamically rebound fresh per phase by `anvil-b6e--metered'.  This is
how sub-call costs (claim extraction, skeptic votes, judge) that are
never surfaced to the caller by `anvil-fusion-verify-extract-claims' /
`anvil-fusion-verify-claims' / `anvil-fusion-ask' get recovered without
touching those modules -- same technique as
benchmarks/doc61-phase6a/run.el's advice on the same function.")

(defun anvil-b6e--capture (result)
  (push result anvil-b6e--captured)
  result)

(advice-add 'anvil-orchestrator-extract-result :filter-return #'anvil-b6e--capture)

(defmacro anvil-b6e--metered (&rest body)
  "Run BODY with a fresh capture scope.  Returns (VALUE WALL-MS COST-USD).
COST-USD sums :cost-usd across every `anvil-orchestrator-extract-result'
call made inside BODY's dynamic extent, or nil if none reported a
number.  Member fan-out cost is NOT included (fusion-ask never calls
extract-result for members) -- see `anvil-b6e--candidates-cost'."
  (declare (indent 0))
  `(let ((anvil-b6e--captured nil)
         (anvil-b6e--m-t0 (float-time)))
     (let* ((anvil-b6e--m-v (progn ,@body))
            (anvil-b6e--m-wall (round (* 1000 (- (float-time) anvil-b6e--m-t0))))
            (anvil-b6e--m-cost
             (let ((anvil-b6e--m-sum 0.0) (anvil-b6e--m-any nil))
               (dolist (anvil-b6e--m-r anvil-b6e--captured)
                 (let ((anvil-b6e--m-c (plist-get anvil-b6e--m-r :cost-usd)))
                   (when (numberp anvil-b6e--m-c)
                     (setq anvil-b6e--m-any t
                           anvil-b6e--m-sum (+ anvil-b6e--m-sum anvil-b6e--m-c)))))
               (if anvil-b6e--m-any anvil-b6e--m-sum nil))))
       (list anvil-b6e--m-v anvil-b6e--m-wall anvil-b6e--m-cost))))

(defun anvil-b6e--candidates-cost (candidates)
  "Sum :cost-usd across slim task plists CANDIDATES, or nil if none numeric."
  (let ((sum 0.0) (any nil))
    (dolist (c candidates)
      (let ((v (plist-get c :cost-usd)))
        (when (numberp v) (setq any t sum (+ sum v)))))
    (if any sum nil)))

(defun anvil-b6e--add-cost (a b)
  (cond ((and a b) (+ a b)) (a a) (b b) (t nil)))

;;;; --- file helpers ------------------------------------------------------

(defun anvil-b6e--write (path text)
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert (or text ""))
      (write-region (point-min) (point-max) path nil 'quiet))))

(defun anvil-b6e--write-sexp (path obj)
  (anvil-b6e--write
   path
   (let ((print-length nil) (print-level nil) (print-circle nil))
     (pp-to-string obj))))

;;;; --- Tier A precedence-aware "final answer section" extraction -------

(defun anvil-b6e--final-section (text)
  "Return (SECTION . FOUND).  SECTION is the substring of TEXT after a
`## 最終回答' (or bare `最終回答') heading; FOUND is t when a heading was
located, nil when TEXT is scored whole (e.g. a malformed/abstention
answer that never reaches the judge template's output-format contract)."
  (let ((s (or text "")))
    (cond
     ((string-match "#+[ \t]*最終回答[ \t]*\n" s)
      (cons (substring s (match-end 0)) t))
     ((string-match "最終回答[ \t]*[:：]?[ \t]*\n?" s)
      (cons (substring s (match-end 0)) t))
     (t (cons s nil)))))

;;;; --- case lookup / filtering -------------------------------------------

(defun anvil-b6e--find-case (id cases)
  (cl-find id cases :key (lambda (c) (plist-get c :id)) :test #'string=))

(defun anvil-b6e--filter-cases (cases ids)
  (if (null ids) cases
    (delq nil (mapcar (lambda (id) (anvil-b6e--find-case id cases)) ids))))

;;;; --- checkpointing -------------------------------------------------------

(defun anvil-b6e--checkpoint-path (tier condition)
  (expand-file-name (format "%s-%s.checkpoint.sexp" tier condition) anvil-b6e--raw-dir))

(defun anvil-b6e--save-checkpoint (tier condition results)
  (anvil-b6e--write-sexp (anvil-b6e--checkpoint-path tier condition) results))

;;;; ============================================================
;;;; Tier A
;;;; ============================================================

(defun anvil-b6e--tier-a-score (case answer-text)
  (let* ((gc (plist-get case :gold-correct))
         (gw (plist-get case :gold-wrong))
         (fs (anvil-b6e--final-section answer-text))
         (section (car fs)) (found (cdr fs))
         (case-fold-search t)
         (correct-p (and gc (string-match-p gc section)))
         (wrong-p (and gw (string-match-p gw section))))
    (list :verdict (cond (correct-p 'correct) (wrong-p 'wrong-adopted) (t 'abstained))
          :gold-correct-hit (and correct-p t)
          :gold-wrong-hit (and wrong-p t)
          :section-found found)))

(defun anvil-b6e--tier-a-run-plain (case)
  (let* ((id (plist-get case :id))
         (question (plist-get case :question))
         (candidates (plist-get case :candidates)))
    (condition-case err
        (with-timeout (400 (error "anvil-b6e: tier-a/plain case %s timed out" id))
          (cl-destructuring-bind (jresult wall-ms cost)
              (anvil-b6e--metered
                (anvil-fusion--run-judge
                 question candidates 'claude nil
                 :timeout-sec 180 :max-wait-sec 300))
            (let* ((answer (plist-get jresult :answer))
                   (score (anvil-b6e--tier-a-score case answer)))
              (anvil-b6e--write
               (expand-file-name (format "tier-a-plain-%s.answer.txt" id) anvil-b6e--raw-dir)
               answer)
              (append (list :id id :condition "plain" :answer answer
                            :wall-ms wall-ms :cost-usd cost :claims nil :fallback nil
                            :errored nil)
                      score))))
      (error
       (list :id id :condition "plain" :errored t
             :error-msg (error-message-string err) :verdict 'errored
             :wall-ms nil :cost-usd nil :claims nil)))))

(defun anvil-b6e--tier-a-run-verified (case)
  (let* ((id (plist-get case :id))
         (question (plist-get case :question))
         (candidates (plist-get case :candidates)))
    (condition-case err
        (with-timeout (900 (error "anvil-b6e: tier-a/verified case %s timed out" id))
          (cl-destructuring-bind (raw-claims wall-ms1 cost1)
              (anvil-b6e--metered
                (anvil-fusion-verify-extract-claims
                 question candidates
                 :provider 'claude :model "haiku" :timeout-sec 180 :max-wait-sec 300))
            (let* ((fallback (null raw-claims))
                   (claims nil) (template nil)
                   (wall-ms2 0) (cost2 nil))
              (unless fallback
                (cl-destructuring-bind (annotated w2 c2)
                    (anvil-b6e--metered
                      (let ((anvil-fusion-verify-kb-roots anvil-b6e--kb-roots))
                        (anvil-fusion-verify-claims
                         raw-claims :question question
                         :skeptics 2 :provider 'claude :model nil
                         :timeout-sec 180 :max-wait-sec 300)))
                  (setq claims annotated wall-ms2 w2 cost2 c2)
                  (setq template (anvil-fusion-verify-judge-template-for annotated))))
              (cl-destructuring-bind (jresult wall-ms3 cost3)
                  (anvil-b6e--metered
                    (anvil-fusion--run-judge
                     question candidates 'claude nil
                     :template template :timeout-sec 180 :max-wait-sec 300))
                (let* ((answer (plist-get jresult :answer))
                       (score (anvil-b6e--tier-a-score case answer))
                       (total-wall (+ wall-ms1 wall-ms2 wall-ms3))
                       (total-cost (anvil-b6e--add-cost cost1 (anvil-b6e--add-cost cost2 cost3))))
                  (anvil-b6e--write
                   (expand-file-name (format "tier-a-verified-%s.answer.txt" id) anvil-b6e--raw-dir)
                   answer)
                  (anvil-b6e--write-sexp
                   (expand-file-name (format "tier-a-verified-%s.claims.sexp" id) anvil-b6e--raw-dir)
                   (list :raw-claims raw-claims :annotated claims :fallback fallback))
                  (append (list :id id :condition "verified" :answer answer
                                :wall-ms total-wall :cost-usd total-cost
                                :claims claims :fallback fallback :errored nil)
                          score))))))
      (error
       (list :id id :condition "verified" :errored t
             :error-msg (error-message-string err) :verdict 'errored
             :wall-ms nil :cost-usd nil :claims nil)))))

(defun anvil-b6e--run-tier-a (condition ids)
  (let ((cases (anvil-b6e--filter-cases anvil-bench-61-6e-tier-a-cases ids))
        (acc nil))
    (dolist (c cases)
      (message "anvil-b6e: tier-a/%s case %s starting" condition (plist-get c :id))
      (let ((r (if (string= condition "plain")
                   (anvil-b6e--tier-a-run-plain c)
                 (anvil-b6e--tier-a-run-verified c))))
        (setq acc (append acc (list r)))
        (anvil-b6e--save-checkpoint "tier-a" condition acc)
        (message "anvil-b6e: tier-a/%s case %s done verdict=%s errored=%s"
                 condition (plist-get c :id) (plist-get r :verdict) (plist-get r :errored))))
    acc))

;;;; ============================================================
;;;; Tier B
;;;; ============================================================

(defun anvil-b6e--tier-b-score (case answer-text)
  (let* ((gold (plist-get case :gold))
         (case-fold-search t)
         (text (or answer-text ""))
         (hits (mapcar (lambda (g)
                          (list :desc (plist-get g :desc)
                                :key (plist-get g :key)
                                :hit (and (string-match-p (plist-get g :key) text) t)))
                        gold)))
    (list :matched (cl-count-if (lambda (h) (plist-get h :hit)) hits)
          :total (length gold)
          :detail hits)))

(defun anvil-b6e--tier-b-run-single (case)
  (let* ((id (plist-get case :id))
         (question (plist-get case :question)))
    (condition-case err
        (with-timeout (250 (error "anvil-b6e: tier-b/single case %s timed out" id))
          (let* ((t0 (float-time))
                 (res (anvil-orchestrator-submit-and-collect
                       :provider 'claude :prompt question
                       :timeout-sec 180 :collect-timeout-sec 220 :full t))
                 (wall-ms (round (* 1000 (- (float-time) t0))))
                 (answer (plist-get res :summary))
                 (cost (plist-get res :cost-usd))
                 (score (anvil-b6e--tier-b-score case answer)))
            (anvil-b6e--write
             (expand-file-name (format "tier-b-single-%s.answer.txt" id) anvil-b6e--raw-dir)
             answer)
            (append (list :id id :condition "single" :answer answer
                          :wall-ms wall-ms :cost-usd cost :errored nil)
                    score)))
      (error
       (list :id id :condition "single" :errored t
             :error-msg (error-message-string err)
             :matched 0 :total (length (plist-get case :gold)))))))

(defun anvil-b6e--tier-b-run-fusion-plain (case)
  (let* ((id (plist-get case :id))
         (question (plist-get case :question)))
    (condition-case err
        (with-timeout (500 (error "anvil-b6e: tier-b/fusion-plain case %s timed out" id))
          (cl-destructuring-bind (fres wall-ms jcost)
              (anvil-b6e--metered
                (anvil-fusion-ask question :panel 'claude-pair :verify nil
                                   :timeout-sec 180 :max-wait-sec 300))
            (let* ((answer (plist-get fres :answer))
                   (mcost (anvil-b6e--candidates-cost (plist-get fres :candidates)))
                   (total-cost (anvil-b6e--add-cost jcost mcost))
                   (score (anvil-b6e--tier-b-score case answer)))
              (anvil-b6e--write
               (expand-file-name (format "tier-b-fusion-plain-%s.answer.txt" id) anvil-b6e--raw-dir)
               answer)
              (append (list :id id :condition "fusion-plain" :answer answer
                            :wall-ms wall-ms :cost-usd total-cost :errored nil)
                      score))))
      (error
       (list :id id :condition "fusion-plain" :errored t
             :error-msg (error-message-string err)
             :matched 0 :total (length (plist-get case :gold)))))))

(defun anvil-b6e--tier-b-run-fusion-verified (case)
  (let* ((id (plist-get case :id))
         (question (plist-get case :question)))
    (condition-case err
        (with-timeout (900 (error "anvil-b6e: tier-b/fusion-verified case %s timed out" id))
          (cl-destructuring-bind (fres wall-ms jcost)
              (anvil-b6e--metered
                (let ((anvil-fusion-verify-kb-roots anvil-b6e--kb-roots))
                  (anvil-fusion-ask question :panel 'claude-pair :verify t
                                     :timeout-sec 180 :max-wait-sec 300
                                     :verify-args (list :timeout-sec 180 :max-wait-sec 300))))
            (let* ((answer (plist-get fres :answer))
                   (claims (plist-get fres :claims))
                   (mcost (anvil-b6e--candidates-cost (plist-get fres :candidates)))
                   (total-cost (anvil-b6e--add-cost jcost mcost))
                   (score (anvil-b6e--tier-b-score case answer)))
              (anvil-b6e--write
               (expand-file-name (format "tier-b-fusion-verified-%s.answer.txt" id) anvil-b6e--raw-dir)
               answer)
              (anvil-b6e--write-sexp
               (expand-file-name (format "tier-b-fusion-verified-%s.claims.sexp" id) anvil-b6e--raw-dir)
               claims)
              (append (list :id id :condition "fusion-verified" :answer answer
                            :wall-ms wall-ms :cost-usd total-cost :claims claims :errored nil)
                      score))))
      (error
       (list :id id :condition "fusion-verified" :errored t
             :error-msg (error-message-string err)
             :matched 0 :total (length (plist-get case :gold)))))))

(defun anvil-b6e--run-tier-b (condition ids)
  (let ((cases (anvil-b6e--filter-cases anvil-bench-61-6e-tier-b-cases ids))
        (acc nil))
    (dolist (c cases)
      (message "anvil-b6e: tier-b/%s case %s starting" condition (plist-get c :id))
      (let ((r (cond
                ((string= condition "single") (anvil-b6e--tier-b-run-single c))
                ((string= condition "fusion-plain") (anvil-b6e--tier-b-run-fusion-plain c))
                ((string= condition "fusion-verified") (anvil-b6e--tier-b-run-fusion-verified c))
                (t (error "unknown tier-b condition %s" condition)))))
        (setq acc (append acc (list r)))
        (anvil-b6e--save-checkpoint "tier-b" condition acc)
        (message "anvil-b6e: tier-b/%s case %s done matched=%s/%s errored=%s"
                 condition (plist-get c :id) (plist-get r :matched) (plist-get r :total)
                 (plist-get r :errored))))
    acc))

;;;; --- summary printer ---------------------------------------------------

(defun anvil-b6e--print-summary (tier condition results)
  (princ (format "\n* %s / %s summary\n\n" tier condition))
  (if (string= tier "tier-a")
      (progn
        (princ "| id | verdict | correct-hit | wrong-hit | section-found | wall-ms | cost | errored |\n")
        (princ "|----+---------+-------------+-----------+---------------+---------+------+---------|\n")
        (dolist (r results)
          (princ (format "| %s | %s | %s | %s | %s | %s | %s | %s |\n"
                          (plist-get r :id) (plist-get r :verdict)
                          (plist-get r :gold-correct-hit) (plist-get r :gold-wrong-hit)
                          (plist-get r :section-found) (plist-get r :wall-ms)
                          (plist-get r :cost-usd) (plist-get r :errored)))))
    (progn
      (princ "| id | matched/total | wall-ms | cost | errored |\n")
      (princ "|----+---------------+---------+------+---------|\n")
      (dolist (r results)
        (princ (format "| %s | %s/%s | %s | %s | %s |\n"
                        (plist-get r :id) (plist-get r :matched) (plist-get r :total)
                        (plist-get r :wall-ms) (plist-get r :cost-usd) (plist-get r :errored))))))
  (princ "\n"))

;;;; --- entry point ---------------------------------------------------------

;; Note: Emacs leaves the literal "--" separator in
;; `command-line-args-left', so drop it before reading args.
(let* ((args (delete "--" (copy-sequence command-line-args-left))))
  (setq command-line-args-left nil)
  (let* ((tier (nth 0 args))
         (condition (nth 1 args))
         (ids (nthcdr 2 args)))
    (unless (and tier condition)
      (error "usage: emacs --batch -L . -l dataset.el -l run.el -- TIER CONDITION [ID...]"))
    (let ((results
           (cond
            ((string= tier "tier-a") (anvil-b6e--run-tier-a condition ids))
            ((string= tier "tier-b") (anvil-b6e--run-tier-b condition ids))
            (t (error "unknown tier %s (expected tier-a / tier-b)" tier)))))
      (anvil-b6e--print-summary tier condition results))))

;;; run.el ends here

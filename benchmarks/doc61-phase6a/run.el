;;; run.el --- Doc 61 section 9 extraction-fidelity benchmark runner  -*- lexical-binding: t; -*-

;; Runs every case in `anvil-bench-61-6a-cases' (dataset.el) through
;; `anvil-fusion-verify-extract-claims' under one MODEL specifier,
;; scores gold-claim recall / spurious count / kind accuracy / format
;; validity deterministically (no LLM judging), and prints an
;; org-flavored per-model report to stdout.
;;
;; Usage (repo root, live `claude' CLI -- makes real network calls,
;; costs real money, ~8 calls per invocation):
;;
;;   emacs --batch -L . \
;;     -l benchmarks/doc61-phase6a/dataset.el \
;;     -l benchmarks/doc61-phase6a/run.el -- haiku  > /tmp/6a-haiku.org
;;   emacs --batch -L . \
;;     -l benchmarks/doc61-phase6a/dataset.el \
;;     -l benchmarks/doc61-phase6a/run.el -- sonnet > /tmp/6a-sonnet.org
;;
;; MODEL is any `claude --model' specifier ("haiku", "sonnet", or a
;; full id such as "claude-haiku-4-5-20251001").  Do not loop this in
;; CI -- it is a manual/occasional fidelity check, not a regression
;; gate.
;;
;; `anvil-state-db-path' / `anvil-orchestrator-work-dir' default to
;; `user-emacs-directory' (anvil-orchestrator's normal behaviour --
;; this run participates in the same task pool/budget/state as any
;; other orchestrator caller).  To sandbox a run against a scratch
;; directory instead, pass extra --eval overrides BEFORE `-l run.el':
;;
;;   --eval '(setq anvil-state-db-path "/tmp/x/state.db")'
;;   --eval '(setq anvil-orchestrator-work-dir "/tmp/x/workdir")'
;;
;; and `cd' into a non-repo scratch directory first (task `:cwd'
;; defaults to `default-directory' at submit time, and
;; `anvil-orchestrator-worktree-auto' only triggers inside a git
;; repo).

;;; Code:

(require 'cl-lib)
(require 'anvil-orchestrator)
(require 'anvil-fusion-verify)

;;;; --- raw-text capture (distinguish "model said NONE" from "model
;;;;     broke the CLAIM:|KIND:|CANDIDATES: line format") -----------

(defvar anvil-bench-61-6a--last-result nil
  "Full plist from the most recent `anvil-orchestrator-extract-result'
call, captured via advice.  `anvil-fusion-verify-extract-claims' is a
thin wrapper around submit -> collect -> extract-result that only
returns the *parsed* claim list, discarding the raw provider summary
on the floor; this advice recovers it for diagnostics without
touching the fusion-verify module.")

(defun anvil-bench-61-6a--capture (result)
  (setq anvil-bench-61-6a--last-result result)
  result)

(advice-add 'anvil-orchestrator-extract-result :filter-return
            #'anvil-bench-61-6a--capture)

;;;; --- per-case run + score -----------------------------------------

(defun anvil-bench-61-6a--none-p (text)
  "Non-nil when TEXT (raw provider summary) is literally the NONE sentinel."
  (and (stringp text)
       (let ((case-fold-search t))
         (string-match-p "\\`[ \t\n]*none[ \t\n]*\\'" (string-trim text)))))

(defun anvil-bench-61-6a--run-case (case model)
  "Run CASE (see dataset.el) through the extraction pass under MODEL."
  (let* ((question   (plist-get case :question))
         (candidates (plist-get case :candidates))
         (gold       (plist-get case :gold))
         (t0 (float-time))
         (_  (setq anvil-bench-61-6a--last-result :unset))
         (claims (anvil-fusion-verify-extract-claims
                  question candidates
                  :provider 'claude :model model :timeout-sec 120))
         (wall-ms (round (* 1000 (- (float-time) t0))))
         (res  (if (eq anvil-bench-61-6a--last-result :unset)
                   nil anvil-bench-61-6a--last-result))
         (raw       (plist-get res :summary))
         (task-stat (plist-get res :status))
         (cost-usd  (plist-get res :cost-usd))
         (case-fold-search t)
         (kind-hits 0) (kind-total 0)
         recalled-keys spurious-claims)
    (dolist (g gold)
      (let* ((key  (plist-get g :key))
             (want (plist-get g :kind))
             (hit  (cl-find-if (lambda (c) (string-match-p key (plist-get c :claim)))
                                claims)))
        (when hit
          (push key recalled-keys)
          (cl-incf kind-total)
          (when (eq (plist-get hit :kind) want)
            (cl-incf kind-hits)))))
    (dolist (c claims)
      (unless (cl-find-if (lambda (g) (string-match-p (plist-get g :key) (plist-get c :claim)))
                           gold)
        (push c spurious-claims)))
    (list :id (plist-get case :id)
          :category (plist-get case :category)
          :model model
          :wall-ms wall-ms
          :cost-usd cost-usd
          :task-status task-stat
          :n-gold (length gold)
          :n-recalled (length recalled-keys)
          :recalled-keys (nreverse recalled-keys)
          :n-spurious (length spurious-claims)
          :spurious-claims (nreverse spurious-claims)
          :kind-hits kind-hits
          :kind-total kind-total
          :claims claims
          :raw-text raw
          :format-valid
          (cond
           (claims t)
           ((null gold) (anvil-bench-61-6a--none-p raw))
           (t (anvil-bench-61-6a--none-p raw))))))

(defun anvil-bench-61-6a--run-all (model)
  (mapcar (lambda (c) (anvil-bench-61-6a--run-case c model))
          anvil-bench-61-6a-cases))

;;;; --- aggregate + org report -----------------------------------------

(defun anvil-bench-61-6a--sum (results key)
  (apply #'+ (mapcar (lambda (r) (or (plist-get r key) 0)) results)))

(defun anvil-bench-61-6a--aggregate (results)
  (let* ((n (length results))
         (total-gold (anvil-bench-61-6a--sum results :n-gold))
         (total-recalled (anvil-bench-61-6a--sum results :n-recalled))
         (total-spurious (anvil-bench-61-6a--sum results :n-spurious))
         (kind-hits (anvil-bench-61-6a--sum results :kind-hits))
         (kind-total (anvil-bench-61-6a--sum results :kind-total))
         (format-valid-n (cl-count-if (lambda (r) (plist-get r :format-valid)) results))
         (costs (delq nil (mapcar (lambda (r) (plist-get r :cost-usd)) results))))
    (list :n n
          :total-gold total-gold
          :total-recalled total-recalled
          :recall-rate (if (> total-gold 0) (/ (float total-recalled) total-gold) 0.0)
          :total-spurious total-spurious
          :kind-accuracy (if (> kind-total 0) (/ (float kind-hits) kind-total) nil)
          :kind-hits kind-hits :kind-total kind-total
          :format-valid-n format-valid-n
          :format-valid-rate (/ (float format-valid-n) n)
          :mean-wall-ms (/ (float (anvil-bench-61-6a--sum results :wall-ms)) n)
          :total-cost-usd (if costs (apply #'+ costs) nil))))

(defun anvil-bench-61-6a--fmt-pct (x)
  (format "%.0f%%" (* 100 x)))

(defun anvil-bench-61-6a--print-report (model results agg)
  (princ (format "* Model: %s\n\n" model))
  (princ (format "| Metric | Value |\n|--------+-------|\n"))
  (princ (format "| Gold recall | %d/%d (%s) |\n"
                 (plist-get agg :total-recalled) (plist-get agg :total-gold)
                 (anvil-bench-61-6a--fmt-pct (plist-get agg :recall-rate))))
  (princ (format "| Spurious claims | %d |\n" (plist-get agg :total-spurious)))
  (princ (format "| Kind accuracy (on recalled) | %s |\n"
                 (if (plist-get agg :kind-accuracy)
                     (format "%d/%d (%s)" (plist-get agg :kind-hits) (plist-get agg :kind-total)
                             (anvil-bench-61-6a--fmt-pct (plist-get agg :kind-accuracy)))
                   "n/a (0 recalled)")))
  (princ (format "| Format validity | %d/%d (%s) |\n"
                 (plist-get agg :format-valid-n) (plist-get agg :n)
                 (anvil-bench-61-6a--fmt-pct (plist-get agg :format-valid-rate))))
  (princ (format "| Mean latency | %.0f ms |\n" (plist-get agg :mean-wall-ms)))
  (princ (format "| Total cost | %s |\n"
                 (if (plist-get agg :total-cost-usd)
                     (format "$%.4f" (plist-get agg :total-cost-usd))
                   "n/a")))
  (princ "\n** Per-case detail\n\n")
  (princ "| ID | Category | Gold | Recalled | Spurious | Kind hit/total | Fmt valid | Wall ms | Cost |\n")
  (princ "|----+----------+------+----------+----------+----------------+-----------+---------+------|\n")
  (dolist (r results)
    (princ (format "| %d | %s | %d | %d | %d | %d/%d | %s | %d | %s |\n"
                    (plist-get r :id) (plist-get r :category)
                    (plist-get r :n-gold) (plist-get r :n-recalled)
                    (plist-get r :n-spurious)
                    (plist-get r :kind-hits) (plist-get r :kind-total)
                    (if (plist-get r :format-valid) "yes" "NO")
                    (plist-get r :wall-ms)
                    (if (plist-get r :cost-usd) (format "$%.4f" (plist-get r :cost-usd)) "n/a"))))
  (princ "\n** Raw claim lines (for spurious / format-invalid cases)\n\n")
  (dolist (r results)
    (when (or (> (plist-get r :n-spurious) 0) (not (plist-get r :format-valid))
              (< (plist-get r :n-recalled) (plist-get r :n-gold)))
      (princ (format "*** case %d (%s) -- task-status=%s\n"
                      (plist-get r :id) (plist-get r :category) (plist-get r :task-status)))
      (princ (format "raw: %S\n" (plist-get r :raw-text)))
      (princ (format "parsed claims: %S\n\n" (plist-get r :claims)))))
  (princ "\n"))

;;;; --- entry point -----------------------------------------------------

;; Note: Emacs leaves the literal "--" separator in
;; `command-line-args-left', so drop it before reading MODEL.
(let* ((args (delete "--" (copy-sequence command-line-args-left)))
       (model (car args)))
  (unless model
    (error "usage: emacs --batch -L . -l dataset.el -l run.el -- MODEL"))
  (setq command-line-args-left nil)
  (let* ((results (anvil-bench-61-6a--run-all model))
         (agg (anvil-bench-61-6a--aggregate results)))
    (anvil-bench-61-6a--print-report model results agg)))

;;; run.el ends here

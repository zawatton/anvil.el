;;; anvil-fusion-eval-test.el --- ERT for anvil-fusion Phase 5 eval -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure tests for the rubric scorer, runner (stub ask-fn), aggregation,
;; comparison and tuning report.  The panel ask-fn builder is checked by
;; stubbing `anvil-fusion-ask'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion-eval)

;;;; --- scorer --------------------------------------------------------------

(ert-deftest anvil-fusion-eval-test-score-full-hit ()
  "All must-include present -> 1.0."
  (let ((s (anvil-fusion-eval-score-answer
            "地絡には地絡方向継電器(DGR)を使う"
            '(:must-include ("地絡" "継電器")))))
    (should (= 1.0 (plist-get s :score)))
    (should (= 2 (plist-get s :must-hit)))
    (should (null (plist-get s :missing)))))

(ert-deftest anvil-fusion-eval-test-score-partial ()
  "Half of must-include present -> 0.5, missing reported."
  (let ((s (anvil-fusion-eval-score-answer
            "継電器の話だけ"
            '(:must-include ("地絡" "継電器")))))
    (should (= 0.5 (plist-get s :score)))
    (should (equal '("地絡") (plist-get s :missing)))))

(ert-deftest anvil-fusion-eval-test-score-must-not-penalty ()
  "A must-not term halves the score."
  (let ((s (anvil-fusion-eval-score-answer
            "地絡 継電器 だが 誤り表現あり"
            '(:must-include ("地絡" "継電器") :must-not ("誤り表現")))))
    (should (= 0.5 (plist-get s :score)))
    (should (plist-get s :penalized))))

(ert-deftest anvil-fusion-eval-test-score-no-rubric ()
  "No must-include -> 1.0 (nothing to fail)."
  (should (= 1.0 (plist-get (anvil-fusion-eval-score-answer "anything" '()) :score))))

(ert-deftest anvil-fusion-eval-test-score-should-counted ()
  "Should-include terms are tallied but do not change :score."
  (let ((s (anvil-fusion-eval-score-answer
            "地絡 継電器 方向 零相"
            '(:must-include ("地絡") :should-include ("方向" "零相" "x")))))
    (should (= 1.0 (plist-get s :score)))
    (should (= 2 (plist-get s :should-hit)))
    (should (= 3 (plist-get s :should-total)))))

;;;; --- runner --------------------------------------------------------------

(defconst anvil-fusion-eval-test--cases
  '((:id c1 :category legal  :prompt "Pa" :must-include ("X" "Y"))
    (:id c2 :category report :prompt "Pb" :must-include ("Z"))))

(defun anvil-fusion-eval-test--stub-ask (answers)
  "ASK-FN returning canned ANSWERS keyed by prompt."
  (lambda (prompt)
    (list :answer (cdr (assoc prompt answers))
          :egress 'local-only :elapsed-ms 10)))

(ert-deftest anvil-fusion-eval-test-run-scores ()
  "Runner scores each case via the injected ask-fn."
  (let* ((ask (anvil-fusion-eval-test--stub-ask
               '(("Pa" . "X and Y here") ("Pb" . "irrelevant"))))
         (res (anvil-fusion-eval-run anvil-fusion-eval-test--cases ask)))
    (should (= 2 (length res)))
    (should (= 1.0 (plist-get (car res) :score)))     ; X+Y
    (should (= 0.0 (plist-get (cadr res) :score)))     ; Z missing
    (should (eq 'local-only (plist-get (car res) :egress)))))

;;;; --- aggregate -----------------------------------------------------------

(ert-deftest anvil-fusion-eval-test-aggregate ()
  (let* ((results '((:category legal :score 1.0 :elapsed-ms 10 :egress local-only)
                    (:category legal :score 0.0 :elapsed-ms 20 :egress local-only)
                    (:category report :score 0.5 :elapsed-ms 30 :egress external)))
         (agg (anvil-fusion-eval-aggregate results)))
    (should (= 3 (plist-get agg :n)))
    (should (= 0.5 (plist-get agg :mean-score)))
    (should (= 20.0 (plist-get agg :mean-elapsed-ms)))
    (should (= 1 (plist-get agg :egress-external)))
    (should (= 0.5 (cdr (assq 'legal (plist-get agg :by-category)))))
    (should (= 0.5 (cdr (assq 'report (plist-get agg :by-category)))))))

;;;; --- compare + tuning report ---------------------------------------------

(ert-deftest anvil-fusion-eval-test-compare-winner ()
  (let* ((cmp (anvil-fusion-eval-compare
               '((sovereign . ((:category legal :score 0.9 :egress local-only)))
                 (quality   . ((:category legal :score 0.7 :egress external)))))))
    (should (eq 'sovereign (plist-get cmp :winner)))
    (should (equal '(sovereign quality)
                   (mapcar #'car (plist-get cmp :ranking))))))

(ert-deftest anvil-fusion-eval-test-tuning-report-text ()
  (let* ((cmp (anvil-fusion-eval-compare
               '((sovereign . ((:category legal :score 0.8 :egress local-only)))
                 (quality   . ((:category legal :score 0.6 :egress external))))))
         (rep (anvil-fusion-eval-tuning-report cmp)))
    (should (string-match-p "winner: sovereign" rep))
    (should (string-match-p "sovereign: mean=0.800" rep))
    (should (string-match-p "0 egress" rep))))

;;;; --- panel ask-fn builder (stub anvil-fusion-ask) -------------------------

(ert-deftest anvil-fusion-eval-test-panel-ask-fn-normalizes ()
  "The panel ask-fn forwards to anvil-fusion-ask and normalizes shape."
  (cl-letf (((symbol-function 'anvil-fusion-ask)
             (lambda (prompt &rest args)
               (should (equal "QQ" prompt))
               (should (eq 'sovereign (plist-get args :panel)))
               (list :answer "FUSED" :egress 'local-only))))
    (let* ((fn (anvil-fusion-eval-panel-ask-fn 'sovereign :fidelity 'full))
           (out (funcall fn "QQ")))
      (should (equal "FUSED" (plist-get out :answer)))
      (should (eq 'local-only (plist-get out :egress)))
      (should (integerp (plist-get out :elapsed-ms))))))

(provide 'anvil-fusion-eval-test)
;;; anvil-fusion-eval-test.el ends here

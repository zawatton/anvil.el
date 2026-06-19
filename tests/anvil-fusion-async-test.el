;;; anvil-fusion-async-test.el --- ERT for anvil-fusion async ask -*- lexical-binding: t; -*-

;;; Commentary:
;; State-machine tests for the non-blocking ask.  orchestrator is faked:
;; submit returns sequential batch ids, status reports running/terminal
;; counts (+ :tasks), extract-result returns the fused answer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion)
(require 'anvil-fusion-panels)
(or (require 'anvil-orchestrator nil t) ;; load the real module when present (anvil.el)
    (provide 'anvil-orchestrator))  ;; else fake it (fusion standalone repo)
(require 'anvil-fusion-async)

;; Terminal flags toggled per batch id so a poll can move running->done.
(defvar anvil-fusion-async-test--terminal nil
  "Alist BATCH-ID -> terminal-p used by the fake orchestrator-status.")

(defmacro anvil-fusion-async-test--with (&rest body)
  "Run BODY with a faked orchestrator + a clean job table."
  (declare (indent 0) (debug t))
  `(let ((calls 0)
         (anvil-fusion--jobs (make-hash-table :test 'equal))
         (anvil-fusion-async-test--terminal nil))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (_tasks) (setq calls (1+ calls)) (format "b%d" calls)))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (let ((term (cdr (assoc id anvil-fusion-async-test--terminal))))
                    (list :running (if term 0 1) :queued 0
                          :tasks '((:id "m1" :provider ollama :status done
                                        :summary "候補A")
                                   (:id "m2" :provider ollama :status done
                                        :summary "候補B"))))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (_id _full) (list :summary "FUSED-ASYNC"))))
       ,@body)))

(defun anvil-fusion-async-test--mark-terminal (batch-id)
  (push (cons batch-id t) anvil-fusion-async-test--terminal))

(ert-deftest anvil-fusion-async-test-submit-returns-job ()
  "ask-async returns a job id immediately and registers the job."
  (anvil-fusion-async-test--with
    (let ((j (anvil-fusion-ask-async "Q" :panel 'sovereign)))
      (should (string-prefix-p "fusion-" (plist-get j :job-id)))
      (should (eq 'members (plist-get j :stage)))
      (should (eq 'local-only (plist-get j :egress)))
      (should (gethash (plist-get j :job-id) anvil-fusion--jobs)))))

(ert-deftest anvil-fusion-async-test-members-running ()
  "While members run, result reports running/members and submits no judge."
  (anvil-fusion-async-test--with
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign))
           (id (plist-get j :job-id))
           (r (anvil-fusion-result id)))
      (should (eq 'running (plist-get r :status)))
      (should (eq 'members (plist-get r :stage)))
      (should (= 1 calls)))))            ; only the member submit so far

(ert-deftest anvil-fusion-async-test-full-lifecycle ()
  "members terminal -> judging -> done with the fused answer."
  (anvil-fusion-async-test--with
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign))
           (id (plist-get j :job-id)))
      ;; members terminal -> next poll submits the judge
      (anvil-fusion-async-test--mark-terminal "b1")
      (let ((r1 (anvil-fusion-result id)))
        (should (eq 'judging (plist-get r1 :stage)))
        (should (= 2 calls)))            ; judge submitted
      ;; judge still running
      (should (eq 'running (plist-get (anvil-fusion-result id) :status)))
      ;; judge terminal -> done
      (anvil-fusion-async-test--mark-terminal "b2")
      (let ((r2 (anvil-fusion-result id)))
        (should (eq 'done (plist-get r2 :status)))
        (should (equal "FUSED-ASYNC" (plist-get r2 :answer)))
        (should (= 2 (plist-get r2 :n-candidates))))
      ;; done is cached / idempotent
      (should (eq 'done (plist-get (anvil-fusion-result id) :status))))))

(ert-deftest anvil-fusion-async-test-unknown-job ()
  (anvil-fusion-async-test--with
    (should-error (anvil-fusion-result "fusion-nope") :type 'user-error)))

(ert-deftest anvil-fusion-async-test-sovereignty-guard ()
  "A non-local judge override on a local-only panel is refused (no submit)."
  (anvil-fusion-async-test--with
    (should-error (anvil-fusion-ask-async "Q" :panel 'sovereign :judge 'claude)
                  :type 'user-error)
    (should (= 0 calls))))

(provide 'anvil-fusion-async-test)
;;; anvil-fusion-async-test.el ends here

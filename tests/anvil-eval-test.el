;;; anvil-eval-test.el --- Tests for anvil eval helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for async eval result formatting.

;;; Code:

(require 'ert)
(require 'anvil-eval)

(defun anvil-eval-test--time-ago (seconds)
  "Return a timestamp SECONDS before now."
  (time-subtract (current-time) (seconds-to-time seconds)))

(ert-deftest anvil-eval-test-result-reports-runtime-for-finished-job ()
  "Finished async jobs report runtime separately from job age."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (let* ((start (anvil-eval-test--time-ago 12))
            (run-start (time-add start (seconds-to-time 10)))
            (finish (time-add run-start (seconds-to-time 0.4))))
       (list :status 'done
             :result "\"ok\""
             :expression "(progn \"ok\")"
             :start-time start
             :run-start-time run-start
             :finish-time finish
             :queue-wait-sec 10.0
             :runtime-sec 0.4))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: done" out))
      (should (string-match-p "^elapsed: [0-9.]+s" out))
      (should (string-match-p "^age: [0-9.]+s" out))
      (should (string-match-p "^queue-wait: 10.0s" out))
      (should (string-match-p "^runtime: 0.4s" out))
      (should (string-match-p "^result: \"ok\"" out)))))

(ert-deftest anvil-eval-test-result-running-before-start-has-no-runtime ()
  "Queued async jobs have age and queue wait, but no runtime."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (list :status 'running
           :result nil
           :expression "(sleep-for 1)"
           :start-time (anvil-eval-test--time-ago 3))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: running" out))
      (should (string-match-p "^queue-wait: [0-9.]+s" out))
      (should (string-match-p "^runtime: N/A" out))
      (should (string-match-p "^result: N/A" out)))))

(ert-deftest anvil-eval-test-result-running-after-start-reports-runtime ()
  "Running async jobs report runtime since their run-start time."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (let* ((start (anvil-eval-test--time-ago 4))
            (run-start (time-add start (seconds-to-time 2))))
       (list :status 'running
             :result nil
             :expression "(sleep-for 10)"
             :start-time start
             :run-start-time run-start))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: running" out))
      (should (string-match-p "^queue-wait: 2.0s" out))
      (should (string-match-p "^runtime: [0-9.]+s" out))
      (should-not (string-match-p "^runtime: N/A" out)))))

(ert-deftest anvil-eval-test-async-records-runtime-fields ()
  "The async timer path records queue wait and runtime fields."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal))
        (anvil-eval--async-counter 0))
    (let* ((started (anvil-eval--async "(+ 1 2)"))
           (job-id (replace-regexp-in-string "\\`Job started: " "" started))
           (deadline (+ (float-time) 1.0)))
      (while (and (< (float-time) deadline)
                  (eq 'running
                      (plist-get (gethash job-id anvil-eval--async-jobs)
                                 :status)))
        (accept-process-output nil 0.01))
      (let ((job (gethash job-id anvil-eval--async-jobs)))
        (should (eq 'done (plist-get job :status)))
        (should (numberp (plist-get job :queue-wait-sec)))
        (should (numberp (plist-get job :runtime-sec)))
        (should (equal "3" (plist-get job :result))))
      (let ((out (anvil-eval--result job-id)))
        (should (string-match-p "^queue-wait: [0-9.]+s" out))
        (should (string-match-p "^runtime: [0-9.]+s" out))))))

(provide 'anvil-eval-test)
;;; anvil-eval-test.el ends here

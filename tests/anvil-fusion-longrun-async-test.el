;;; anvil-fusion-longrun-async-test.el --- tests for the async quest runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 3 tests.  The orchestrator is stubbed via cl-letf so the full
;; submit -> poll -> done lifecycle runs with no real model: submit
;; records each batch's task name, status reports terminal, and
;; extract-result returns canned step / distill output (distill output
;; carries the STATUS marker that drives termination).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-fusion-longrun-async)

(defun anvil-fusion-longrun-async-test--tmpdb ()
  (anvil-fusion-longrun-store-open (make-temp-file "alr-async-" nil ".db")))

(defun anvil-fusion-longrun-async-test--drain (job-id &optional cap)
  "Poll JOB-ID until terminal (or CAP polls); return the last status."
  (let ((cap (or cap 60)) r)
    (while (and (> cap 0)
                (setq r (anvil-fusion-longrun-status job-id))
                (not (memq (plist-get r :status) '(done error))))
      (cl-decf cap))
    r))

;; Stub env: orchestrator-submit maps batch -> task name; status is
;; always terminal; extract-result returns canned output keyed on whether
;; the batch was a step or a distill (distill carries STATUS).
(defmacro anvil-fusion-longrun-async-test--with-stub (status-verdict &rest body)
  "Run BODY with the orchestrator stubbed.
STATUS-VERDICT is an elisp form evaluated per `anvil-orchestrator-status'
call returning the status plist; bind `distill-status' is the STATUS
word emitted by the distill stub (\"CONTINUE\" / \"DONE\")."
  (declare (indent 1))
  `(let ((--counter 0)
         (--names (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (cl-incf --counter)
                  (let ((bid (format "batch-%d" --counter)))
                    (puthash bid (plist-get (car tasks) :name) --names)
                    bid)))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (_id) ,status-verdict))
               ((symbol-function 'anvil-fusion--batch-first-task-id)
                (lambda (bid) bid))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (bid &optional _full)
                  (let ((name (gethash bid --names)))
                    (if (string-prefix-p "lr-distill" name)
                        ;; distinct digest per step so the convergence detector
                        ;; (Phase 4) does not fire on near-identical strings.
                        (let* ((k (string-to-number
                                   (or (car (last (split-string name "-"))) "0")))
                               (words '("alpha-apple" "bravo-river" "charlie-tower"
                                        "delta-cloud" "echo-stone" "foxtrot-maple"))
                               (w (nth (mod k (length words)) words)))
                          (list :summary (format "DIGEST-%s\nSTATUS: %s" w distill-status)))
                      (list :summary (format "stepout-%s" name)))))))
       ,@body)))

(ert-deftest anvil-fusion-longrun-async-test-lifecycle-budget ()
  (let ((distill-status "CONTINUE")
        (db (anvil-fusion-longrun-async-test--tmpdb)))
    (anvil-fusion-longrun-async-test--with-stub
        (list :running 0 :queued 0)
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 2 :db db))
             (job-id (plist-get s :job-id))
             (r (anvil-fusion-longrun-async-test--drain job-id)))
        (should (equal (plist-get s :stage) 'step-running))
        (should (eq (plist-get r :status) 'done))
        (should (= (plist-get r :steps) 2))
        (should (eq (plist-get r :stopped) 'budget))
        (should (string-prefix-p "DIGEST-" (plist-get r :answer)))
        ;; persisted: quest at step 2 with status budget, 2 step rows
        (let ((q (anvil-fusion-longrun-store-get db (plist-get s :quest-id))))
          (should (= (plist-get q :step) 2))
          (should (equal (plist-get q :status) "budget")))
        (should (= (length (anvil-fusion-longrun-store-steps
                            db (plist-get s :quest-id)))
                   2))))))

(ert-deftest anvil-fusion-longrun-async-test-early-done ()
  (let ((distill-status "DONE")
        (db (anvil-fusion-longrun-async-test--tmpdb)))
    (anvil-fusion-longrun-async-test--with-stub
        (list :running 0 :queued 0)
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 9 :db db))
             (r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
        (should (eq (plist-get r :status) 'done))
        (should (= (plist-get r :steps) 1))
        (should (eq (plist-get r :stopped) 'done))
        (should (equal (plist-get (anvil-fusion-longrun-store-get
                                   db (plist-get s :quest-id))
                                  :status)
                       "done"))))))

(ert-deftest anvil-fusion-longrun-async-test-running-does-not-advance ()
  ;; status reports a still-running batch on the first poll -> no advance.
  (let ((distill-status "CONTINUE")
        (db (anvil-fusion-longrun-async-test--tmpdb))
        (polls 0))
    (anvil-fusion-longrun-async-test--with-stub
        (progn (cl-incf polls)
               (if (= polls 1) (list :running 1 :queued 0)
                 (list :running 0 :queued 0)))
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 1 :db db))
             (r1 (anvil-fusion-longrun-status (plist-get s :job-id))))
        (should (eq (plist-get r1 :status) 'running))
        (should (eq (plist-get r1 :stage) 'step-running))
        ;; subsequent polls (terminal) drive it to done
        (let ((r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
          (should (eq (plist-get r :status) 'done))
          (should (= (plist-get r :steps) 1)))))))

(ert-deftest anvil-fusion-longrun-async-test-resume ()
  (let ((distill-status "CONTINUE")
        (db (anvil-fusion-longrun-async-test--tmpdb)))
    (anvil-fusion-longrun-async-test--with-stub
        (list :running 0 :queued 0)
      ;; phase A: budget 2
      (let* ((a (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 2 :db db))
             (qid (plist-get a :quest-id)))
        (anvil-fusion-longrun-async-test--drain (plist-get a :job-id))
        (should (= (plist-get (anvil-fusion-longrun-store-get db qid) :step) 2))
        ;; phase B: resume to budget 4
        (let* ((b (anvil-fusion-longrun-resume-async qid :max-steps 4 :db db))
               (rb (anvil-fusion-longrun-async-test--drain (plist-get b :job-id))))
          (should (= (plist-get b :resumed-from) 2))
          (should (eq (plist-get rb :status) 'done))
          (should (= (plist-get rb :steps) 4))
          (should (= (length (anvil-fusion-longrun-store-steps db qid)) 4)))))))

(ert-deftest anvil-fusion-longrun-async-test-unknown-job ()
  (should-error (anvil-fusion-longrun-status "nope")))

(ert-deftest anvil-fusion-longrun-async-test-resume-unknown-quest ()
  (let ((db (anvil-fusion-longrun-async-test--tmpdb)))
    (should-error (anvil-fusion-longrun-resume-async "nope" :db db))))

(ert-deftest anvil-fusion-longrun-async-test-converges ()
  "Async quest ends early (converged) when the digest stops changing."
  (let ((db (anvil-fusion-longrun-async-test--tmpdb))
        (--counter 0) (--names (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (cl-incf --counter)
                 (let ((bid (format "batch-%d" --counter)))
                   (puthash bid (plist-get (car tasks) :name) --names) bid)))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id) (list :running 0 :queued 0)))
              ((symbol-function 'anvil-fusion--batch-first-task-id) (lambda (bid) bid))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (bid &optional _full)
                 (if (string-prefix-p "lr-distill" (gethash bid --names))
                     (list :summary "STABLE STATE\nSTATUS: CONTINUE") ; identical -> stagnation
                   (list :summary "stepout")))))
      (let* ((s (anvil-fusion-longrun-start-async "G" :provider 'ollama :max-steps 9 :db db))
             (r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
        (should (eq (plist-get r :status) 'done))
        (should (eq (plist-get r :stopped) 'converged))
        (should (= (plist-get r :steps) 3))))))

(ert-deftest anvil-fusion-longrun-async-test-hermetic-allowed-tools ()
  "start-async :hermetic puts the disclosure allow-list + instruction on the step."
  (let ((db (anvil-fusion-longrun-async-test--tmpdb)) captured)
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks) (push (car tasks) captured) "batch-x")))
      (anvil-fusion-longrun-start-async "G" :provider 'claude :max-steps 3
                                       :db db :hermetic t)
      (let ((task (car captured)))
        (should (string-match-p "mcp__emacs-eval-ultra__file-outline"
                                (plist-get task :allowed-tools)))
        (should (string-match-p "読み取り専用" (plist-get task :prompt)))))))

(provide 'anvil-fusion-longrun-async-test)
;;; anvil-fusion-longrun-async-test.el ends here

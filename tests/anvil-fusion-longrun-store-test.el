;;; anvil-fusion-longrun-store-test.el --- tests for longrun SQLite store -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 2 tests.  Each test opens a fresh temp-file DB and drives the
;; loop with injected step-fn / distill-fn (no orchestrator, no model).
;; The focus is persistence + interrupt/resume from a checkpoint.

;;; Code:

(require 'ert)
(require 'anvil-fusion-longrun-store)

(defun anvil-fusion-longrun-store-test--tmpdb ()
  "Open a fresh temp-file quest store and return the handle."
  (anvil-fusion-longrun-store-open (make-temp-file "alr-store-" nil ".db")))

(defun anvil-fusion-longrun-store-test--distill (body)
  "Return a distill-fn emitting BODY-<n> and STATUS: CONTINUE."
  (lambda (_p n) (format "%s-%d\nSTATUS: CONTINUE" body n)))

(ert-deftest anvil-fusion-longrun-store-test-create-get ()
  (let* ((db (anvil-fusion-longrun-store-test--tmpdb))
         (id (anvil-fusion-longrun-store-create
              db :goal "G" :provider 'ollama :max-steps 5)))
    (let ((q (anvil-fusion-longrun-store-get db id)))
      (should (equal (plist-get q :goal) "G"))
      (should (equal (plist-get q :status) "running"))
      (should (= (plist-get q :step) 0))
      (should (equal (plist-get q :provider) "ollama")))))

(ert-deftest anvil-fusion-longrun-store-test-get-missing ()
  (let ((db (anvil-fusion-longrun-store-test--tmpdb)))
    (should-not (anvil-fusion-longrun-store-get db "nope"))))

(ert-deftest anvil-fusion-longrun-store-test-checkpoint ()
  (let* ((db (anvil-fusion-longrun-store-test--tmpdb))
         (id (anvil-fusion-longrun-store-create db :goal "G")))
    (anvil-fusion-longrun-store-checkpoint
     db id 1 "DIGEST-1" "running"
     (list :output-chars 10 :digest-chars 8 :digest-head "DIGEST-1" :done nil))
    (let ((q (anvil-fusion-longrun-store-get db id))
          (steps (anvil-fusion-longrun-store-steps db id)))
      (should (= (plist-get q :step) 1))
      (should (equal (plist-get q :digest) "DIGEST-1"))
      (should (= (length steps) 1))
      (should (= (plist-get (car steps) :step) 1))
      (should (= (plist-get (car steps) :digest-chars) 8)))))

(ert-deftest anvil-fusion-longrun-store-test-start-persists-each-step ()
  (let* ((db (anvil-fusion-longrun-store-test--tmpdb))
         (r (anvil-fusion-longrun-start
             "G" :db db :max-steps 3
             :step-fn (lambda (_p _n) "out")
             :distill-fn (anvil-fusion-longrun-store-test--distill "D"))))
    (should (= (plist-get r :steps) 3))
    (should (eq (plist-get r :stopped) 'budget))
    (let* ((id (plist-get r :id))
           (q (anvil-fusion-longrun-store-get db id))
           (steps (anvil-fusion-longrun-store-steps db id)))
      (should (equal (plist-get q :status) "budget"))
      (should (= (plist-get q :step) 3))
      (should (equal (plist-get q :digest) "D-3"))
      (should (= (length steps) 3)))))

(ert-deftest anvil-fusion-longrun-store-test-resume-continues-from-checkpoint ()
  (let* ((db (anvil-fusion-longrun-store-test--tmpdb))
         ;; phase A: run is "interrupted" by hitting the budget at step 2
         (a (anvil-fusion-longrun-start
             "G" :db db :max-steps 2
             :step-fn (lambda (_p _n) "out")
             :distill-fn (anvil-fusion-longrun-store-test--distill "D")))
         (id (plist-get a :id)))
    (should (= (plist-get a :steps) 2))
    (should (equal (plist-get (anvil-fusion-longrun-store-get db id) :digest) "D-2"))
    ;; phase B: resume with a raised cap; capture the step prompts seen
    (let* ((seen nil)
           (b (anvil-fusion-longrun-resume
               id :db db :max-steps 4
               :step-fn (lambda (p _n) (push p seen) "out")
               :distill-fn (anvil-fusion-longrun-store-test--distill "D"))))
      (setq seen (nreverse seen))
      (should (= (plist-get b :steps) 4))
      ;; the first resumed step (step 3) must see the SAVED digest D-2
      (should (string-match-p "D-2" (nth 0 seen)))
      ;; exactly two new steps ran (3 and 4), not a restart from 1
      (should (= (length seen) 2))
      ;; store now holds 4 distinct steps and the quest advanced to step 4
      (should (= (length (anvil-fusion-longrun-store-steps db id)) 4))
      (should (= (plist-get (anvil-fusion-longrun-store-get db id) :step) 4)))))

(ert-deftest anvil-fusion-longrun-store-test-resume-unknown-errors ()
  (let ((db (anvil-fusion-longrun-store-test--tmpdb)))
    (should-error (anvil-fusion-longrun-resume "nope" :db db))))

(ert-deftest anvil-fusion-longrun-store-test-done-status ()
  (let* ((db (anvil-fusion-longrun-store-test--tmpdb))
         (r (anvil-fusion-longrun-start
             "G" :db db :max-steps 9
             :step-fn (lambda (_p _n) "out")
             :distill-fn (lambda (_p n)
                           (format "D-%d\nSTATUS: %s"
                                   n (if (>= n 2) "DONE" "CONTINUE"))))))
    (should (eq (plist-get r :stopped) 'done))
    (should (= (plist-get r :steps) 2))
    (should (equal (plist-get (anvil-fusion-longrun-store-get db (plist-get r :id))
                              :status)
                   "done"))))

(ert-deftest anvil-fusion-longrun-store-test-list ()
  (let* ((db (anvil-fusion-longrun-store-test--tmpdb)))
    (anvil-fusion-longrun-store-create db :goal "alpha")
    (anvil-fusion-longrun-store-create db :goal "beta")
    (let ((rows (anvil-fusion-longrun-store-list db :limit 10)))
      (should (= (length rows) 2))
      (should (cl-every (lambda (r) (plist-get r :goal-head)) rows)))))

(provide 'anvil-fusion-longrun-store-test)
;;; anvil-fusion-longrun-store-test.el ends here

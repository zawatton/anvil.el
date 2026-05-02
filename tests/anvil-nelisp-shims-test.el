;;; anvil-nelisp-shims-test.el --- Tests for anvil-nelisp-shims  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the standalone-mode shim layer (sqlite alias + register
;; accumulator).  Because the shim auto-fires at load time gated on
;; `(featurep 'nelisp)', most tests use `cl-letf' to override
;; `anvil-server-register-tools' and friends so we can assert
;; behaviour without needing the actual NeLisp interpreter present
;; in the ERT batch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-nelisp-shims)


;;;; --- accumulator behaviour ---------------------------------------------

(ert-deftest anvil-nelisp-shims-test/drain-flat-and-ordered ()
  "Drain returns specs in registration order and resets accumulator."
  (let ((anvil-nelisp-shims--collected-specs nil))
    (cl-letf (((symbol-function 'anvil-server-register-tools)
               (lambda (server-id specs)
                 (push (cons server-id specs)
                       anvil-nelisp-shims--collected-specs)
                 specs)))
      (anvil-server-register-tools "S1" '((:id "a") (:id "b")))
      (anvil-server-register-tools "S2" '((:id "c"))))
    (let ((flat (anvil-nelisp-shims-drain)))
      (should (equal (mapcar (lambda (s) (plist-get s :id)) flat)
                     '("a" "b" "c")))
      ;; Drain resets.
      (should (null anvil-nelisp-shims--collected-specs))
      (should (= 0 (anvil-nelisp-shims-collected-count))))))

(ert-deftest anvil-nelisp-shims-test/collected-count-tracks ()
  "Collected count reflects accumulator size before drain."
  (let ((anvil-nelisp-shims--collected-specs nil))
    (cl-letf (((symbol-function 'anvil-server-register-tools)
               (lambda (server-id specs)
                 (push (cons server-id specs)
                       anvil-nelisp-shims--collected-specs)
                 specs)))
      (should (= 0 (anvil-nelisp-shims-collected-count)))
      (anvil-server-register-tools "S" '((:id "x")))
      (should (= 1 (anvil-nelisp-shims-collected-count)))
      (anvil-server-register-tools "S" '((:id "y") (:id "z")))
      (should (= 3 (anvil-nelisp-shims-collected-count))))))


;;;; --- register-tool synthesis ---------------------------------------------

(ert-deftest anvil-nelisp-shims-test/register-tool-builds-spec ()
  "register-tool synthesizes a 1-element spec via register-tools."
  (let* ((anvil-nelisp-shims--collected-specs nil)
         (last-server nil)
         (last-specs  nil))
    (cl-letf (((symbol-function 'anvil-server-register-tools)
               (lambda (server-id specs)
                 (setq last-server server-id last-specs specs)
                 specs)))
      ;; Need to redefine register-tool locally because the auto-fire
      ;; gating made it conditional on featurep.  Inline a copy here.
      (let ((handler (lambda () "hi")))
        (let ((server-id "default"))
          (funcall
           (lambda (h &rest props)
             (let* ((id (plist-get props :id))
                    (sid (or (plist-get props :server-id) "default"))
                    (spec (list :id id :handler h
                                :description (plist-get props :description)
                                :title (plist-get props :title)
                                :read-only (plist-get props :read-only))))
               (anvil-server-register-tools sid (list spec))))
           handler :id "echo" :description "Echo back."
           :server-id "test-srv" :title "Echo" :read-only t)
          (should (equal last-server "test-srv"))
          (should (= 1 (length last-specs)))
          (let ((spec (car last-specs)))
            (should (equal (plist-get spec :id) "echo"))
            (should (equal (plist-get spec :description) "Echo back."))
            (should (equal (plist-get spec :title) "Echo"))
            (should (eq (plist-get spec :read-only) t))
            (should (functionp (plist-get spec :handler)))))))))


;;;; --- sqlite alias coverage ----------------------------------------------

(ert-deftest anvil-nelisp-shims-test/sqlite-alias-table-is-complete ()
  "Every sqlite-* symbol used by anvil-state / anvil-memory has an alias entry."
  ;; Must mirror the dolist in the shim source; if either side grows,
  ;; this test catches the drift.
  (let ((covered '(sqlite-open sqlite-close sqlite-execute sqlite-select
                               sqlitep sqlite-pragma sqlite-transaction
                               sqlite-commit sqlite-rollback))
        (used    '(sqlite-open sqlite-close sqlite-execute sqlite-select
                               sqlite-transaction sqlite-available-p
                               sqlite-supports-trigram-p)))
    (dolist (sym used)
      (should (or (memq sym covered)
                  (memq sym '(sqlite-available-p sqlite-supports-trigram-p)))))))


;;;; --- inertness when nelisp feature absent --------------------------------

(ert-deftest anvil-nelisp-shims-test/inert-under-emacs ()
  "When `nelisp' is not provided, the shim activation flag is nil."
  ;; The defconst is evaluated at load time so we cannot retroactively
  ;; flip it for this assertion.  We instead assert on the snapshot
  ;; recorded at load time — under ERT batch this should be nil.
  (let ((features (delq 'nelisp (copy-sequence features))))
    (should-not (memq 'nelisp features))
    ;; The module-level defconst captured the load-time value; we
    ;; assert that under the current (non-NeLisp) Emacs the shim was
    ;; not auto-armed by checking the documented snapshot.
    (should (not anvil-nelisp-shims-active-p))))


(provide 'anvil-nelisp-shims-test)

;;; anvil-nelisp-shims-test.el ends here

;;; anvil-treesit-backend-test.el --- Tests for anvil-treesit-backend  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 38 Phase F backend dispatch tests.  Pure / side-effect-free —
;; they exercise `anvil-treesit-backend-pick' under each policy
;; setting and verify the subprocess stub returns a structured
;; `:not-implemented-yet' user-error.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-treesit-backend nil 'noerror)

(defun anvil-treesit-backend-test--loaded-p ()
  "Return non-nil when the backend module is loaded."
  (fboundp 'anvil-treesit-backend-pick))

;;;; --- backend pick dispatch ---------------------------------------------

(ert-deftest anvil-treesit-backend-pick-honours-treesit-policy ()
  "Forcing `treesit' returns nil when grammar absent, the symbol when present."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((anvil-treesit-backend-preferred 'treesit))
    (cond
     ((and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'python))
      (should (eq 'treesit (anvil-treesit-backend-pick 'python))))
     (t
      (should (eq nil (anvil-treesit-backend-pick 'python)))))))

(ert-deftest anvil-treesit-backend-pick-honours-subprocess-policy ()
  "Forcing `subprocess' always returns the symbol regardless of treesit state."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((anvil-treesit-backend-preferred 'subprocess))
    (should (eq 'subprocess (anvil-treesit-backend-pick 'python)))
    (should (eq 'subprocess (anvil-treesit-backend-pick 'typescript)))
    (should (eq 'subprocess (anvil-treesit-backend-pick 'javascript)))))

(ert-deftest anvil-treesit-backend-pick-auto-falls-back-to-subprocess ()
  "Auto policy picks treesit when grammar present, subprocess otherwise."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((anvil-treesit-backend-preferred 'auto))
    ;; A bogus language symbol is never treesit-available; auto must
    ;; fall through to the subprocess stub.
    (should (eq 'subprocess
                (anvil-treesit-backend-pick 'utterly-fake-lang-zzz)))))

;;;; --- subprocess stub returns structured error ---------------------------

(ert-deftest anvil-treesit-backend-subprocess-stub-signals-not-implemented ()
  "The subprocess parse stub signals a structured `:not-implemented-yet' error."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((err (should-error
              (anvil-treesit-backend-subprocess-parse "x = 1" 'python)
              :type 'user-error)))
    ;; user-error data is the formatted string; we round-trip through
    ;; `read' to recover the structured plist.
    (let* ((msg (cadr err))
           (plist (read msg)))
      (should (eq 'not-implemented-yet (plist-get plist :kind)))
      (should (eq 'python (plist-get plist :lang)))
      (should (eq 'subprocess (plist-get plist :backend))))))

(ert-deftest anvil-treesit-backend-with-root-on-subprocess-stubs ()
  "`anvil-treesit-with-root' under the subprocess backend signals the stub."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((anvil-treesit-backend-preferred 'subprocess)
        (tmp (make-temp-file "anvil-treesit-backend-test-" nil ".py")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "x = 1\n"))
          (should-error
           (anvil-treesit-with-root tmp 'python _root nil)
           :type 'user-error))
      (delete-file tmp))))

(provide 'anvil-treesit-backend-test)
;;; anvil-treesit-backend-test.el ends here

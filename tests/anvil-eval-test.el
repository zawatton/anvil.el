;;; anvil-eval-test.el --- Tests for anvil-eval NeLisp backend -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for the architecture α NeLisp interpreter backend
;; (`anvil-eval--nelisp' + `anvil-eval--locate-nelisp-binary' + the
;; `nelisp-eval' MCP tool registration).
;;
;; All tests skip cleanly when the NeLisp `nelisp' binary cannot be
;; located so the suite is safe to ship in CI on hosts that have not
;; built the Rust runtime.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-eval)


;;;; --- guards -------------------------------------------------------------

(defun anvil-eval-test--nelisp-bin-or-skip ()
  "Return the located `nelisp' binary path, or skip the calling test."
  (let ((anvil-eval--nelisp-binary-cache 'unset))
    (or (anvil-eval--locate-nelisp-binary)
        (ert-skip "nelisp binary not found on this host"))))


;;;; --- locate-nelisp-binary ----------------------------------------------

(ert-deftest anvil-eval-test-locate-honours-explicit-override ()
  "When `anvil-eval-nelisp-binary' points at an existing executable
the resolver returns it without consulting PATH or env vars."
  (let* ((tmp (make-temp-file "anvil-eval-test-bin-")))
    (unwind-protect
        (progn
          (set-file-modes tmp #o755)
          (let ((anvil-eval-nelisp-binary tmp)
                (anvil-eval--nelisp-binary-cache 'unset)
                (process-environment
                 (cons "NELISP_BIN=/nonexistent/should-not-be-used"
                       process-environment)))
            (should (equal tmp (anvil-eval--locate-nelisp-binary)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest anvil-eval-test-locate-honours-env-var ()
  "When `anvil-eval-nelisp-binary' is nil and $NELISP_BIN points at an
existing executable the resolver picks it up."
  (let* ((tmp (make-temp-file "anvil-eval-test-envbin-")))
    (unwind-protect
        (progn
          (set-file-modes tmp #o755)
          (let ((anvil-eval-nelisp-binary nil)
                (anvil-eval--nelisp-binary-cache 'unset)
                (process-environment
                 (cons (concat "NELISP_BIN=" tmp) process-environment)))
            (should (equal tmp (anvil-eval--locate-nelisp-binary)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest anvil-eval-test-locate-caches-result ()
  "Second call returns the same path without re-probing."
  (anvil-eval-test--nelisp-bin-or-skip)
  (let ((first (anvil-eval--locate-nelisp-binary))
        (second (anvil-eval--locate-nelisp-binary)))
    (should (equal first second))
    (should (stringp first))))


;;;; --- anvil-eval--nelisp -------------------------------------------------

(ert-deftest anvil-eval-test-nelisp-arithmetic ()
  "`(+ 1 2)' on the NeLisp interpreter returns \"3\"."
  (anvil-eval-test--nelisp-bin-or-skip)
  (should (equal "3" (anvil-eval--nelisp "(+ 1 2)"))))

(ert-deftest anvil-eval-test-nelisp-keyword-self-eval ()
  "Keywords self-evaluate on NeLisp (Phase 8.x fix shipped 2026-04-27)."
  (anvil-eval-test--nelisp-bin-or-skip)
  (should (equal ":foo" (anvil-eval--nelisp ":foo"))))

(ert-deftest anvil-eval-test-nelisp-string-len ()
  "Round-trip a string operation through the subprocess."
  (anvil-eval-test--nelisp-bin-or-skip)
  (should (equal "5" (anvil-eval--nelisp "(length \"hello\")"))))

(ert-deftest anvil-eval-test-nelisp-missing-binary-errors ()
  "When no binary can be located the eval helper signals an error.
Force the cache to a `nil' result so the resolver short-circuits to
\"not found\" without consulting PATH or env vars."
  (let ((anvil-eval--nelisp-binary-cache nil))
    (should-error (anvil-eval--nelisp "(+ 1 2)") :type 'error)))


;;;; --- tool registration --------------------------------------------------

(ert-deftest anvil-eval-test-nelisp-tool-registered ()
  "`anvil-eval-enable' registers a `nelisp-eval' tool that
`anvil-eval-disable' removes again.  Probes via the registry hash
table directly since anvil-server has no public `tool-registered-p'."
  (unwind-protect
      (progn
        (anvil-eval-enable)
        (should (gethash "nelisp-eval"
                         (anvil-server--get-server-tools
                          anvil-eval--server-id))))
    (anvil-eval-disable)
    (should-not (gethash "nelisp-eval"
                         (anvil-server--get-server-tools
                          anvil-eval--server-id)))))


(provide 'anvil-eval-test)
;;; anvil-eval-test.el ends here

;;; anvil-ide-elisp-test.el --- Tests for anvil-ide-elisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 38 Phase C — tests for the IDE-only `elisp-info-lookup-symbol'
;; tool that was split out of anvil-elisp.el.  The fboundp-guard +
;; symbol-validation paths are exercised via `cl-letf' so the tests do
;; not require an Info reader to be configured in this Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-ide-elisp)

;;;; --- elisp-info-lookup-symbol: fboundp guard ----------------------

(ert-deftest anvil-ide-elisp-test-info-lookup-unavailable-returns-error-json ()
  "When `info-lookup-symbol' is not bound the tool returns a JSON
object with `error: \"info-look-unavailable\"' instead of throwing."
  (let ((real-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (and (not (eq sym 'info-lookup-symbol))
                      (funcall real-fboundp sym)))))
      (let* ((out (anvil-ide-elisp--info-lookup-symbol "car"))
             (parsed (json-read-from-string out)))
        (should (equal :json-false (alist-get 'found parsed)))
        (should (equal "info-look-unavailable"
                       (alist-get 'error parsed)))))))

(ert-deftest anvil-ide-elisp-test-info-lookup-validates-symbol-first ()
  "Empty / non-string symbol arg must throw before reaching the
fboundp guard."
  (should-error (anvil-ide-elisp--info-lookup-symbol "")
                :type 'anvil-server-tool-error)
  (should-error (anvil-ide-elisp--info-lookup-symbol 42)
                :type 'anvil-server-tool-error))

;;; anvil-ide-elisp-test.el ends here

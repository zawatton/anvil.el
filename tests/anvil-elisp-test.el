;;; anvil-elisp-test.el --- NeLisp-compat tests for anvil-elisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase B2 (Doc 38) — verify that anvil-elisp's 7 MCP tools work in
;; both Emacs and NeLisp runtimes.  Six tools must be portable; the
;; seventh (`elisp-info-lookup-symbol') must degrade gracefully when
;; `info-lookup-symbol' is unavailable.
;;
;; The split-out fallback / portable-renderer / fboundp-guard paths
;; are exercised via `cl-letf' so the tests do not require an actual
;; NeLisp runtime.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-elisp)

;;;; --- elisp-describe-function: portable renderer -------------------

(ert-deftest anvil-elisp-test-describe-function-portable-uses-documentation ()
  "`anvil-elisp--describe-function-portable' returns a string that
contains the docstring and a function signature, without calling
`describe-function-1'."
  (let ((called-describe-1 nil))
    (cl-letf (((symbol-function 'describe-function-1)
               (lambda (&rest _) (setq called-describe-1 t))))
      (let ((out (anvil-elisp--describe-function "car")))
        (should (stringp out))
        (should (string-match-p "(car" out))
        (should-not called-describe-1)))))

(ert-deftest anvil-elisp-test-describe-function-void-throws ()
  "Void functions still raise `anvil-server-tool-error'."
  (should-error
   (anvil-elisp--describe-function
    "anvil-elisp-test--definitely-not-a-function")
   :type 'anvil-server-tool-error))

;;;; --- elisp-info-lookup-symbol: fboundp guard ----------------------

(ert-deftest anvil-elisp-test-info-lookup-unavailable-returns-error-json ()
  "When `info-lookup-symbol' is not bound the tool returns a JSON
object with `error: \"info-look-unavailable\"' instead of throwing."
  (let ((real-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (and (not (eq sym 'info-lookup-symbol))
                      (funcall real-fboundp sym)))))
      (let* ((out (anvil-elisp--info-lookup-symbol "car"))
             (parsed (json-read-from-string out)))
        (should (equal :json-false (alist-get 'found parsed)))
        (should (equal "info-look-unavailable"
                       (alist-get 'error parsed)))))))

(ert-deftest anvil-elisp-test-info-lookup-validates-symbol-first ()
  "Empty / non-string symbol arg must throw before reaching the
fboundp guard."
  (should-error (anvil-elisp--info-lookup-symbol "")
                :type 'anvil-server-tool-error)
  (should-error (anvil-elisp--info-lookup-symbol 42)
                :type 'anvil-server-tool-error))

;;;; --- elisp-byte-compile-file: nelisp-cc delegate ------------------

(defun anvil-elisp-test--write (path content)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'silent)))

(ert-deftest anvil-elisp-test-byte-compile-fallback-when-no-nelisp-cc ()
  "Without `nelisp-cc-runtime-compile-and-allocate' the original
`byte-compile-file' renderer runs and the `:backend' key is absent.
Skipped if NeLisp happens to be loaded into this Emacs."
  (skip-unless (not (fboundp 'nelisp-cc-runtime-compile-and-allocate)))
  (let ((tmp (make-temp-file "anvil-elisp-bc-" nil ".el")))
    (unwind-protect
        (progn
          (anvil-elisp-test--write
           tmp
           ";;; tmp --- x -*- lexical-binding: t; -*-
;;; Commentary: tmp
;;; Code:
(defun anvil-elisp-test-tmp-fn () \"clean.\" 1)
(provide 'tmp)
;;; tmp ends here
")
          (let* ((out (anvil-elisp--byte-compile-file tmp))
                 (res (car (read-from-string out))))
            (should (eq t (plist-get res :ok)))
            (should (null (plist-get res :backend)))))
      (ignore-errors (delete-file tmp))
      (ignore-errors
        (delete-file (concat (file-name-sans-extension tmp) ".elc"))))))

(ert-deftest anvil-elisp-test-byte-compile-uses-nelisp-cc-when-bound ()
  "When `nelisp-cc-runtime-compile-and-allocate' is bound the tool
delegates each top-level defun to it and tags `:backend nelisp-cc'."
  (let ((calls 0))
    (cl-letf (((symbol-function 'nelisp-cc-runtime-compile-and-allocate)
               (lambda (form &optional _backend)
                 (cl-incf calls)
                 ;; Pretend success.
                 (list :exec-page :stub :backend 'x86_64))))
      (let ((tmp (make-temp-file "anvil-elisp-bc-nelisp-" nil ".el")))
        (unwind-protect
            (progn
              (anvil-elisp-test--write
               tmp
               "(defun anvil-elisp-test-nfn-a () 1)
(defun anvil-elisp-test-nfn-b (x) (+ x 1))
(defvar anvil-elisp-test-nv 42)
")
              (let* ((out (anvil-elisp--byte-compile-file tmp))
                     (res (car (read-from-string out))))
                (should (eq t (plist-get res :ok)))
                (should (eq 'nelisp-cc (plist-get res :backend)))
                (should (= 2 (plist-get res :compiled-forms)))
                (should (= 2 calls))))
          (ignore-errors (delete-file tmp)))))))

(ert-deftest anvil-elisp-test-byte-compile-nelisp-captures-form-errors ()
  "A nelisp-cc error on one form is captured into :warnings, not
raised."
  (cl-letf (((symbol-function 'nelisp-cc-runtime-compile-and-allocate)
             (lambda (_form &optional _backend)
               (error "synthetic compile error"))))
    (let ((tmp (make-temp-file "anvil-elisp-bc-nelisp-err-" nil ".el")))
      (unwind-protect
          (progn
            (anvil-elisp-test--write
             tmp "(defun anvil-elisp-test-bad-fn () 1)\n")
            (let* ((out (anvil-elisp--byte-compile-file tmp))
                   (res (car (read-from-string out)))
                   (warns (plist-get res :warnings)))
              (should (eq t (plist-get res :ok)))
              (should (= 1 (length warns)))
              (should (string-match-p "synthetic compile error"
                                      (car warns)))))
        (ignore-errors (delete-file tmp))))))

;;; anvil-elisp-test.el ends here

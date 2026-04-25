;;; anvil-extend-test.el --- Tests for anvil-extend (Doc 35 Phase A) -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT coverage for the Phase A scaffold generator.  Every test
;; rebinds `anvil-extend-storage-dir' to a fresh temp directory so
;; the suite is hermetic and parallel-safe.  No MCP server is
;; required: the function-level API is exercised directly and the
;; MCP wrapper is tested with a local stub of
;; `anvil-server-with-error-handling'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-extend)

;;;; --- fixtures -----------------------------------------------------------

(defmacro anvil-extend-test--with-storage (var &rest body)
  "Bind VAR to a fresh storage dir and `let' `anvil-extend-storage-dir' to it.

The directory and its contents are removed at the end of the
test, even on failure.  VAR is bound for tests that need to
inspect the directory.  When the body does not reference VAR a
trailing `(ignore VAR)' is appended to silence the unused-lexical
warning without forcing callers to underscore-prefix the symbol."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory
                 (make-temp-file "anvil-extend-test-" t)))
          (anvil-extend-storage-dir ,var))
     (unwind-protect
         (progn (ignore ,var) ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun anvil-extend-test--cleanup-feature (sym)
  "Best-effort `unload-feature' for SYM and its `*-test' sibling."
  (dolist (s (list sym (intern (concat (symbol-name sym) "-test"))))
    (when (featurep s)
      (ignore-errors (unload-feature s t)))))

;;;; --- name validation ----------------------------------------------------

(ert-deftest anvil-extend-test-rejects-non-symbol ()
  "NAME must be a symbol; strings, numbers, lists are rejected."
  (anvil-extend-test--with-storage dir
    (should-error (anvil-extend-scaffold "string-name") :type 'user-error)
    (should-error (anvil-extend-scaffold 42)            :type 'user-error)
    (should-error (anvil-extend-scaffold '(a b))        :type 'user-error)))

(ert-deftest anvil-extend-test-rejects-bad-symbol-shape ()
  "NAME symbol must be lowercase + hyphenated (no underscores, no caps)."
  (anvil-extend-test--with-storage dir
    (should-error (anvil-extend-scaffold 'BadName)    :type 'user-error)
    (should-error (anvil-extend-scaffold 'bad_name)   :type 'user-error)
    (should-error (anvil-extend-scaffold '-leading-)  :type 'user-error)
    (should-error (anvil-extend-scaffold '1starts)    :type 'user-error)))

;;;; --- scaffold output ----------------------------------------------------

(ert-deftest anvil-extend-test-scaffold-creates-three-files ()
  "A successful scaffold call writes elisp + test + register stubs."
  (anvil-extend-test--with-storage dir
    (let ((res (anvil-extend-scaffold 'sample-tool
                                      :params '(x y)
                                      :body '((+ x y))
                                      :docstring "sum")))
      (should (eq :created (plist-get res :status)))
      (should (file-exists-p (plist-get res :elisp-file)))
      (should (file-exists-p (plist-get res :test-file)))
      (should (file-exists-p (plist-get res :register-stub))))))

(ert-deftest anvil-extend-test-scaffold-then-load-callable ()
  "After scaffold + load, the new function is `fboundp' and runs."
  (anvil-extend-test--with-storage dir
    (unwind-protect
        (progn
          (anvil-extend-scaffold 'sample-mul
                                 :params '(x y)
                                 :body '((* x y))
                                 :docstring "multiply")
          (let ((load-res (anvil-extend-load 'sample-mul)))
            (should (eq :loaded (plist-get load-res :status)))
            (should (fboundp 'sample-mul))
            (should (= 21 (funcall (symbol-function 'sample-mul) 3 7)))))
      (anvil-extend-test--cleanup-feature 'sample-mul))))

(ert-deftest anvil-extend-test-scaffold-without-body-still-loads ()
  "Empty body falls back to placeholder so the file still compiles."
  (anvil-extend-test--with-storage dir
    (unwind-protect
        (progn
          (anvil-extend-scaffold 'sample-noop)
          (let ((res (anvil-extend-load 'sample-noop)))
            (should (eq :loaded (plist-get res :status)))
            (should (fboundp 'sample-noop))))
      (anvil-extend-test--cleanup-feature 'sample-noop))))

(ert-deftest anvil-extend-test-scaffold-respects-custom-storage ()
  "Generated paths land under the rebound storage directory."
  (anvil-extend-test--with-storage dir
    (let ((res (anvil-extend-scaffold 'sample-here
                                      :body '(t))))
      (should (string-prefix-p dir (plist-get res :elisp-file)))
      (should (string-prefix-p dir (plist-get res :test-file)))
      (should (string-prefix-p dir (plist-get res :register-stub))))))

(ert-deftest anvil-extend-test-scaffold-overwrites-existing ()
  "Re-scaffolding the same NAME replaces previous files in place."
  (anvil-extend-test--with-storage dir
    (anvil-extend-scaffold 'sample-overwrite :body '(1))
    (let ((first (with-temp-buffer
                   (insert-file-contents
                    (plist-get (anvil-extend--paths 'sample-overwrite)
                               :elisp-file))
                   (buffer-string))))
      (anvil-extend-scaffold 'sample-overwrite :body '(2))
      (let ((second (with-temp-buffer
                      (insert-file-contents
                       (plist-get (anvil-extend--paths 'sample-overwrite)
                                  :elisp-file))
                      (buffer-string))))
        (should-not (string= first second))))))

(ert-deftest anvil-extend-test-scaffold-creates-storage-dir ()
  "Storage directory is created on demand if absent."
  (let* ((dir (file-name-as-directory
               (make-temp-name
                (expand-file-name "anvil-extend-mkdir-" temporary-file-directory))))
         (anvil-extend-storage-dir dir))
    (unwind-protect
        (progn
          (should-not (file-directory-p dir))
          (anvil-extend-scaffold 'sample-mkdir :body '(t))
          (should (file-directory-p dir)))
      (when (file-directory-p dir) (delete-directory dir t)))))

;;;; --- load failure paths -------------------------------------------------

(ert-deftest anvil-extend-test-load-missing-returns-failed ()
  "Loading without prior scaffold returns plist :status :failed."
  (anvil-extend-test--with-storage dir
    (let ((res (anvil-extend-load 'never-scaffolded)))
      (should (eq :failed (plist-get res :status)))
      (should (stringp (plist-get res :reason)))
      (should (string-match-p "no scaffold file" (plist-get res :reason))))))

;;;; --- list ---------------------------------------------------------------

(ert-deftest anvil-extend-test-list-empty-when-no-storage ()
  "An empty / absent storage dir lists nothing."
  (let* ((dir (file-name-as-directory
               (make-temp-name
                (expand-file-name "anvil-extend-empty-" temporary-file-directory))))
         (anvil-extend-storage-dir dir))
    (unwind-protect
        (should (null (anvil-extend-list)))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest anvil-extend-test-list-after-scaffold ()
  "Scaffolded extensions appear in the list with structural metadata."
  (anvil-extend-test--with-storage dir
    (anvil-extend-scaffold 'list-a :body '(t))
    (anvil-extend-scaffold 'list-b :body '(t))
    (let* ((rows (anvil-extend-list))
           (names (mapcar (lambda (r) (plist-get r :name)) rows)))
      (should (= 2 (length rows)))
      (should (memq 'list-a names))
      (should (memq 'list-b names))
      (dolist (r rows)
        (should (eq t (plist-get r :tested)))
        (should (memq (plist-get r :pool) '(ephemeral permanent)))))))

(ert-deftest anvil-extend-test-list-skips-test-and-register-files ()
  "Only the bare `<NAME>.el' files become entries (no -test/-register)."
  (anvil-extend-test--with-storage dir
    (anvil-extend-scaffold 'unique-stem :body '(t))
    (let* ((rows (anvil-extend-list))
           (names (mapcar (lambda (r) (plist-get r :name)) rows)))
      (should (equal '(unique-stem) names)))))

;;;; --- remove -------------------------------------------------------------

(ert-deftest anvil-extend-test-remove-cleans-up ()
  "Remove deletes elisp + test + register and reports a count."
  (anvil-extend-test--with-storage dir
    (anvil-extend-scaffold 'gone :body '(t))
    (let ((res (anvil-extend-remove 'gone)))
      (should (eq :removed (plist-get res :status)))
      (should (>= (plist-get res :deleted) 3)))
    (let ((paths (anvil-extend--paths 'gone)))
      (should-not (file-exists-p (plist-get paths :elisp-file)))
      (should-not (file-exists-p (plist-get paths :test-file)))
      (should-not (file-exists-p (plist-get paths :register-stub))))))

(ert-deftest anvil-extend-test-remove-tolerates-absent ()
  "Removing an extension that was never scaffolded is a no-op succeed."
  (anvil-extend-test--with-storage dir
    (let ((res (anvil-extend-remove 'never-was)))
      (should (eq :removed (plist-get res :status)))
      (should (= 0 (plist-get res :deleted))))))

;;;; --- test runner -------------------------------------------------------

(ert-deftest anvil-extend-test-test-missing-reports-missing ()
  "`anvil-extend-test' without scaffold returns :status :missing."
  (anvil-extend-test--with-storage dir
    (let ((res (anvil-extend-test 'never-scaffolded)))
      (should (eq :missing (plist-get res :status)))
      (should (stringp (plist-get res :reason))))))

(ert-deftest anvil-extend-test-test-runs-smoke ()
  "Running ERT against a scaffolded extension yields a passed status."
  (anvil-extend-test--with-storage dir
    (unwind-protect
        (progn
          (anvil-extend-scaffold 'sample-runme
                                 :params '(x)
                                 :body '(x)
                                 :docstring "echo")
          (let ((res (anvil-extend-test 'sample-runme)))
            (should (memq (plist-get res :status) '(:passed :failed)))
            ;; The smoke template asserts `fboundp', so it must pass:
            (should (eq :passed (plist-get res :status)))
            (should (>= (plist-get res :pass-count) 1))
            (should (= 0 (plist-get res :fail-count)))))
      (anvil-extend-test--cleanup-feature 'sample-runme))))

;;;; --- MCP wrapper smoke --------------------------------------------------

(defmacro anvil-extend-test--with-server-stub (&rest body)
  "Provide a minimal `anvil-server-with-error-handling' stub for tests.

The real macro lives in `anvil-server', which is not required by
`anvil-extend' at load time; we shim it here so the MCP wrapper
function can run under ERT without pulling in the whole server."
  `(cl-letf* (((symbol-function 'anvil-server-with-error-handling-fn)
               (lambda (thunk) (funcall thunk))))
     (let ((expanded
            (lambda (&rest args)
              (apply #'anvil-extend--tool-scaffold args))))
       (funcall expanded ,@body))))

(ert-deftest anvil-extend-test-mcp-wrapper-accepts-string-name ()
  "The MCP tool accepts NAME as a string and emits the scaffold."
  (anvil-extend-test--with-storage dir
    ;; Provide a stub macro if anvil-server is absent so the wrapper
    ;; can be called; if anvil-server IS loaded, use the real macro.
    (let* ((have-server (featurep 'anvil-server))
           (out (if have-server
                    (anvil-extend--tool-scaffold "mcp-string-name")
                  (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                             (lambda (&rest body)
                               `(progn ,@body))))
                    (eval `(anvil-extend--tool-scaffold "mcp-string-name"))))))
      (should (stringp out))
      (should (string-match-p ":status :created" out))
      (let ((paths (anvil-extend--paths 'mcp-string-name)))
        (should (file-exists-p (plist-get paths :elisp-file)))))))

(ert-deftest anvil-extend-test-register-stub-references-symbol ()
  "Generated register stub references the new symbol and tool-id."
  (anvil-extend-test--with-storage dir
    (anvil-extend-scaffold 'reg-target :body '(t))
    (let* ((paths (anvil-extend--paths 'reg-target))
           (txt (with-temp-buffer
                  (insert-file-contents (plist-get paths :register-stub))
                  (buffer-string))))
      (should (string-match-p "reg-target" txt))
      (should (string-match-p "anvil-server-register-tool" txt))
      (should (string-match-p ":id \"reg-target\"" txt)))))

(ert-deftest anvil-extend-test-elisp-file-has-provide ()
  "Generated <NAME>.el ends with a matching `provide' form."
  (anvil-extend-test--with-storage dir
    (anvil-extend-scaffold 'provide-check :body '(t))
    (let* ((paths (anvil-extend--paths 'provide-check))
           (txt (with-temp-buffer
                  (insert-file-contents (plist-get paths :elisp-file))
                  (buffer-string))))
      (should (string-match-p "(provide 'provide-check)" txt))
      (should (string-match-p "provide-check.el ends here" txt)))))

(provide 'anvil-extend-test)
;;; anvil-extend-test.el ends here

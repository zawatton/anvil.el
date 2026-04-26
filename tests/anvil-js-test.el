;;; anvil-js-test.el --- Tests for anvil-js  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 21 Phase 1b — read-only JS / JSX locators via
;; tree-sitter-javascript.  anvil-js is a thin dispatcher over the
;; shared `anvil-ts--*-lang' helpers, so these tests focus on
;; language-routing correctness (every extension → `javascript')
;; and on the JS fixture's expected shapes; the deep capture
;; semantics are covered by anvil-ts-test.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Doc 38 Phase E — anvil-ide-treesit lives in zawatton/anvil-ide.el now.
;; Soft-require keeps loading clean when the IDE layer is absent;
;; grammar-dependent tests already auto-skip on missing grammars.
(require 'anvil-ide-treesit nil 'noerror)
(require 'anvil-ts)
(require 'anvil-js)

(defconst anvil-js-test--js-fixture
  (expand-file-name "fixtures/js-sample/app.js"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst anvil-js-test--jsx-fixture
  (expand-file-name "fixtures/jsx-sample/app.jsx"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun anvil-js-test--ide-ready ()
  "Return non-nil when anvil-ide-treesit is loaded (= Doc 38 Phase E)."
  (fboundp 'anvil-treesit-language-for-file))

(defun anvil-js-test--grammar-ready ()
  "Return non-nil when the JavaScript grammar is loadable."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'javascript)))

(defmacro anvil-js-test--requires-grammar (&rest body)
  "Skip the test body unless tree-sitter-javascript is available
AND the anvil-ide-treesit layer is loaded (= Doc 38 Phase E gate)."
  (declare (indent 0))
  `(cond
    ((not (anvil-js-test--ide-ready))
     (ert-skip "anvil-ide-treesit not loaded (= ide layer absent)"))
    ((anvil-js-test--grammar-ready) (progn ,@body))
    (t (ert-skip "tree-sitter-javascript grammar not installed"))))

(defmacro anvil-js-test--requires-ide (&rest body)
  "Skip BODY when anvil-ide-treesit isn't loaded.  For tests that
exercise pure dispatch helpers but still depend on the IDE layer."
  (declare (indent 0))
  `(if (anvil-js-test--ide-ready)
       (progn ,@body)
     (ert-skip "anvil-ide-treesit not loaded (= ide layer absent)")))

;;;; --- pure / grammar-independent -----------------------------------------

(ert-deftest anvil-js-test-lang-for-file-accepts-all-js-extensions ()
 (anvil-js-test--requires-ide
  (dolist (ext '("js" "jsx" "mjs" "cjs"))
    (should (eq 'javascript (anvil-js--lang-for-file (concat "/x/app." ext)))))))

(ert-deftest anvil-js-test-lang-for-file-rejects-ts ()
  "anvil-js refuses .ts / .tsx so callers can't bypass anvil-ts."
 (anvil-js-test--requires-ide
  (should-error (anvil-js--lang-for-file "/x/app.ts") :type 'user-error)
  (should-error (anvil-js--lang-for-file "/x/app.tsx") :type 'user-error)
  (should-error (anvil-js--lang-for-file "/x/foo.py") :type 'user-error)))

;;;; --- JS locators vs fixture ---------------------------------------------

(ert-deftest anvil-js-test-list-imports-count ()
  (anvil-js-test--requires-grammar
    (let ((ims (anvil-js-list-imports anvil-js-test--js-fixture)))
      ;; 4 `import' statements: readFile, path, express, side-effects.
      (should (= 4 (length ims)))
      (should (cl-every (lambda (p) (eq 'import (plist-get p :kind))) ims)))))

(ert-deftest anvil-js-test-list-functions-covers-fn-and-arrow ()
  (anvil-js-test--requires-grammar
    (let* ((fns (anvil-js-list-functions anvil-js-test--js-fixture))
           (names (mapcar (lambda (p) (plist-get p :name)) fns)))
      (should (member "plainFunc" names))
      (should (member "fetchValue" names))
      (should (member "arrowHelper" names))
      (should (member "privateArrow" names))
      (should (member "outer" names))
      (should (member "inner" names))
      (let ((fv (cl-find "fetchValue" fns :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=)))
        (should (eq 'function (plist-get fv :kind)))
        (should (plist-get fv :async)))
      (let ((ah (cl-find "arrowHelper" fns :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=)))
        (should (eq 'arrow (plist-get ah :kind)))))))

(ert-deftest anvil-js-test-list-classes ()
  (anvil-js-test--requires-grammar
    (let* ((cls (anvil-js-list-classes anvil-js-test--js-fixture))
           (names (mapcar (lambda (p) (plist-get p :name)) cls)))
      (should (member "ReportWriter" names))
      (should (member "Config" names)))))

(ert-deftest anvil-js-test-list-methods-reportwriter ()
  (anvil-js-test--requires-grammar
    (let* ((ms (anvil-js-list-methods anvil-js-test--js-fixture "ReportWriter"))
           (names (mapcar (lambda (p) (plist-get p :name)) ms)))
      (should (member "constructor" names))
      (should (member "write" names))
      (should (member "defaultName" names))
      (should (member "writeAsync" names))
      (let ((dn (cl-find "defaultName" ms :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=)))
        (should (plist-get dn :static))))))

(ert-deftest anvil-js-test-list-exports-includes-reexport ()
  (anvil-js-test--requires-grammar
    (let* ((exs (anvil-js-list-exports anvil-js-test--js-fixture))
           (kinds (mapcar (lambda (e) (plist-get e :kind)) exs)))
      (should (memq 'const kinds))
      (should (memq 'function kinds))
      (should (memq 'class kinds))
      (should (memq 'reexport kinds)))))

(ert-deftest anvil-js-test-find-definition-class ()
  (anvil-js-test--requires-grammar
    (let ((d (anvil-js-find-definition anvil-js-test--js-fixture "Config")))
      (should d)
      (should (eq 'class (plist-get d :kind))))))

(ert-deftest anvil-js-test-find-definition-does-not-consult-ts-kinds ()
  "anvil-js-find-definition must not hallucinate interface / type kinds
even when handed a fixture that happens to parse clean under the
javascript grammar.  Our JS fixture has no interfaces, so any
lookup named after a TS-only kind should return nil."
  (anvil-js-test--requires-grammar
    (should (null (anvil-js-find-definition anvil-js-test--js-fixture
                                            "ButtonProps")))))

(ert-deftest anvil-js-test-surrounding-form-inside-class ()
  (anvil-js-test--requires-grammar
    (let* ((cls (anvil-js-find-definition anvil-js-test--js-fixture
                                          "ReportWriter"))
           (b (plist-get cls :bounds))
           (mid (+ (plist-get b :start) 8))
           (encl (anvil-js-surrounding-form anvil-js-test--js-fixture mid)))
      (should encl)
      (should (string= "ReportWriter" (plist-get encl :name))))))

;;;; --- JSX dispatch -------------------------------------------------------

(ert-deftest anvil-js-test-jsx-dispatch ()
  "JSX fixtures route through the javascript grammar; React components
are reported as functions / arrows and the extending class is
preserved."
  (anvil-js-test--requires-grammar
    (let* ((fns (anvil-js-list-functions anvil-js-test--jsx-fixture))
           (cls (anvil-js-list-classes anvil-js-test--jsx-fixture))
           (fn-names (mapcar (lambda (p) (plist-get p :name)) fns))
           (cls-names (mapcar (lambda (p) (plist-get p :name)) cls)))
      (should (member "Button" fn-names))
      (should (member "Greeting" fn-names))
      (should (member "ThemedPanel" cls-names)))))

;;;; --- error paths --------------------------------------------------------

(ert-deftest anvil-js-test-list-imports-errors-on-missing-file ()
  (anvil-js-test--requires-grammar
    (should-error (anvil-js-list-imports "/tmp/definitely-not-there.js")
                  :type 'user-error)))

(ert-deftest anvil-js-test-list-imports-errors-on-nil-file ()
 (anvil-js-test--requires-ide
  (should-error (anvil-js-list-imports nil) :type 'user-error)))

;;;; --- Phase 2: edit helpers ---------------------------------------------

(defun anvil-js-test--temp-copy (src ext)
  "Copy SRC to a fresh tempfile with extension EXT and return its path."
  (let ((dst (make-temp-file "anvil-js-phase2-" nil (concat "." ext))))
    (with-temp-file dst
      (insert-file-contents src))
    dst))

(defun anvil-js-test--contents (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest anvil-js-test-add-import-merge ()
  (anvil-js-test--requires-grammar
    (let ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js")))
      (unwind-protect
          (progn
            (anvil-js-add-import
             f (list :from "node:fs/promises"
                     :named (list "writeFile"))
             :apply t)
            (should (string-match-p
                     "import { readFile, writeFile } from \"node:fs/promises\";"
                     (anvil-js-test--contents f))))
        (delete-file f)))))

(ert-deftest anvil-js-test-add-import-idempotent ()
  (anvil-js-test--requires-grammar
    (let ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js")))
      (unwind-protect
          (let ((plan (anvil-js-add-import
                       f (list :from "node:fs/promises"
                               :named (list "readFile")))))
            (should (null (plist-get plan :ops))))
        (delete-file f)))))

(ert-deftest anvil-js-test-add-import-ignores-type-only ()
  "JS has no type-only imports; the flag is silently dropped so a
fresh `import type { ... }' is never emitted into a .js file."
  (anvil-js-test--requires-grammar
    (let ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js")))
      (unwind-protect
          (progn
            (anvil-js-add-import
             f (list :from "brand-new" :type-only t
                     :named (list "x"))
             :apply t)
            (should (string-match-p
                     "import { x } from \"brand-new\";"
                     (anvil-js-test--contents f)))
            (should-not (string-match-p
                         "import type" (anvil-js-test--contents f))))
        (delete-file f)))))

(ert-deftest anvil-js-test-remove-import-drops-statement ()
  (anvil-js-test--requires-grammar
    (let ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js")))
      (unwind-protect
          (progn
            (anvil-js-remove-import
             f (list :from "express" :default "express")
             :apply t)
            (should-not (string-match-p
                         "from \"express\""
                         (anvil-js-test--contents f))))
        (delete-file f)))))

(ert-deftest anvil-js-test-rename-import ()
  (anvil-js-test--requires-grammar
    (let ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js")))
      (unwind-protect
          (progn
            (anvil-js-rename-import f "express" "ex" :apply t)
            (should (string-match-p
                     "import ex from \"express\";"
                     (anvil-js-test--contents f))))
        (delete-file f)))))

(ert-deftest anvil-js-test-replace-function-preserves-export ()
  (anvil-js-test--requires-grammar
    (let ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js")))
      (unwind-protect
          (progn
            (anvil-js-replace-function
             f "plainFunc"
             "function plainFunc(x) {\n  return x + 42;\n}"
             :apply t)
            (let ((body (anvil-js-test--contents f)))
              (should (string-match-p
                       "export function plainFunc(x) {" body))
              (should (string-match-p "  return x \\+ 42;" body))))
        (delete-file f)))))

(ert-deftest anvil-js-test-replace-function-method ()
  (anvil-js-test--requires-grammar
    (let ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js")))
      (unwind-protect
          (progn
            (anvil-js-replace-function
             f "write"
             "write(rows) {\n  console.log(rows.length);\n}"
             :class "ReportWriter" :apply t)
            (should (string-match-p
                     "  write(rows) {"
                     (anvil-js-test--contents f))))
        (delete-file f)))))

(ert-deftest anvil-js-test-wrap-expr ()
  (anvil-js-test--requires-grammar
    (let ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js")))
      (unwind-protect
          (let (beg end)
            (with-temp-buffer
              (insert-file-contents f)
              (goto-char (point-min))
              (re-search-forward "(x) => \\(x \\* 2\\);")
              (setq beg (match-beginning 1) end (match-end 1)))
            (anvil-js-wrap-expr f beg end "cached(|anvil-hole|)"
                                :apply t)
            (should (string-match-p "cached(x \\* 2)"
                                    (anvil-js-test--contents f))))
        (delete-file f)))))

(ert-deftest anvil-js-test-preview-default-does-not-touch-disk ()
  (anvil-js-test--requires-grammar
    (let* ((f (anvil-js-test--temp-copy anvil-js-test--js-fixture "js"))
           (before (anvil-js-test--contents f)))
      (unwind-protect
          (progn
            (anvil-js-add-import
             f (list :from "brand-new" :named (list "foo")))
            (should (string= before (anvil-js-test--contents f))))
        (delete-file f)))))

(provide 'anvil-js-test)
;;; anvil-js-test.el ends here

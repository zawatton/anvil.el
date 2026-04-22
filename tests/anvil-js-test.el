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
(require 'anvil-treesit)
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

(defun anvil-js-test--grammar-ready ()
  "Return non-nil when the JavaScript grammar is loadable."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'javascript)))

(defmacro anvil-js-test--requires-grammar (&rest body)
  "Skip the test body unless tree-sitter-javascript is available."
  (declare (indent 0))
  `(if (anvil-js-test--grammar-ready)
       (progn ,@body)
     (ert-skip "tree-sitter-javascript grammar not installed")))

;;;; --- pure / grammar-independent -----------------------------------------

(ert-deftest anvil-js-test-lang-for-file-accepts-all-js-extensions ()
  (dolist (ext '("js" "jsx" "mjs" "cjs"))
    (should (eq 'javascript (anvil-js--lang-for-file (concat "/x/app." ext))))))

(ert-deftest anvil-js-test-lang-for-file-rejects-ts ()
  "anvil-js refuses .ts / .tsx so callers can't bypass anvil-ts."
  (should-error (anvil-js--lang-for-file "/x/app.ts") :type 'user-error)
  (should-error (anvil-js--lang-for-file "/x/app.tsx") :type 'user-error)
  (should-error (anvil-js--lang-for-file "/x/foo.py") :type 'user-error))

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
  (should-error (anvil-js-list-imports nil) :type 'user-error))

(provide 'anvil-js-test)
;;; anvil-js-test.el ends here

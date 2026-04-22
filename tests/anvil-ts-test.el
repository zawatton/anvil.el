;;; anvil-ts-test.el --- Tests for anvil-ts  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 21 Phase 1b — read-only TS / TSX locators via
;; tree-sitter-typescript.  Fixture-backed assertions run only when
;; the matching grammar is installed; unit-shaped tests
;; (extension dispatch, grammar-missing error shape) run
;; unconditionally so the suite stays green on grammar-less
;; runners.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-treesit)
(require 'anvil-ts)

(defconst anvil-ts-test--ts-fixture
  (expand-file-name "fixtures/ts-sample/app.ts"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst anvil-ts-test--tsx-fixture
  (expand-file-name "fixtures/tsx-sample/app.tsx"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun anvil-ts-test--grammar-ready (lang)
  "Return non-nil when the LANG grammar is loadable."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p lang)))

(defmacro anvil-ts-test--requires (lang &rest body)
  "Skip the test body unless the tree-sitter LANG grammar is available."
  (declare (indent 1))
  `(if (anvil-ts-test--grammar-ready ,lang)
       (progn ,@body)
     (ert-skip (format "tree-sitter-%s grammar not installed" ,lang))))

;;;; --- pure / grammar-independent -----------------------------------------

(ert-deftest anvil-ts-test-dispatch-ts-extension ()
  (should (eq 'typescript (anvil-treesit-language-for-file "/x/app.ts")))
  (should (eq 'tsx        (anvil-treesit-language-for-file "/x/app.tsx")))
  (should (eq 'javascript (anvil-treesit-language-for-file "/x/app.js")))
  (should (eq 'javascript (anvil-treesit-language-for-file "/x/app.jsx")))
  (should (eq 'javascript (anvil-treesit-language-for-file "/x/app.mjs")))
  (should (eq 'javascript (anvil-treesit-language-for-file "/x/app.cjs")))
  (should (null (anvil-treesit-language-for-file "/x/unknown.xyz"))))

(ert-deftest anvil-ts-test-lang-for-file-rejects-non-ts ()
  "The TS dispatcher errors on .js / .jsx / unknown rather than silently
defaulting; users who meant JS should have called anvil-js-*."
  (should-error (anvil-ts--lang-for-file "/x/app.js") :type 'user-error)
  (should-error (anvil-ts--lang-for-file "/x/app.jsx") :type 'user-error)
  (should-error (anvil-ts--lang-for-file "/x/foo.py") :type 'user-error)
  (should (eq 'typescript (anvil-ts--lang-for-file "/x/app.ts")))
  (should (eq 'tsx (anvil-ts--lang-for-file "/x/app.tsx"))))

(ert-deftest anvil-ts-test-grammar-missing-error-shape ()
  "The structured error carries the install hint and source URL."
  (dolist (lang '(typescript tsx javascript))
    (let ((e (anvil-treesit-grammar-missing-error lang)))
      (should (eq 'grammar-missing (plist-get e :kind)))
      (should (eq lang (plist-get e :lang)))
      (should (string-match-p "treesit-install-language-grammar"
                              (plist-get e :install-hint)))
      (should (string-match-p "tree-sitter" (plist-get e :source-url))))))

(ert-deftest anvil-ts-test-grammar-missing-signals-structured-user-error ()
  "With the grammar stubbed as unavailable, callers see a user-error
whose message is a `read'-able grammar-missing plist."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang) nil)))
    (condition-case err
        (progn (anvil-treesit-ensure-grammar 'typescript)
               (should nil))
      (user-error
       (let* ((msg (error-message-string err))
              (plist (read msg)))
         (should (eq 'grammar-missing (plist-get plist :kind)))
         (should (eq 'typescript (plist-get plist :lang))))))))

;;;; --- TS locators vs fixture ---------------------------------------------

(ert-deftest anvil-ts-test-list-imports-count ()
  (anvil-ts-test--requires 'typescript
    (let ((ims (anvil-ts-list-imports anvil-ts-test--ts-fixture)))
      ;; Fixture has 5 import_statement nodes: readFile, path,
      ;; Buffer type import, express+named, and the bare
      ;; side-effect "./side-effects".
      (should (= 5 (length ims)))
      (should (cl-every (lambda (p) (eq 'import (plist-get p :kind))) ims))
      ;; The bare side-effect import has no `from' keyword.
      (should (cl-some (lambda (p)
                         (string-match-p "side-effects"
                                         (plist-get p :text)))
                       ims)))))

(ert-deftest anvil-ts-test-list-exports-kinds ()
  (anvil-ts-test--requires 'typescript
    (let* ((exs (anvil-ts-list-exports anvil-ts-test--ts-fixture))
           (kinds (mapcar (lambda (e) (plist-get e :kind)) exs))
           (names (cl-remove nil (mapcar (lambda (e) (plist-get e :name))
                                         exs)
                             :test #'equal)))
      (should (memq 'const kinds))        ; `export const MAX_ROWS'
      (should (memq 'interface kinds))    ; `export interface ReportInput'
      (should (memq 'type kinds))         ; `export type ReportMode'
      (should (memq 'function kinds))     ; `export function plainFunc'
      (should (memq 'class kinds))        ; `export class ReportWriter'
      (should (memq 'reexport kinds))     ; `export { plainFunc as ... }'
      (should (member "MAX_ROWS" names))
      (should (member "plainFunc" names))
      (should (member "ReportWriter" names))
      (should (member "ReportInput" names)))))

(ert-deftest anvil-ts-test-list-functions-covers-fn-and-arrow ()
  (anvil-ts-test--requires 'typescript
    (let* ((fns (anvil-ts-list-functions anvil-ts-test--ts-fixture))
           (names (mapcar (lambda (p) (plist-get p :name)) fns))
           (by-name (lambda (n) (cl-find n fns
                                         :key (lambda (p) (plist-get p :name))
                                         :test #'string=))))
      (should (member "plainFunc" names))
      (should (member "fetchValue" names))
      (should (member "arrowHelper" names))
      (should (member "privateArrow" names))
      (should (member "outer" names))
      (should (member "inner" names))  ; nested
      (should (eq 'function (plist-get (funcall by-name "plainFunc") :kind)))
      (should (eq 'arrow (plist-get (funcall by-name "arrowHelper") :kind)))
      ;; Async flag.
      (should (plist-get (funcall by-name "fetchValue") :async))
      (should (plist-get (funcall by-name "privateArrow") :async))
      (should-not (plist-get (funcall by-name "plainFunc") :async)))))

(ert-deftest anvil-ts-test-list-functions-source-order ()
  "Functions come back sorted by :start so callers can trust order."
  (anvil-ts-test--requires 'typescript
    (let* ((fns (anvil-ts-list-functions anvil-ts-test--ts-fixture))
           (starts (mapcar (lambda (p) (plist-get (plist-get p :bounds) :start))
                           fns)))
      (should (equal starts (sort (copy-sequence starts) #'<))))))

(ert-deftest anvil-ts-test-list-classes ()
  (anvil-ts-test--requires 'typescript
    (let* ((cls (anvil-ts-list-classes anvil-ts-test--ts-fixture))
           (names (mapcar (lambda (p) (plist-get p :name)) cls)))
      (should (member "ReportWriter" names))
      (should (member "Config" names)))))

(ert-deftest anvil-ts-test-list-methods-reportwriter ()
  (anvil-ts-test--requires 'typescript
    (let* ((ms (anvil-ts-list-methods anvil-ts-test--ts-fixture "ReportWriter"))
           (names (mapcar (lambda (p) (plist-get p :name)) ms)))
      (should (member "constructor" names))
      (should (member "write" names))
      (should (member "defaultName" names))
      (should (member "writeAsync" names))
      (should (= 4 (length ms)))
      (let ((wa (cl-find "writeAsync" ms :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=))
            (dn (cl-find "defaultName" ms :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=)))
        (should (plist-get wa :async))
        (should (plist-get dn :static))
        (should-not (plist-get wa :static))))))

(ert-deftest anvil-ts-test-list-methods-config ()
  (anvil-ts-test--requires 'typescript
    (let* ((ms (anvil-ts-list-methods anvil-ts-test--ts-fixture "Config"))
           (names (mapcar (lambda (p) (plist-get p :name)) ms)))
      (should (equal names '("constructor" "describe"))))))

(ert-deftest anvil-ts-test-list-methods-nonexistent-class ()
  (anvil-ts-test--requires 'typescript
    (should (null (anvil-ts-list-methods anvil-ts-test--ts-fixture
                                         "DoesNotExist")))))

(ert-deftest anvil-ts-test-list-interfaces ()
  (anvil-ts-test--requires 'typescript
    (let* ((intfs (anvil-ts-list-interfaces anvil-ts-test--ts-fixture))
           (names (mapcar (lambda (p) (plist-get p :name)) intfs)))
      (should (equal (sort (copy-sequence names) #'string-lessp)
                     '("Internal" "ReportInput")))
      (should (cl-every (lambda (p) (eq 'interface (plist-get p :kind)))
                        intfs)))))

(ert-deftest anvil-ts-test-list-type-aliases ()
  (anvil-ts-test--requires 'typescript
    (let* ((tas (anvil-ts-list-type-aliases anvil-ts-test--ts-fixture))
           (names (mapcar (lambda (p) (plist-get p :name)) tas)))
      (should (equal (sort (copy-sequence names) #'string-lessp)
                     '("Callback" "ReportMode")))
      (should (cl-every (lambda (p) (eq 'type (plist-get p :kind))) tas)))))

(ert-deftest anvil-ts-test-find-definition-function ()
  (anvil-ts-test--requires 'typescript
    (let ((d (anvil-ts-find-definition anvil-ts-test--ts-fixture "plainFunc")))
      (should d)
      (should (eq 'function (plist-get d :kind)))
      (should (string= "plainFunc" (plist-get d :name))))))

(ert-deftest anvil-ts-test-find-definition-class ()
  (anvil-ts-test--requires 'typescript
    (let ((d (anvil-ts-find-definition anvil-ts-test--ts-fixture "ReportWriter")))
      (should d)
      (should (eq 'class (plist-get d :kind))))))

(ert-deftest anvil-ts-test-find-definition-interface ()
  (anvil-ts-test--requires 'typescript
    (let ((d (anvil-ts-find-definition anvil-ts-test--ts-fixture "ReportInput")))
      (should d)
      (should (eq 'interface (plist-get d :kind))))))

(ert-deftest anvil-ts-test-find-definition-type ()
  (anvil-ts-test--requires 'typescript
    (let ((d (anvil-ts-find-definition anvil-ts-test--ts-fixture "ReportMode")))
      (should d)
      (should (eq 'type (plist-get d :kind))))))

(ert-deftest anvil-ts-test-find-definition-missing-returns-nil ()
  (anvil-ts-test--requires 'typescript
    (should (null (anvil-ts-find-definition anvil-ts-test--ts-fixture
                                            "no_such")))))

(ert-deftest anvil-ts-test-surrounding-form-inside-method ()
  (anvil-ts-test--requires 'typescript
    (let* ((cls (anvil-ts-find-definition anvil-ts-test--ts-fixture
                                          "ReportWriter"))
           (b  (plist-get cls :bounds))
           ;; Point near the class header, definitely inside the
           ;; class_declaration range.
           (mid (+ (plist-get b :start) 8))
           (encl (anvil-ts-surrounding-form anvil-ts-test--ts-fixture mid)))
      (should encl)
      (should (string= "ReportWriter" (plist-get encl :name))))))

(ert-deftest anvil-ts-test-surrounding-form-outside-returns-nil ()
  (anvil-ts-test--requires 'typescript
    (should (null (anvil-ts-surrounding-form anvil-ts-test--ts-fixture 1)))))

(ert-deftest anvil-ts-test-surrounding-form-kind-filter-arrow ()
  (anvil-ts-test--requires 'typescript
    (let* ((fns (anvil-ts-list-functions anvil-ts-test--ts-fixture))
           (arrow (cl-find 'arrow fns
                           :key (lambda (p) (plist-get p :kind))))
           (b (plist-get arrow :bounds))
           (mid (+ (plist-get b :start) 4)))
      (let ((hit (anvil-ts-surrounding-form
                  anvil-ts-test--ts-fixture mid :kind 'arrow)))
        (should hit)
        (should (eq 'arrow (plist-get hit :kind)))))))

;;;; --- TSX dispatch -------------------------------------------------------

(ert-deftest anvil-ts-test-tsx-dispatch-parses-jsx ()
  "TSX fixture routes through the `tsx' grammar; jsx inside a function
body parses without an error."
  (anvil-ts-test--requires 'tsx
    (let* ((fns (anvil-ts-list-functions anvil-ts-test--tsx-fixture))
           (cls (anvil-ts-list-classes anvil-ts-test--tsx-fixture))
           (intfs (anvil-ts-list-interfaces anvil-ts-test--tsx-fixture))
           (fn-names (mapcar (lambda (p) (plist-get p :name)) fns))
           (cls-names (mapcar (lambda (p) (plist-get p :name)) cls)))
      (should (member "Button" fn-names))
      (should (member "Greeting" fn-names))
      (should (member "ThemedPanel" cls-names))
      (should (cl-find "ButtonProps" intfs
                       :key (lambda (p) (plist-get p :name))
                       :test #'string=)))))

;;;; --- error paths --------------------------------------------------------

(ert-deftest anvil-ts-test-list-imports-errors-on-missing-file ()
  (anvil-ts-test--requires 'typescript
    (should-error (anvil-ts-list-imports "/tmp/definitely-not-there.ts")
                  :type 'user-error)))

(ert-deftest anvil-ts-test-list-imports-errors-on-nil-file ()
  (should-error (anvil-ts-list-imports nil) :type 'user-error))

(provide 'anvil-ts-test)
;;; anvil-ts-test.el ends here

;;; anvil-js.el --- JavaScript structural locators via tree-sitter  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Phase 1b of Doc 21 (docs/design/21-treesit-edits.org).  Provides
;; read-only structural locators for JavaScript, MJS, CJS, and JSX
;; source files via the tree-sitter-javascript grammar.
;;
;; All the heavy lifting --- queries, capture collectors, node
;; helpers --- lives in `anvil-ts.el' under `anvil-ts--*-lang'
;; internals.  This module only owns:
;;   - the language dispatch (all supported extensions map to
;;     `javascript')
;;   - public `anvil-js-*' wrappers that omit the TS-only kinds
;;     (no interfaces, no type aliases)
;;   - MCP tool registrations on the shared `emacs-eval' server
;;
;; Requiring `anvil-ts' at load time is intentional: `anvil-js-enable'
;; does not enable ts-* tools, because the tool registry is a side
;; effect of `anvil-ts--register-tools', not of `(require 'anvil-ts)'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)
(require 'anvil-ide-treesit)
(require 'anvil-ts)

(defconst anvil-js--server-id "emacs-eval"
  "Server ID under which js-* MCP tools are registered.")

(defconst anvil-js--lang 'javascript)

(defun anvil-js--lang-for-file (file)
  "Return `javascript' for any .js / .jsx / .mjs / .cjs file.
Errors when FILE is not a JavaScript-family extension --- the caller
should have used `anvil-ts-*' for .ts / .tsx."
  (let ((detected (anvil-treesit-language-for-file file)))
    (unless (eq detected 'javascript)
      (user-error
       "anvil-js: expected .js/.jsx/.mjs/.cjs file, got %s (lang=%S)"
       (or file "nil") detected))
    anvil-js--lang))

;;;; --- public JS surface --------------------------------------------------

(defun anvil-js-list-imports (file)
  "Return a list of plists describing imports in FILE."
  (anvil-js--lang-for-file file)
  (anvil-ts--list-imports-lang file anvil-js--lang))

(defun anvil-js-list-exports (file)
  "Return a list of plists describing exports in FILE."
  (anvil-js--lang-for-file file)
  (anvil-ts--list-exports-lang file anvil-js--lang))

(defun anvil-js-list-functions (file)
  "Return function declarations + named arrow bindings in FILE."
  (anvil-js--lang-for-file file)
  (anvil-ts--list-functions-lang file anvil-js--lang))

(defun anvil-js-list-classes (file)
  "Return class declarations in FILE."
  (anvil-js--lang-for-file file)
  (anvil-ts--list-classes-lang file anvil-js--lang))

(defun anvil-js-list-methods (file class-name)
  "Return methods of CLASS-NAME directly defined in FILE."
  (anvil-js--lang-for-file file)
  (anvil-ts--list-methods-lang file anvil-js--lang class-name))

(defun anvil-js-find-definition (file name)
  "Find a function or class named NAME in FILE.
Interfaces and type aliases are TypeScript-only and therefore not
considered here."
  (anvil-js--lang-for-file file)
  (anvil-ts--find-definition-lang file anvil-js--lang name nil))

(cl-defun anvil-js-surrounding-form (file point &key kind)
  "Return the innermost JS form covering POINT in FILE.
KIND optionally filters to `function' / `arrow' / `class'."
  (anvil-js--lang-for-file file)
  (anvil-ts--surrounding-form-lang file anvil-js--lang point kind nil))

;;;; --- public edit API (Phase 2) ------------------------------------------

(cl-defun anvil-js-add-import (file spec &key apply)
  "Add the import specified by SPEC to FILE.
Mirrors `anvil-ts-add-import' minus :type-only (JavaScript has no
type-only imports — the flag is silently ignored).  Returns the
edit plan unless APPLY is truthy."
  (anvil-js--lang-for-file file)
  (let ((plan (anvil-ts--plan-add-import
               anvil-js--lang file
               (let ((copy (copy-sequence spec)))
                 (plist-put copy :type-only nil)))))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-js-remove-import (file spec &key apply)
  "Remove the import specified by SPEC from FILE.
Mirrors `anvil-ts-remove-import'.  Returns the plan unless APPLY
is truthy."
  (anvil-js--lang-for-file file)
  (let ((plan (anvil-ts--plan-remove-import
               anvil-js--lang file spec)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-js-rename-import (file old new &key apply)
  "Rename the binding OLD to NEW across imports in FILE.
Mirrors `anvil-ts-rename-import'.  Returns the plan unless APPLY
is truthy."
  (anvil-js--lang-for-file file)
  (let ((plan (anvil-ts--plan-rename-import
               anvil-js--lang file old new)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-js-replace-function (file name new-source &key class apply)
  "Replace the def named NAME in FILE with NEW-SOURCE.
Mirrors `anvil-ts-replace-function'.  Returns the plan unless
APPLY is truthy."
  (anvil-js--lang-for-file file)
  (let ((plan (anvil-ts--plan-replace-function
               anvil-js--lang file name new-source
               (and class (if (symbolp class) (symbol-name class) class)))))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-js-wrap-expr (file start end wrapper &key apply)
  "Wrap the expression [START, END) of FILE with WRAPPER.
Mirrors `anvil-ts-wrap-expr'.  Returns the plan unless APPLY is
truthy."
  (anvil-js--lang-for-file file)
  (let ((plan (anvil-ts--plan-wrap-expr
               anvil-js--lang file start end wrapper)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

;;;; --- MCP handlers -------------------------------------------------------

(defun anvil-js--tool-list-imports (file)
  "MCP wrapper — list JS / JSX imports.

MCP Parameters:
  file - absolute path to the .js/.jsx/.mjs/.cjs file to analyze"
  (anvil-server-with-error-handling
   (anvil-js-list-imports file)))

(defun anvil-js--tool-list-exports (file)
  "MCP wrapper — list JS / JSX exports.

MCP Parameters:
  file - absolute path to the .js/.jsx/.mjs/.cjs file to analyze"
  (anvil-server-with-error-handling
   (anvil-js-list-exports file)))

(defun anvil-js--tool-list-functions (file)
  "MCP wrapper — list JS / JSX function declarations and named arrows.

MCP Parameters:
  file - absolute path to the .js/.jsx/.mjs/.cjs file to analyze"
  (anvil-server-with-error-handling
   (anvil-js-list-functions file)))

(defun anvil-js--tool-list-classes (file)
  "MCP wrapper — list JS / JSX class declarations.

MCP Parameters:
  file - absolute path to the .js/.jsx/.mjs/.cjs file to analyze"
  (anvil-server-with-error-handling
   (anvil-js-list-classes file)))

(defun anvil-js--tool-list-methods (file class-name)
  "MCP wrapper — list direct methods of CLASS-NAME.

MCP Parameters:
  file       - absolute path to the .js/.jsx/.mjs/.cjs file to analyze
  class-name - class name whose methods to list"
  (anvil-server-with-error-handling
   (anvil-js-list-methods file class-name)))

(defun anvil-js--tool-find-definition (file name)
  "MCP wrapper — find a function or class by NAME.

MCP Parameters:
  file - absolute path to the .js/.jsx/.mjs/.cjs file to analyze
  name - identifier to find"
  (anvil-server-with-error-handling
   (or (anvil-js-find-definition file name)
       (list :found nil :name name))))

(defun anvil-js--tool-surrounding-form (file point kind)
  "MCP wrapper — return the JS form enclosing POINT.

MCP Parameters:
  file  - absolute path to the .js/.jsx/.mjs/.cjs file to analyze
  point - 1-based buffer point inside the file
  kind  - optional \"function\" / \"arrow\" / \"class\"; empty / nil
          matches any"
  (anvil-server-with-error-handling
   (let* ((p (cond ((integerp point) point)
                   ((stringp point) (string-to-number point))
                   (t (user-error "anvil-js: point must be an integer"))))
          (k (pcase kind
               ((pred null) nil)
               ((pred stringp)
                (if (string-empty-p kind) nil (intern kind)))
               ((pred symbolp) kind)
               (_ nil))))
     (or (anvil-js-surrounding-form file p :kind k)
         (list :found nil :point p)))))

(defun anvil-js--tool-add-import (file spec apply)
  "MCP wrapper — add an import.

MCP Parameters:
  file  - absolute path to the .js/.jsx/.mjs/.cjs file to edit
  spec  - plist (:from MODULE :default NAME :named LIST)
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-js-add-import file (anvil-ts--coerce-spec spec) :apply apply)))

(defun anvil-js--tool-remove-import (file spec apply)
  "MCP wrapper — remove an import.

MCP Parameters:
  file  - absolute path to the .js/.jsx/.mjs/.cjs file to edit
  spec  - plist (:from MODULE :default NAME :named LIST)
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-js-remove-import file (anvil-ts--coerce-spec spec) :apply apply)))

(defun anvil-js--tool-rename-import (file old new apply)
  "MCP wrapper — rename an imported binding alias.

MCP Parameters:
  file  - absolute path to the .js/.jsx/.mjs/.cjs file to edit
  old   - current binding name
  new   - new binding name
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-js-rename-import file old new :apply apply)))

(defun anvil-js--tool-replace-function (file name new-source class apply)
  "MCP wrapper — replace a JS / JSX function or method body.

MCP Parameters:
  file       - absolute path to the .js/.jsx/.mjs/.cjs file to edit
  name       - identifier of the function / method to replace
  new-source - full replacement source
  class      - optional enclosing class name; empty / nil means any
  apply      - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (let ((cls (cond ((null class) nil)
                    ((and (stringp class) (string-empty-p class)) nil)
                    (t class))))
     (anvil-js-replace-function file name new-source
                                :class cls :apply apply))))

(defun anvil-js--tool-wrap-expr (file start end wrapper apply)
  "MCP wrapper — wrap a JS / JSX expression.

MCP Parameters:
  file    - absolute path to the .js/.jsx/.mjs/.cjs file to edit
  start   - 1-based buffer point at the start of the expression
  end     - 1-based buffer point at the end of the expression
  wrapper - source template containing `|anvil-hole|' exactly once
  apply   - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (let ((s (if (stringp start) (string-to-number start) start))
         (e (if (stringp end) (string-to-number end) end)))
     (anvil-js-wrap-expr file s e wrapper :apply apply))))

;;;; --- module lifecycle ---------------------------------------------------

(defconst anvil-js--tool-ids
  '("js-list-imports"
    "js-list-exports"
    "js-list-functions"
    "js-list-classes"
    "js-list-methods"
    "js-find-definition"
    "js-surrounding-form"
    "js-add-import"
    "js-remove-import"
    "js-rename-import"
    "js-replace-function"
    "js-wrap-expr")
  "Stable list of MCP tool ids registered by `anvil-js-enable'.")

(defun anvil-js--register-tools ()
  "Register the Phase 1b + 2 js-* MCP tools on `anvil-js--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-imports)
   :id "js-list-imports"
   :intent '(js-read structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "List every `import ...' statement in a JavaScript,
MJS, CJS, or JSX file.  Returns an ordered list of (:kind :text
:bounds) plists, preserving the literal source text and 1-based
source range."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-exports)
   :id "js-list-exports"
   :intent '(js-read structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "List every `export ...' statement in a JS / JSX
file.  Returns (:kind :name :text :bounds); :kind is one of
`function' / `class' / `const' / `var' / `reexport' / `other'."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-functions)
   :id "js-list-functions"
   :intent '(js-read structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "List function declarations and named arrow bindings
in a JS / JSX file.  Returns (:kind :name :async :class-name :bounds);
:kind is `function' for `function f()' / `function *gen()' and
`arrow' for `const f = () => ...'."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-classes)
   :id "js-list-classes"
   :intent '(js-read structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "List class declarations in a JS / JSX file.
Returns (:kind 'class :name :bounds) per entry."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-methods)
   :id "js-list-methods"
   :intent '(js-read structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "List methods defined directly inside the class named
CLASS-NAME.  Nested classes' methods are excluded.  Returns
\(:kind 'method :class-name :name :async :static :bounds)."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-find-definition)
   :id "js-find-definition"
   :intent '(js-read)
   :layer 'core
   :server-id anvil-js--server-id
   :description "Find the first function or class named NAME in a
JS / JSX file.  Source-order wins when the name is shared; use
`js-list-functions' / `js-list-classes' for exhaustive results."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-surrounding-form)
   :id "js-surrounding-form"
   :intent '(js-read structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "Return the innermost function or class whose source
range contains the 1-based buffer POINT.  KIND restricts the match
\(`function' / `arrow' / `class'); empty / nil matches any."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-add-import)
   :id "js-add-import"
   :intent '(js-edit structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "Add an import to a JS / JSX file.  SPEC is a plist:
(:from MODULE :default NAME :named LIST).  :named entries may be
plain NAME strings, (NAME . ALIAS) cons cells, or (:name :alias)
plists.  Idempotent — merges into an existing `import ... from
MODULE'.  Preview-default; pass :apply t to write.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-remove-import)
   :id "js-remove-import"
   :intent '(js-edit structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "Remove an import from a JS / JSX file.  SPEC shape
matches `js-add-import'.  :named entries are removed from the
existing statement; :default drops the default specifier.  When
nothing remains the statement is deleted.  Idempotent.  Preview-
default.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-rename-import)
   :id "js-rename-import"
   :intent '(js-edit structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "Rename the binding OLD to NEW across JS / JSX
imports.  Walks every `import_statement' and rewrites the unique
specifier (default / named / named-alias / namespace) whose
visible binding is OLD.  Errors on ambiguity or absence.
Reference use sites are not touched.  Preview-default.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-replace-function)
   :id "js-replace-function"
   :intent '(js-edit structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "Replace a JS / JSX function or method body with
NEW-SOURCE.  NEW-SOURCE is the full def at column 0; dedented +
reindented to the existing def's column.  CLASS disambiguates
methods of the same name.  Double-apply with identical NEW-SOURCE
is a no-op.  Preview-default.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-wrap-expr)
   :id "js-wrap-expr"
   :intent '(js-edit structure)
   :layer 'core
   :server-id anvil-js--server-id
   :description "Wrap the expression at [START, END) with WRAPPER.
WRAPPER is a source template containing `|anvil-hole|' exactly
once.  The range must align to an exact tree-sitter node boundary,
or the tool errors rather than produce invalid source.  Preview-
default."))

(defun anvil-js-enable ()
  "Enable the Phase 1b + 2 js-* MCP tools."
  (interactive)
  (anvil-js--register-tools))

(defun anvil-js-disable ()
  "Unregister the Phase 1b + 2 js-* MCP tools."
  (interactive)
  (dolist (id anvil-js--tool-ids)
    (anvil-server-unregister-tool id anvil-js--server-id)))

(provide 'anvil-js)
;;; anvil-js.el ends here

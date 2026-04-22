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
(require 'anvil-treesit)
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

;;;; --- module lifecycle ---------------------------------------------------

(defconst anvil-js--tool-ids
  '("js-list-imports"
    "js-list-exports"
    "js-list-functions"
    "js-list-classes"
    "js-list-methods"
    "js-find-definition"
    "js-surrounding-form")
  "Stable list of MCP tool ids registered by `anvil-js-enable'.")

(defun anvil-js--register-tools ()
  "Register the Phase 1b js-* MCP tools on `anvil-js--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-imports)
   :id "js-list-imports"
   :server-id anvil-js--server-id
   :description "List every `import ...' statement in a JavaScript,
MJS, CJS, or JSX file.  Returns an ordered list of (:kind :text
:bounds) plists, preserving the literal source text and 1-based
source range."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-exports)
   :id "js-list-exports"
   :server-id anvil-js--server-id
   :description "List every `export ...' statement in a JS / JSX
file.  Returns (:kind :name :text :bounds); :kind is one of
`function' / `class' / `const' / `var' / `reexport' / `other'."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-functions)
   :id "js-list-functions"
   :server-id anvil-js--server-id
   :description "List function declarations and named arrow bindings
in a JS / JSX file.  Returns (:kind :name :async :class-name :bounds);
:kind is `function' for `function f()' / `function *gen()' and
`arrow' for `const f = () => ...'."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-classes)
   :id "js-list-classes"
   :server-id anvil-js--server-id
   :description "List class declarations in a JS / JSX file.
Returns (:kind 'class :name :bounds) per entry."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-list-methods)
   :id "js-list-methods"
   :server-id anvil-js--server-id
   :description "List methods defined directly inside the class named
CLASS-NAME.  Nested classes' methods are excluded.  Returns
\(:kind 'method :class-name :name :async :static :bounds)."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-find-definition)
   :id "js-find-definition"
   :server-id anvil-js--server-id
   :description "Find the first function or class named NAME in a
JS / JSX file.  Source-order wins when the name is shared; use
`js-list-functions' / `js-list-classes' for exhaustive results."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-js--tool-surrounding-form)
   :id "js-surrounding-form"
   :server-id anvil-js--server-id
   :description "Return the innermost function or class whose source
range contains the 1-based buffer POINT.  KIND restricts the match
\(`function' / `arrow' / `class'); empty / nil matches any."
   :read-only t))

(defun anvil-js-enable ()
  "Enable the Phase 1b js-* MCP tools."
  (interactive)
  (anvil-js--register-tools))

(defun anvil-js-disable ()
  "Unregister the Phase 1b js-* MCP tools."
  (interactive)
  (dolist (id anvil-js--tool-ids)
    (anvil-server-unregister-tool id anvil-js--server-id)))

(provide 'anvil-js)
;;; anvil-js.el ends here

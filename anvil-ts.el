;;; anvil-ts.el --- TypeScript structural locators via tree-sitter  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Phase 1b of Doc 21 (docs/design/21-treesit-edits.org).  Provides
;; read-only structural locators for TypeScript (.ts) and TSX (.tsx)
;; source files, built on the anvil-treesit shared core.  The
;; tree-sitter-typescript repository ships two grammars --- one for
;; plain TypeScript and one for TSX --- and this module dispatches
;; between them by file extension.
;;
;; All tools here are read-only; edit primitives land in Phase 2.
;;
;; Surface (elisp):
;;   (anvil-ts-list-imports       FILE)
;;   (anvil-ts-list-exports       FILE)
;;   (anvil-ts-list-functions     FILE)
;;   (anvil-ts-list-classes       FILE)
;;   (anvil-ts-list-methods       FILE CLASS-NAME)
;;   (anvil-ts-list-interfaces    FILE)       ; TS-only node type
;;   (anvil-ts-list-type-aliases  FILE)
;;   (anvil-ts-find-definition    FILE NAME)
;;   (anvil-ts-surrounding-form   FILE POINT &key kind)
;;
;; Internal helpers `anvil-ts--list-<op>-lang' take an explicit
;; language symbol so `anvil-js.el' can reuse them with `javascript'.
;; The JS surface drops `list-interfaces' / `list-type-aliases' since
;; those node types do not exist in the JavaScript grammar.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)
(require 'anvil-treesit)

(defconst anvil-ts--server-id "emacs-eval"
  "Server ID under which ts-* MCP tools are registered.")

;;;; --- file → language dispatch -------------------------------------------

(defun anvil-ts--lang-for-file (file)
  "Return `typescript' or `tsx' for FILE by extension.
Errors when FILE is neither .ts nor .tsx --- anvil-ts is the
TypeScript-family dispatcher, so a mismatched extension means the
caller should have used `anvil-js-*' instead."
  (pcase (anvil-treesit-language-for-file file)
    ('typescript 'typescript)
    ('tsx        'tsx)
    (other (user-error
            "anvil-ts: expected .ts or .tsx file, got %s (lang=%S)"
            (or file "nil") other))))

;;;; --- queries ------------------------------------------------------------

;; Each query captures the whole declaration node under a single
;; named capture; the collector extracts the name from the node's
;; `name' field, matching the anvil-py pattern.  Keeping one capture
;; per pattern avoids the tree-order-vs-pattern-order pairing hazard
;; called out in `anvil-py--captures-grouped'.

(defconst anvil-ts--queries
  '((imports
     . "(import_statement) @im")
    (exports
     . "(export_statement) @ex")
    (functions
     . "[(function_declaration) @fn
        (generator_function_declaration) @fn]")
    (arrow-bindings
     ;; Named arrow functions: `const foo = () => ...' /
     ;; `let foo = async () => ...'.  The declarator's name is the
     ;; function's callable identity.
     . "(lexical_declaration
          (variable_declarator
            name: (identifier) @arrow-name
            value: [(arrow_function) @arrow
                    (function_expression) @arrow]))")
    (classes
     . "(class_declaration) @cls")
    (methods
     . "(method_definition) @method")
    (interfaces
     . "(interface_declaration) @intf")
    (type-aliases
     . "(type_alias_declaration) @ta"))
  "Tree-sitter query source strings keyed by operation symbol.
Compiled via `anvil-treesit-compile-query' and cached under
\(LANG . OP).  Same sources are reused for `typescript' / `tsx' /
`javascript'; the JavaScript grammar shares every node type listed
here except `interface_declaration' and `type_alias_declaration',
which the JS query set never asks for.")

(defun anvil-ts--query (lang op)
  "Return the compiled tree-sitter query for (LANG . OP)."
  (let ((src (alist-get op anvil-ts--queries)))
    (unless src
      (error "anvil-ts: unknown query op %S" op))
    (anvil-treesit-compile-query lang op src)))

;;;; --- node helpers -------------------------------------------------------

(defun anvil-ts--node-name (node)
  "Return the text of NODE's `name' field, or empty string if missing.
Works for function_declaration, class_declaration,
interface_declaration, type_alias_declaration, and
method_definition."
  (let ((n (treesit-node-child-by-field-name node "name")))
    (if n (anvil-treesit-node-text n) "")))

(defun anvil-ts--async-node-p (node)
  "Return non-nil when NODE has an `async' modifier child.
The TS / JS grammars expose `async' as an anonymous leading child
rather than a named field."
  (let ((c0 (treesit-node-child node 0)))
    (and c0 (string= (treesit-node-text c0 t) "async"))))

(defun anvil-ts--enclosing-class-name (node)
  "Return the name of the class body directly containing NODE, or nil.
Climbs past a `class_body' parent to the `class_declaration', then
reads the `name' field.  Methods buried inside a nested arrow body
or another function do not count --- only direct members of a class
match, matching anvil-py's behaviour."
  (let ((body (treesit-node-parent node)))
    (when (and body (string= (treesit-node-type body) "class_body"))
      (let ((cls (treesit-node-parent body)))
        (when (and cls (string= (treesit-node-type cls) "class_declaration"))
          (anvil-ts--node-name cls))))))

(defun anvil-ts--export-target-name (export-node)
  "Return (:kind :name) for the declaration inside an `export_statement'.
Handles:
  `export function f' / `export class C' / `export interface I' /
  `export type T' / `export const x = ...' / `export default <expr>'.
Returns (:kind 'reexport :name nil) for `export { a, b }' re-exports
and (:kind 'side-effect :name nil) for bare `export *' etc."
  (let ((kid (treesit-node-child export-node 1))
        kind name)
    (cond
     ((null kid) (list :kind 'reexport :name nil))
     (t
      (setq kind (pcase (treesit-node-type kid)
                   ("function_declaration"        'function)
                   ("generator_function_declaration" 'function)
                   ("class_declaration"           'class)
                   ("interface_declaration"       'interface)
                   ("type_alias_declaration"      'type)
                   ("lexical_declaration"         'const)
                   ("variable_declaration"        'var)
                   ("enum_declaration"            'enum)
                   ("export_clause"               'reexport)
                   (_                             'other)))
      (setq name
            (pcase kind
              ('const
               (let ((decl (car (cl-remove-if-not
                                 (lambda (c)
                                   (string= (treesit-node-type c)
                                            "variable_declarator"))
                                 (treesit-node-children kid t)))))
                 (and decl (anvil-ts--node-name decl))))
              ('var
               (let ((decl (car (cl-remove-if-not
                                 (lambda (c)
                                   (string= (treesit-node-type c)
                                            "variable_declarator"))
                                 (treesit-node-children kid t)))))
                 (and decl (anvil-ts--node-name decl))))
              ((or 'function 'class 'interface 'type 'enum)
               (anvil-ts--node-name kid))
              (_ nil)))
      (list :kind kind :name name)))))

;;;; --- internal list helpers (language-parametric) ------------------------

(defun anvil-ts--list-imports-lang (file lang)
  "Return a list of plists describing imports in FILE parsed under LANG.
Each entry: (:kind 'import :text STR :bounds PLIST).  The single
`:kind' value is deliberate --- the TS / JS grammars do not split
`import X from Y' and `import \"side\"' into different node types,
so the distinction has to come from inspecting `:text' rather than
:kind."
  (anvil-treesit-with-root file lang root
    (let ((q (anvil-ts--query lang 'imports))
          results)
      (dolist (cap (treesit-query-capture root q))
        (let ((node (cdr cap)))
          (push (list :kind 'import
                      :text (anvil-treesit-node-text node)
                      :bounds (anvil-treesit-node-bounds node))
                results)))
      (nreverse results))))

(defun anvil-ts--list-exports-lang (file lang)
  "Return a list of plists describing exports in FILE parsed under LANG.
Each entry: (:kind SYM :name STR-OR-NIL :text STR :bounds PLIST).
See `anvil-ts--export-target-name' for the :kind vocabulary."
  (anvil-treesit-with-root file lang root
    (let ((q (anvil-ts--query lang 'exports))
          results)
      (dolist (cap (treesit-query-capture root q))
        (let* ((node (cdr cap))
               (target (anvil-ts--export-target-name node)))
          (push (list :kind (plist-get target :kind)
                      :name (plist-get target :name)
                      :text (anvil-treesit-node-text node)
                      :bounds (anvil-treesit-node-bounds node))
                results)))
      (nreverse results))))

(defun anvil-ts--list-functions-lang (file lang)
  "Return functions and named arrow bindings defined in FILE under LANG.
Each entry: (:kind 'function|'arrow :name STR :async BOOL
:class-name STR-OR-NIL :bounds PLIST).  `function_declaration' and
`generator_function_declaration' are returned as kind=function;
`const NAME = (...) => ...' / `let NAME = (...) => ...' are
returned as kind=arrow with :name coming from the declarator."
  (anvil-treesit-with-root file lang root
    (let (results)
      ;; Regular function declarations.
      (dolist (cap (treesit-query-capture
                    root (anvil-ts--query lang 'functions)))
        (when (eq (car cap) 'fn)
          (let* ((node (cdr cap))
                 (cls (anvil-ts--enclosing-class-name node)))
            (push (list :kind 'function
                        :name (anvil-ts--node-name node)
                        :async (and (anvil-ts--async-node-p node) t)
                        :class-name cls
                        :bounds (anvil-treesit-node-bounds node))
                  results))))
      ;; Arrow-function bindings (named via their enclosing
      ;; variable_declarator).  The capture comes back paired:
      ;; @arrow-name + @arrow per match.
      (let* ((caps (treesit-query-capture
                    root (anvil-ts--query lang 'arrow-bindings)))
             (by-pair (make-hash-table :test 'equal)))
        (dolist (cap caps)
          (let* ((node (cdr cap))
                 (parent (treesit-node-parent node))
                 ;; Group captures that share a variable_declarator
                 ;; parent.  The declarator's (:start . :end) is a
                 ;; stable key even when the arrow node has a wider
                 ;; span.
                 (key (when parent
                        (cons (treesit-node-start parent)
                              (treesit-node-end parent)))))
            (when key
              (push cap (gethash key by-pair)))))
        (maphash
         (lambda (_k pair)
           (let* ((name-cap (cl-find 'arrow-name pair :key #'car))
                  (arrow-cap (cl-find 'arrow pair :key #'car)))
             (when (and name-cap arrow-cap)
               (let* ((arrow-node (cdr arrow-cap))
                      (name-node (cdr name-cap))
                      (declarator (treesit-node-parent arrow-node))
                      (bounds (if declarator
                                  (anvil-treesit-node-bounds declarator)
                                (anvil-treesit-node-bounds arrow-node))))
                 (push (list :kind 'arrow
                             :name (anvil-treesit-node-text name-node)
                             :async (and (anvil-ts--async-node-p arrow-node) t)
                             :class-name nil
                             :bounds bounds)
                       results)))))
         by-pair))
      ;; Sort by :start so callers see declarations in source order.
      (sort (nreverse results)
            (lambda (a b)
              (< (plist-get (plist-get a :bounds) :start)
                 (plist-get (plist-get b :bounds) :start)))))))

(defun anvil-ts--list-classes-lang (file lang)
  "Return class declarations defined in FILE parsed under LANG.
Each entry: (:kind 'class :name STR :async nil :bounds PLIST)."
  (anvil-treesit-with-root file lang root
    (let ((q (anvil-ts--query lang 'classes))
          results)
      (dolist (cap (treesit-query-capture root q))
        (let ((node (cdr cap)))
          (push (list :kind 'class
                      :name (anvil-ts--node-name node)
                      :async nil
                      :class-name nil
                      :bounds (anvil-treesit-node-bounds node))
                results)))
      (nreverse results))))

(defun anvil-ts--list-methods-lang (file lang class-name)
  "Return methods defined directly inside CLASS-NAME in FILE under LANG.
Each entry: (:kind 'method :class-name STR :name STR :async BOOL
:static BOOL :bounds PLIST).  Nested classes' methods are excluded;
only the first-level body of a class named CLASS-NAME matches."
  (anvil-treesit-with-root file lang root
    (let ((q (anvil-ts--query lang 'methods))
          results)
      (dolist (cap (treesit-query-capture root q))
        (when (eq (car cap) 'method)
          (let* ((node (cdr cap))
                 (enclosing (anvil-ts--enclosing-class-name node)))
            (when (and enclosing (string= enclosing class-name))
              (let* ((static-p
                      (cl-some (lambda (c)
                                 (string= (treesit-node-text c t) "static"))
                               (treesit-node-children node nil))))
                (push (list :kind 'method
                            :class-name enclosing
                            :name (anvil-ts--node-name node)
                            :async (and (anvil-ts--async-node-p node) t)
                            :static (and static-p t)
                            :bounds (anvil-treesit-node-bounds node))
                      results))))))
      (nreverse results))))

(defun anvil-ts--list-interfaces-lang (file lang)
  "Return TypeScript interface declarations in FILE parsed under LANG.
Valid only for `typescript' / `tsx'; the JavaScript grammar has no
`interface_declaration' node so callers pass a TS-family lang.
Each entry: (:kind 'interface :name STR :bounds PLIST)."
  (anvil-treesit-with-root file lang root
    (let ((q (anvil-ts--query lang 'interfaces))
          results)
      (dolist (cap (treesit-query-capture root q))
        (let ((node (cdr cap)))
          (push (list :kind 'interface
                      :name (anvil-ts--node-name node)
                      :bounds (anvil-treesit-node-bounds node))
                results)))
      (nreverse results))))

(defun anvil-ts--list-type-aliases-lang (file lang)
  "Return TypeScript type-alias declarations in FILE parsed under LANG.
Valid only for `typescript' / `tsx'.  Each entry:
\(:kind 'type :name STR :bounds PLIST)."
  (anvil-treesit-with-root file lang root
    (let ((q (anvil-ts--query lang 'type-aliases))
          results)
      (dolist (cap (treesit-query-capture root q))
        (let ((node (cdr cap)))
          (push (list :kind 'type
                      :name (anvil-ts--node-name node)
                      :bounds (anvil-treesit-node-bounds node))
                results)))
      (nreverse results))))

(defun anvil-ts--find-definition-lang (file lang name &optional include-ts-only)
  "Find a function, class, interface, or type alias named NAME in FILE.
When INCLUDE-TS-ONLY is non-nil, interfaces and type aliases are
considered (used by `anvil-ts-find-definition').  `anvil-js.el'
passes nil so JS lookups ignore TS-only kinds even when a caller
accidentally points an anvil-js tool at a .ts file.  Returns the
first match in source order, or nil."
  (let* ((fns (anvil-ts--list-functions-lang file lang))
         (cls (anvil-ts--list-classes-lang   file lang))
         (extra (and include-ts-only
                     (append
                      (anvil-ts--list-interfaces-lang   file lang)
                      (anvil-ts--list-type-aliases-lang file lang))))
         (all (append fns cls extra))
         (sorted (sort (copy-sequence all)
                       (lambda (a b)
                         (< (plist-get (plist-get a :bounds) :start)
                            (plist-get (plist-get b :bounds) :start))))))
    (cl-some (lambda (entry)
               (and (string= (plist-get entry :name) name) entry))
             sorted)))

(defun anvil-ts--surrounding-form-lang (file lang point kind include-ts-only)
  "Return the innermost function / class / interface covering POINT.
LANG is `typescript' / `tsx' / `javascript'.  KIND (optional)
restricts to `function', `arrow', `class', `interface', or `type';
nil matches any.  INCLUDE-TS-ONLY toggles interface / type
candidates (see `anvil-ts--find-definition-lang')."
  (let* ((fns (anvil-ts--list-functions-lang file lang))
         (cls (anvil-ts--list-classes-lang   file lang))
         (extra (and include-ts-only
                     (append
                      (anvil-ts--list-interfaces-lang   file lang)
                      (anvil-ts--list-type-aliases-lang file lang))))
         (candidates (pcase kind
                       ('function fns)
                       ('arrow (cl-remove-if-not
                                (lambda (e) (eq (plist-get e :kind) 'arrow))
                                fns))
                       ('class cls)
                       ('interface (and include-ts-only
                                        (anvil-ts--list-interfaces-lang
                                         file lang)))
                       ('type (and include-ts-only
                                   (anvil-ts--list-type-aliases-lang
                                    file lang)))
                       (_ (append fns cls extra))))
         (enclosing (cl-remove-if-not
                     (lambda (e)
                       (let* ((b (plist-get e :bounds))
                              (s (plist-get b :start))
                              (en (plist-get b :end)))
                         (and s en (<= s point en))))
                     candidates)))
    (when enclosing
      (car (sort (copy-sequence enclosing)
                 (lambda (a b)
                   (> (plist-get (plist-get a :bounds) :start)
                      (plist-get (plist-get b :bounds) :start))))))))

;;;; --- public TS surface --------------------------------------------------

(defun anvil-ts-list-imports (file)
  "Return a list of plists describing TS / TSX imports in FILE."
  (anvil-ts--list-imports-lang file (anvil-ts--lang-for-file file)))

(defun anvil-ts-list-exports (file)
  "Return a list of plists describing TS / TSX exports in FILE."
  (anvil-ts--list-exports-lang file (anvil-ts--lang-for-file file)))

(defun anvil-ts-list-functions (file)
  "Return TS / TSX function declarations + named arrow bindings in FILE."
  (anvil-ts--list-functions-lang file (anvil-ts--lang-for-file file)))

(defun anvil-ts-list-classes (file)
  "Return TS / TSX class declarations in FILE."
  (anvil-ts--list-classes-lang file (anvil-ts--lang-for-file file)))

(defun anvil-ts-list-methods (file class-name)
  "Return methods of CLASS-NAME directly defined in FILE."
  (anvil-ts--list-methods-lang
   file (anvil-ts--lang-for-file file) class-name))

(defun anvil-ts-list-interfaces (file)
  "Return TypeScript interface declarations in FILE."
  (anvil-ts--list-interfaces-lang file (anvil-ts--lang-for-file file)))

(defun anvil-ts-list-type-aliases (file)
  "Return TypeScript type alias declarations in FILE."
  (anvil-ts--list-type-aliases-lang file (anvil-ts--lang-for-file file)))

(defun anvil-ts-find-definition (file name)
  "Find a function / class / interface / type named NAME in FILE."
  (anvil-ts--find-definition-lang
   file (anvil-ts--lang-for-file file) name t))

(cl-defun anvil-ts-surrounding-form (file point &key kind)
  "Return the innermost TS form covering POINT in FILE.
KIND optionally filters to `function' / `arrow' / `class' /
`interface' / `type'."
  (anvil-ts--surrounding-form-lang
   file (anvil-ts--lang-for-file file) point kind t))

;;;; --- MCP handlers -------------------------------------------------------

(defun anvil-ts--tool-list-imports (file)
  "MCP wrapper — list TS / TSX imports.

MCP Parameters:
  file - absolute path to the .ts or .tsx file to analyze"
  (anvil-server-with-error-handling
   (anvil-ts-list-imports file)))

(defun anvil-ts--tool-list-exports (file)
  "MCP wrapper — list TS / TSX exports.

MCP Parameters:
  file - absolute path to the .ts or .tsx file to analyze"
  (anvil-server-with-error-handling
   (anvil-ts-list-exports file)))

(defun anvil-ts--tool-list-functions (file)
  "MCP wrapper — list TS / TSX function declarations and named arrows.

MCP Parameters:
  file - absolute path to the .ts or .tsx file to analyze"
  (anvil-server-with-error-handling
   (anvil-ts-list-functions file)))

(defun anvil-ts--tool-list-classes (file)
  "MCP wrapper — list TS / TSX class declarations.

MCP Parameters:
  file - absolute path to the .ts or .tsx file to analyze"
  (anvil-server-with-error-handling
   (anvil-ts-list-classes file)))

(defun anvil-ts--tool-list-methods (file class-name)
  "MCP wrapper — list direct methods of CLASS-NAME.

MCP Parameters:
  file       - absolute path to the .ts or .tsx file to analyze
  class-name - class name whose methods to list"
  (anvil-server-with-error-handling
   (anvil-ts-list-methods file class-name)))

(defun anvil-ts--tool-list-interfaces (file)
  "MCP wrapper — list TypeScript interface declarations.

MCP Parameters:
  file - absolute path to the .ts or .tsx file to analyze"
  (anvil-server-with-error-handling
   (anvil-ts-list-interfaces file)))

(defun anvil-ts--tool-list-type-aliases (file)
  "MCP wrapper — list TypeScript type-alias declarations.

MCP Parameters:
  file - absolute path to the .ts or .tsx file to analyze"
  (anvil-server-with-error-handling
   (anvil-ts-list-type-aliases file)))

(defun anvil-ts--tool-find-definition (file name)
  "MCP wrapper — find a function / class / interface / type by NAME.

MCP Parameters:
  file - absolute path to the .ts or .tsx file to analyze
  name - identifier to find"
  (anvil-server-with-error-handling
   (or (anvil-ts-find-definition file name)
       (list :found nil :name name))))

(defun anvil-ts--tool-surrounding-form (file point kind)
  "MCP wrapper — return the TS form enclosing POINT.

MCP Parameters:
  file  - absolute path to the .ts or .tsx file to analyze
  point - 1-based buffer point inside the file
  kind  - optional \"function\" / \"arrow\" / \"class\" / \"interface\"
          / \"type\"; empty / nil matches any"
  (anvil-server-with-error-handling
   (let* ((p (cond ((integerp point) point)
                   ((stringp point) (string-to-number point))
                   (t (user-error "anvil-ts: point must be an integer"))))
          (k (pcase kind
               ((pred null) nil)
               ((pred stringp)
                (if (string-empty-p kind) nil (intern kind)))
               ((pred symbolp) kind)
               (_ nil))))
     (or (anvil-ts-surrounding-form file p :kind k)
         (list :found nil :point p)))))

;;;; --- module lifecycle ---------------------------------------------------

(defconst anvil-ts--tool-ids
  '("ts-list-imports"
    "ts-list-exports"
    "ts-list-functions"
    "ts-list-classes"
    "ts-list-methods"
    "ts-list-interfaces"
    "ts-list-type-aliases"
    "ts-find-definition"
    "ts-surrounding-form")
  "Stable list of MCP tool ids registered by `anvil-ts-enable'.")

(defun anvil-ts--register-tools ()
  "Register the Phase 1b ts-* MCP tools on `anvil-ts--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-imports)
   :id "ts-list-imports"
   :server-id anvil-ts--server-id
   :description "List every `import ...' statement in a TypeScript or
TSX file.  Returns an ordered list of (:kind :text :bounds) plists,
preserving the literal source text and 1-based source range.  Side-
effect imports (`import \"x\"') and type imports (`import type {A}')
are included; the caller inspects :text to tell them apart."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-exports)
   :id "ts-list-exports"
   :server-id anvil-ts--server-id
   :description "List every `export ...' statement in a TypeScript or
TSX file.  Returns (:kind :name :text :bounds) per entry.  :kind is
one of `function' / `class' / `interface' / `type' / `const' / `var'
/ `enum' / `reexport' / `other'; :name is nil for `export { a, b }'
re-exports and for anonymous default exports."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-functions)
   :id "ts-list-functions"
   :server-id anvil-ts--server-id
   :description "List function declarations and named arrow bindings
in a TypeScript or TSX file.  Returns (:kind :name :async :class-name
:bounds); :kind is `function' for `function f()' /
`function *gen()' and `arrow' for `const f = () => ...'.  Arrows
passed as callbacks without a name are not included --- name them via
`const' if you want them surfaced."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-classes)
   :id "ts-list-classes"
   :server-id anvil-ts--server-id
   :description "List class declarations in a TypeScript or TSX file.
Returns (:kind 'class :name :bounds) per entry."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-methods)
   :id "ts-list-methods"
   :server-id anvil-ts--server-id
   :description "List methods defined directly inside the class named
CLASS-NAME.  Nested classes' methods are excluded; only the first-
level body of a class with that exact name matches.  Returns
\(:kind 'method :class-name :name :async :static :bounds)."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-interfaces)
   :id "ts-list-interfaces"
   :server-id anvil-ts--server-id
   :description "List TypeScript `interface' declarations in a .ts /
.tsx file.  Returns (:kind 'interface :name :bounds) per entry.
JavaScript files have no interfaces --- use `js-list-*' for .js /
.jsx."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-type-aliases)
   :id "ts-list-type-aliases"
   :server-id anvil-ts--server-id
   :description "List TypeScript `type' alias declarations in a .ts /
.tsx file.  Returns (:kind 'type :name :bounds) per entry."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-find-definition)
   :id "ts-find-definition"
   :server-id anvil-ts--server-id
   :description "Find the first function / class / interface / type
named NAME in a TypeScript or TSX file.  Source-order wins when the
name is shared across kinds; use the per-kind listers for exhaustive
results."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-surrounding-form)
   :id "ts-surrounding-form"
   :server-id anvil-ts--server-id
   :description "Return the innermost function / class / interface /
type whose source range contains the 1-based buffer POINT.  KIND
restricts the match (`function' / `arrow' / `class' / `interface' /
`type'); empty / nil matches any."
   :read-only t))

(defun anvil-ts-enable ()
  "Enable the Phase 1b ts-* MCP tools."
  (interactive)
  (anvil-ts--register-tools))

(defun anvil-ts-disable ()
  "Unregister the Phase 1b ts-* MCP tools."
  (interactive)
  (dolist (id anvil-ts--tool-ids)
    (anvil-server-unregister-tool id anvil-ts--server-id)))

(provide 'anvil-ts)
;;; anvil-ts.el ends here

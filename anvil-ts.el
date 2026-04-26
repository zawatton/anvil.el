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
;; Doc 38 Phase E — anvil-ide-treesit lives in zawatton/anvil-ide.el now.
;; We soft-require so anvil.el loads cleanly in NeLisp standalone tarball
;; (= no IDE layer); the treesit-* tools below will then signal at call
;; time, which is acceptable since they require Emacs anyway.
(require 'anvil-ide-treesit nil 'noerror)

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

;;;; --- import edit helpers (Phase 2) --------------------------------------

(defun anvil-ts--import-module-of (node)
  "Return the module string (no quotes) of an `import_statement' NODE."
  (let (mod)
    (dolist (c (treesit-node-children node t))
      (when (and (null mod)
                 (string= (treesit-node-type c) "string"))
        ;; Find the string_fragment child; falls back to the node's
        ;; raw text with quotes trimmed when the grammar omits it.
        (let ((frag (cl-find-if (lambda (cc)
                                  (string= (treesit-node-type cc)
                                           "string_fragment"))
                                (treesit-node-children c t))))
          (setq mod (if frag
                        (anvil-treesit-node-text frag)
                      (let ((raw (anvil-treesit-node-text c)))
                        (if (and (> (length raw) 1)
                                 (memq (aref raw 0) '(?\" ?\')))
                            (substring raw 1 -1)
                          raw)))))))
    mod))

(defun anvil-ts--import-type-only-p (node)
  "Return non-nil when NODE is an `import type { ... }' statement.
Detected by an anonymous `type' keyword child of `import_statement'
\\(the grammar does not elevate it to a named node)."
  (cl-some (lambda (c)
             (and (not (treesit-node-check c 'named))
                  (string= (treesit-node-text c t) "type")))
           (treesit-node-children node nil)))

(defun anvil-ts--import-clause (node)
  "Return the direct `import_clause' child of NODE, or nil."
  (cl-find "import_clause"
           (treesit-node-children node t)
           :key #'treesit-node-type
           :test #'string=))

(defun anvil-ts--import-default-of (node)
  "Return the default specifier's identifier as a string, or nil."
  (let ((clause (anvil-ts--import-clause node)))
    (when clause
      (let ((ident (cl-find-if
                    (lambda (c)
                      (string= (treesit-node-type c) "identifier"))
                    (treesit-node-children clause t))))
        (and ident (anvil-treesit-node-text ident))))))

(defun anvil-ts--import-namespace-of (node)
  "Return the namespace-import name (`* as X' → \"X\") or nil."
  (let ((clause (anvil-ts--import-clause node)))
    (when clause
      (let ((ns (cl-find-if
                 (lambda (c)
                   (string= (treesit-node-type c) "namespace_import"))
                 (treesit-node-children clause t))))
        (when ns
          (let ((ident (cl-find-if
                        (lambda (c)
                          (string= (treesit-node-type c) "identifier"))
                        (treesit-node-children ns t))))
            (and ident (anvil-treesit-node-text ident))))))))

(defun anvil-ts--named-imports-node (node)
  "Return the `named_imports' node under NODE's import_clause, or nil."
  (let ((clause (anvil-ts--import-clause node)))
    (when clause
      (cl-find "named_imports"
               (treesit-node-children clause t)
               :key #'treesit-node-type
               :test #'string=))))

(defun anvil-ts--import-specifier-parts (spec-node)
  "Return (:name STR :alias STR-OR-NIL :type-only BOOL) for SPEC-NODE.
SPEC-NODE is an `import_specifier' — e.g. `a', `a as b', `type a',
`type a as b'."
  (let ((type-only nil)
        (idents '())
        saw-as)
    (dolist (c (treesit-node-children spec-node nil))
      (pcase (treesit-node-type c)
        ("type" (setq type-only t))
        ("as"   (setq saw-as t))
        ("identifier"
         (push (anvil-treesit-node-text c) idents))))
    (let* ((idents (nreverse idents))
           (name (car idents))
           (alias (and saw-as (cadr idents))))
      (list :name name :alias alias :type-only (and type-only t)))))

(defun anvil-ts--import-named-of (node)
  "Return the named-import specifier list for NODE.
Each entry: (:name STR :alias STR-OR-NIL :type-only BOOL)."
  (let ((ni (anvil-ts--named-imports-node node)))
    (when ni
      (let (out)
        (dolist (c (treesit-node-children ni t))
          (when (string= (treesit-node-type c) "import_specifier")
            (push (anvil-ts--import-specifier-parts c) out)))
        (nreverse out)))))

(defun anvil-ts--find-import-by-module (root module)
  "Return the first `import ... from MODULE' statement in ROOT, or nil."
  (let (found)
    (dolist (cap (treesit-query-capture
                  root (anvil-ts--query
                        (treesit-node-language root) 'imports)))
      (unless found
        (let ((n (cdr cap)))
          (when (string= (anvil-ts--import-module-of n) module)
            (setq found n)))))
    found))

(defun anvil-ts--render-named-specifier (entry)
  "Render a single `import_specifier' source from ENTRY plist."
  (let ((prefix (if (plist-get entry :type-only) "type " "")))
    (if (plist-get entry :alias)
        (format "%s%s as %s"
                prefix
                (plist-get entry :name)
                (plist-get entry :alias))
      (format "%s%s" prefix (plist-get entry :name)))))

(defun anvil-ts--render-named-clause (entries)
  "Render `{ a, b as c }' from an ENTRIES list of specifier plists."
  (concat "{ "
          (mapconcat #'anvil-ts--render-named-specifier entries ", ")
          " }"))

(defun anvil-ts--render-import-statement (spec)
  "Render a full `import ...' statement from SPEC plist.
SPEC keys: :from, :default, :named (list of plists or strings),
:namespace, :type-only.  :named entries may be plain strings (no
alias) or plists (:name :alias :type-only).  A :namespace and
:named are mutually exclusive — the renderer emits :namespace and
drops :named."
  (let* ((from (plist-get spec :from))
         (default (plist-get spec :default))
         (namespace (plist-get spec :namespace))
         (named (plist-get spec :named))
         (type-only (plist-get spec :type-only))
         (normalized
          (mapcar (lambda (e)
                    (cond
                     ((stringp e) (list :name e :alias nil :type-only nil))
                     ((and (consp e) (not (plistp e)))
                      (list :name (car e) :alias (cdr e) :type-only nil))
                     (t e)))
                  named))
         (pieces '())
         (header (if type-only "import type " "import ")))
    (unless from
      (user-error "anvil-ts: :from required in SPEC"))
    (cond
     ((and (null default) (null namespace) (null normalized))
      ;; Side-effect import.
      (format "import \"%s\";" from))
     (t
      (when default (push default pieces))
      (cond
       (namespace (push (format "* as %s" namespace) pieces))
       (normalized (push (anvil-ts--render-named-clause normalized) pieces)))
      (format "%s%s from \"%s\";"
              header
              (mapconcat #'identity (nreverse pieces) ", ")
              from)))))

(defun anvil-ts--import-insertion-point (root)
  "Return the 1-based point after which new imports should be inserted.
Picks the end of the last existing top-level import statement, or
the start of the first non-import top-level statement."
  (let ((last-import-end nil)
        (first-non-import-start nil)
        (c (treesit-node-child root 0)))
    (while c
      (pcase (treesit-node-type c)
        ("import_statement"
         (setq last-import-end (treesit-node-end c)))
        (_ (unless first-non-import-start
             (setq first-non-import-start (treesit-node-start c)))))
      (setq c (treesit-node-next-sibling c)))
    (or last-import-end
        first-non-import-start
        (point-min))))

(defun anvil-ts--normalize-spec-entries (spec)
  "Return a fresh plist with :named normalized to list of plists.
Every :named string / cons is promoted to a (:name :alias :type-only)
plist so downstream comparisons don't need to handle both shapes."
  (let* ((named (plist-get spec :named))
         (norm (mapcar
                (lambda (e)
                  (cond
                   ((stringp e) (list :name e :alias nil :type-only nil))
                   ((and (consp e) (not (plistp e)))
                    (list :name (car e) :alias (cdr e) :type-only nil))
                   (t e)))
                named))
         (copy (copy-sequence spec)))
    (plist-put copy :named norm)))

(defun anvil-ts--specifier-key (entry)
  "Return a cons key (name . alias) uniquely identifying an ENTRY.
Used to de-duplicate named specifiers when merging."
  (cons (plist-get entry :name)
        (plist-get entry :alias)))

(cl-defun anvil-ts--plan-add-import (lang file spec)
  "Build an edit plan adding SPEC to FILE parsed under LANG.
SPEC keys: :from (required), :default, :named, :type-only.
Merges into an existing `import ... from :from' when type-only
flags match; otherwise appends a new statement at the import
block's tail.  Returns a no-op plan when every requested specifier
is already present and the type-only flag matches."
  (let* ((spec (anvil-ts--normalize-spec-entries spec))
         (module (plist-get spec :from))
         (type-only (and (plist-get spec :type-only) t)))
    (unless module
      (user-error "anvil-ts-add-import: :from required"))
    (anvil-treesit-with-root file lang root
      (let* ((existing (anvil-ts--find-import-by-module root module))
             (ex-type-only (and existing
                                (anvil-ts--import-type-only-p existing))))
        (cond
         ;; No existing — emit a brand-new statement.
         ((null existing)
          (let* ((ins (anvil-ts--import-insertion-point root))
                 (rendered (anvil-ts--render-import-statement spec))
                 (new-text (if (> ins (point-min))
                               (concat "\n" rendered)
                             (concat rendered "\n"))))
            (anvil-treesit-make-plan
             file ins ins new-text
             (format "add-import from \"%s\" (new)" module))))
         ;; Existing with mismatching type-only flag — append new stmt.
         ((not (eq ex-type-only type-only))
          (let* ((ins (treesit-node-end existing))
                 (rendered (anvil-ts--render-import-statement spec))
                 (new-text (concat "\n" rendered)))
            (anvil-treesit-make-plan
             file ins ins new-text
             (format "add-import from \"%s\" (split %s)"
                     module (if type-only "type" "value")))))
         ;; Existing match — merge specifiers in place.
         (t
          (let* ((cur-default (anvil-ts--import-default-of existing))
                 (cur-namespace (anvil-ts--import-namespace-of existing))
                 (cur-named (anvil-ts--import-named-of existing))
                 (want-default (plist-get spec :default))
                 (want-named (plist-get spec :named))
                 (want-namespace (plist-get spec :namespace))
                 (new-default (or cur-default want-default))
                 (current-keys (mapcar #'anvil-ts--specifier-key cur-named))
                 (missing (cl-remove-if
                           (lambda (e)
                             (member (anvil-ts--specifier-key e) current-keys))
                           want-named))
                 (merged (append cur-named missing))
                 (default-differs (and want-default cur-default
                                       (not (string= cur-default want-default))))
                 (namespace-differs (and want-namespace
                                         (not (equal cur-namespace
                                                     want-namespace)))))
            (when default-differs
              (user-error "anvil-ts-add-import: \"%s\" already has default %S, refusing to overwrite with %S"
                          module cur-default want-default))
            (when namespace-differs
              (user-error "anvil-ts-add-import: \"%s\" already has namespace %S, refusing to overwrite with %S"
                          module cur-namespace want-namespace))
            (cond
             ((and (null missing)
                   (or (null want-default)
                       (and cur-default (string= cur-default want-default)))
                   (or (null want-namespace)
                       (and cur-namespace (string= cur-namespace want-namespace))))
              (anvil-treesit-make-noop-plan
               file (format "add-import from \"%s\"" module)))
             (t
              (let* ((rendered
                      (anvil-ts--render-import-statement
                       (list :from module
                             :type-only type-only
                             :default new-default
                             :namespace (or cur-namespace want-namespace)
                             :named merged)))
                     (beg (treesit-node-start existing))
                     (end (treesit-node-end existing)))
                (anvil-treesit-make-plan
                 file beg end rendered
                 (format "add-import from \"%s\" (merge %d name%s)"
                         module (length missing)
                         (if (= 1 (length missing)) "" "s")))))))))))))

(cl-defun anvil-ts--plan-remove-import (lang file spec)
  "Build an edit plan removing SPEC from FILE parsed under LANG.
SPEC keys match `anvil-ts--plan-add-import'.  :named entries are
removed from the existing statement; :default (when non-nil) drops
the default specifier.  When the resulting statement has no
specifiers left, the whole statement is removed (including its
trailing newline).  No-op when SPEC is already absent."
  (let* ((spec (anvil-ts--normalize-spec-entries spec))
         (module (plist-get spec :from))
         (drop-default (plist-get spec :default))
         (drop-named (plist-get spec :named)))
    (unless module
      (user-error "anvil-ts-remove-import: :from required"))
    (anvil-treesit-with-root file lang root
      (let ((existing (anvil-ts--find-import-by-module root module)))
        (if (null existing)
            (anvil-treesit-make-noop-plan
             file (format "remove-import from \"%s\"" module))
          (let* ((cur-type-only (anvil-ts--import-type-only-p existing))
                 (cur-default (anvil-ts--import-default-of existing))
                 (cur-namespace (anvil-ts--import-namespace-of existing))
                 (cur-named (anvil-ts--import-named-of existing))
                 (drop-keys (mapcar #'anvil-ts--specifier-key drop-named))
                 (kept-named (cl-remove-if
                              (lambda (e)
                                (member (anvil-ts--specifier-key e) drop-keys))
                              cur-named))
                 (new-default (if (and drop-default cur-default
                                       (string= drop-default cur-default))
                                  nil
                                cur-default))
                 (changed (or (and drop-default cur-default
                                   (string= drop-default cur-default))
                              (/= (length kept-named) (length cur-named)))))
            (cond
             ((not changed)
              (anvil-treesit-make-noop-plan
               file (format "remove-import from \"%s\"" module)))
             ;; Nothing left — drop whole statement + trailing newline.
             ((and (null new-default)
                   (null cur-namespace)
                   (null kept-named))
              (let* ((beg (treesit-node-start existing))
                     (end (treesit-node-end existing))
                     (end+1 (with-temp-buffer
                              (insert-file-contents file)
                              (goto-char end)
                              (if (eq (char-after) ?\n) (1+ end) end))))
                (anvil-treesit-make-plan
                 file beg end+1 ""
                 (format "remove-import from \"%s\" (drop whole)" module))))
             (t
              (let* ((rendered
                      (anvil-ts--render-import-statement
                       (list :from module
                             :type-only cur-type-only
                             :default new-default
                             :namespace cur-namespace
                             :named kept-named)))
                     (beg (treesit-node-start existing))
                     (end (treesit-node-end existing)))
                (anvil-treesit-make-plan
                 file beg end rendered
                 (format "remove-import from \"%s\"" module)))))))))))

(defun anvil-ts--bound-name-of-specifier (spec)
  "Return the binding name an import specifier SPEC brings into scope.
An alias wins over the raw name — `import { a as b }' binds `b'."
  (or (plist-get spec :alias) (plist-get spec :name)))

(cl-defun anvil-ts--plan-rename-import (lang file old new)
  "Build an edit plan renaming the OLD binding to NEW in FILE.
Walks every `import_statement', finds the unique specifier whose
visible binding name is OLD (default / named / named-alias /
namespace), and rewrites only that specifier.  Signals a
`user-error' when:
 - OLD is absent from any import.
 - OLD matches multiple specifiers across imports (ambiguous).

Reference use sites are not touched — that is Phase 3 territory."
  (unless (and (stringp old) (stringp new) (not (string-empty-p old))
               (not (string-empty-p new)))
    (user-error "anvil-ts-rename-import: OLD and NEW must be non-empty strings"))
  (anvil-treesit-with-root file lang root
    (let* ((q (anvil-ts--query lang 'imports))
           (matches '()))
      (dolist (cap (treesit-query-capture root q))
        (let* ((node (cdr cap))
               (default (anvil-ts--import-default-of node))
               (namespace (anvil-ts--import-namespace-of node))
               (named (anvil-ts--import-named-of node)))
          (when (and default (string= default old))
            (push (list :node node :kind 'default) matches))
          (when (and namespace (string= namespace old))
            (push (list :node node :kind 'namespace) matches))
          (dolist (s named)
            (when (string= (anvil-ts--bound-name-of-specifier s) old)
              (push (list :node node :kind 'named :spec s) matches)))))
      (cond
       ((null matches)
        (user-error "anvil-ts-rename-import: `%s' is not bound by any import in %s"
                    old (file-name-nondirectory file)))
       ((> (length matches) 1)
        (user-error "anvil-ts-rename-import: `%s' matches %d specifiers (ambiguous)"
                    old (length matches)))
       (t
        (let* ((m (car matches))
               (node (plist-get m :node))
               (module (anvil-ts--import-module-of node))
               (cur-type-only (anvil-ts--import-type-only-p node))
               (cur-default (anvil-ts--import-default-of node))
               (cur-namespace (anvil-ts--import-namespace-of node))
               (cur-named (anvil-ts--import-named-of node))
               (new-default cur-default)
               (new-namespace cur-namespace)
               (new-named cur-named))
          (pcase (plist-get m :kind)
            ('default (setq new-default new))
            ('namespace (setq new-namespace new))
            ('named
             (let ((hit (plist-get m :spec)))
               (setq new-named
                     (mapcar
                      (lambda (e)
                        (if (and (equal (plist-get e :name) (plist-get hit :name))
                                 (equal (plist-get e :alias) (plist-get hit :alias)))
                            ;; Attach an alias if the name itself was the
                            ;; binding (no existing alias); otherwise just
                            ;; rewrite the alias.
                            (let ((had-alias (plist-get e :alias)))
                              (if had-alias
                                  (list :name (plist-get e :name)
                                        :alias new
                                        :type-only (plist-get e :type-only))
                                (list :name (plist-get e :name)
                                      :alias (and (not (string= (plist-get e :name) new))
                                                  new)
                                      :type-only (plist-get e :type-only))))
                          e))
                      cur-named)))))
          (let* ((rendered
                  (anvil-ts--render-import-statement
                   (list :from module
                         :type-only cur-type-only
                         :default new-default
                         :namespace new-namespace
                         :named new-named)))
                 (beg (treesit-node-start node))
                 (end (treesit-node-end node))
                 (current (treesit-node-text node)))
            (if (string= current rendered)
                (anvil-treesit-make-noop-plan
                 file (format "rename-import %s → %s" old new))
              (anvil-treesit-make-plan
               file beg end rendered
               (format "rename-import %s → %s" old new))))))))))

;;;; --- replace-function helpers (Phase 2) ---------------------------------

(defun anvil-ts--line-beginning-at (point)
  "Return the 1-based point of the line start containing POINT."
  (save-excursion (goto-char point) (line-beginning-position)))

(defun anvil-ts--common-leading-whitespace (lines)
  "Longest whitespace prefix common to every non-empty LINES entry."
  (let ((candidates (cl-remove-if
                     (lambda (l) (string-empty-p (string-trim l)))
                     lines)))
    (if (null candidates)
        ""
      (cl-reduce
       (lambda (a b)
         (let ((i 0) (len (min (length a) (length b))))
           (while (and (< i len) (eq (aref a i) (aref b i)))
             (cl-incf i))
           (substring a 0 i)))
       (mapcar (lambda (l)
                 (if (string-match "\\`[ \t]*" l)
                     (match-string 0 l)
                   ""))
               candidates)))))

(defun anvil-ts--dedent (text)
  "Strip common leading whitespace from every line of TEXT."
  (let* ((lines (split-string text "\n"))
         (common (anvil-ts--common-leading-whitespace lines)))
    (if (string-empty-p common)
        text
      (mapconcat (lambda (l)
                   (if (string-prefix-p common l)
                       (substring l (length common))
                     l))
                 lines "\n"))))

(defun anvil-ts--reindent (text indent)
  "Prepend INDENT to every non-empty line of TEXT."
  (if (string-empty-p indent)
      text
    (mapconcat (lambda (l)
                 (if (string-empty-p (string-trim l))
                     l
                   (concat indent l)))
               (split-string text "\n")
               "\n")))

(defun anvil-ts--reindent-except-first (text indent)
  "Like `anvil-ts--reindent' but leave the first line untouched.
Used by `replace-function' where the first line sits after an
existing prefix (`export' / `async' / access modifier) and should
not be re-indented; every subsequent line of TEXT gets INDENT
prepended so the swapped block keeps the original column."
  (if (string-empty-p indent)
      text
    (let* ((lines (split-string text "\n"))
           (idx -1))
      (mapconcat
       (lambda (l)
         (cl-incf idx)
         (cond
          ((= idx 0) l)
          ((string-empty-p (string-trim l)) l)
          (t (concat indent l))))
       lines "\n"))))

(defun anvil-ts--find-replace-target (lang root name class-filter)
  "Return the (function_declaration | method_definition) for NAME.
CLASS-FILTER restricts to methods of a named class.  Signals a
`user-error' on ambiguity (without CLASS-FILTER) or miss."
  (let (candidates)
    (dolist (cap (treesit-query-capture root (anvil-ts--query lang 'functions)))
      (when (eq (car cap) 'fn)
        (let ((n (cdr cap)))
          (when (string= (anvil-ts--node-name n) name)
            (push (cons nil n) candidates)))))
    (dolist (cap (treesit-query-capture root (anvil-ts--query lang 'methods)))
      (when (eq (car cap) 'method)
        (let* ((n (cdr cap))
               (cls (anvil-ts--enclosing-class-name n)))
          (when (and cls (string= (anvil-ts--node-name n) name)
                     (or (null class-filter)
                         (string= cls class-filter)))
            (push (cons cls n) candidates)))))
    (cond
     ((null candidates)
      (user-error "anvil-ts-replace-function: no def named %S%s"
                  name
                  (if class-filter (format " in class %S" class-filter) "")))
     ((and (null class-filter) (> (length candidates) 1))
      (user-error "anvil-ts-replace-function: ambiguous %S (matches %s); pass :class"
                  name
                  (mapconcat (lambda (c)
                               (format "%s" (or (car c) "<top-level>")))
                             candidates ", ")))
     (t (cdr (car candidates))))))

(defun anvil-ts--leading-whitespace-at (point)
  "Return the leading whitespace string of the line containing POINT.
Used by `replace-function' as the re-indent prefix so body lines
of the replacement match the enclosing context.  Counting the
whitespace from line-beg — rather than the width from line-beg to
fn-start — means a top-level `export function foo' gets indent=\"\"
while a method `  greet()' gets indent=\"  \", both correct."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (let ((start (point)))
      (skip-chars-forward " \t")
      (buffer-substring-no-properties start (point)))))

(cl-defun anvil-ts--plan-replace-function (lang file name new-source class-filter)
  "Build an edit plan replacing def NAME with NEW-SOURCE in FILE.
NEW-SOURCE is column-0 source; it is dedented and reindented to
match the existing def's column so the same input works for
top-level functions and for methods.  Any prefix on the def line
\(`export', `async', `static', access modifiers, decorator
pragma `@deco') is preserved — the swap covers only the
function_declaration / method_definition node itself.  Double-
apply with identical NEW-SOURCE is a no-op."
  (anvil-treesit-with-root file lang root
    (let* ((fn (anvil-ts--find-replace-target lang root name class-filter))
           (fn-start (treesit-node-start fn))
           (fn-end (treesit-node-end fn))
           (indent (anvil-ts--leading-whitespace-at fn-start))
           (dedented (anvil-ts--dedent new-source))
           (replacement (anvil-ts--reindent-except-first dedented indent))
           (current (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-substring-no-properties fn-start fn-end)))
           (reason (format "replace-function %s%s" name
                           (if class-filter
                               (format " (class %s)" class-filter)
                             ""))))
      (if (string= current replacement)
          (anvil-treesit-make-noop-plan file reason)
        (anvil-treesit-make-plan file fn-start fn-end replacement reason)))))

;;;; --- wrap-expr helpers (Phase 2) ----------------------------------------

(defconst anvil-ts-wrap-placeholder "|anvil-hole|"
  "Token replaced with the wrapped text inside a `ts-wrap-expr' wrapper.")

(defun anvil-ts--count-substring (needle haystack)
  "Return non-overlapping occurrences of NEEDLE in HAYSTACK."
  (if (string-empty-p needle)
      0
    (let ((i 0) (n 0) (len (length needle)))
      (while (and (< i (length haystack))
                  (setq i (string-match-p (regexp-quote needle) haystack i)))
        (cl-incf n)
        (setq i (+ i len)))
      n)))

(defun anvil-ts--validate-wrapper (wrapper)
  "Signal a user-error unless WRAPPER has exactly one placeholder."
  (let ((n (anvil-ts--count-substring anvil-ts-wrap-placeholder wrapper)))
    (cond
     ((= n 0)
      (user-error "anvil-ts-wrap-expr: wrapper missing `%s' placeholder"
                  anvil-ts-wrap-placeholder))
     ((> n 1)
      (user-error
       "anvil-ts-wrap-expr: wrapper has %d `%s' placeholders (want 1)"
       n anvil-ts-wrap-placeholder)))
    wrapper))

(defun anvil-ts--node-at-exact-range (lang start end)
  "Return the tree-sitter node whose range is exactly [START, END)."
  (let ((n (treesit-node-on start end lang)))
    (catch 'found
      (while n
        (when (and (= (treesit-node-start n) start)
                   (= (treesit-node-end n) end))
          (throw 'found n))
        (setq n (treesit-node-parent n)))
      nil)))

(cl-defun anvil-ts--plan-wrap-expr (lang file start end wrapper)
  "Build an edit plan wrapping [START, END) of FILE with WRAPPER.
Requires the range to align with a tree-sitter node; misaligned
ranges signal a `user-error' rather than produce invalid source."
  (anvil-ts--validate-wrapper wrapper)
  (unless (and (integerp start) (integerp end) (< start end))
    (user-error "anvil-ts-wrap-expr: invalid range (%S . %S)" start end))
  (anvil-treesit-with-root file lang _root
    (let* ((node (anvil-ts--node-at-exact-range lang start end))
           (original (buffer-substring-no-properties start end))
           (replacement (replace-regexp-in-string
                         (regexp-quote anvil-ts-wrap-placeholder)
                         original wrapper t t))
           (reason (format "wrap-expr [%d,%d)%s" start end
                           (if node
                               (format " (%s)" (treesit-node-type node))
                             " (unaligned!)"))))
      (unless node
        (user-error
         "anvil-ts-wrap-expr: range %d-%d does not align with a tree-sitter node"
         start end))
      (if (string= original replacement)
          (anvil-treesit-make-noop-plan file reason)
        (anvil-treesit-make-plan file start end replacement reason)))))

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

;;;; --- public edit API (Phase 2) ------------------------------------------

(cl-defun anvil-ts-add-import (file spec &key apply)
  "Add the import specified by SPEC to FILE.
SPEC keys: :from MODULE (required), :default NAME, :named LIST,
:type-only BOOL.  :named entries are either a plain NAME string,
a (NAME . ALIAS) cons, or a (:name :alias :type-only) plist.

Idempotent: re-adding the same specifiers is a no-op.  Merges
into an existing `import ... from MODULE' whose type-only flag
matches; splits into a second statement when the flag differs.

Returns the edit plan unless APPLY is truthy."
  (let ((plan (anvil-ts--plan-add-import
               (anvil-ts--lang-for-file file) file spec)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-ts-remove-import (file spec &key apply)
  "Remove the import specified by SPEC from FILE.
SPEC keys match `anvil-ts-add-import'.  :named entries are
removed from the existing statement; :default (when non-nil and
matching the current default) drops the default clause.  When
nothing remains, the statement is deleted entirely (including its
trailing newline).  No-op when SPEC is already absent.

Returns the edit plan unless APPLY is truthy."
  (let ((plan (anvil-ts--plan-remove-import
               (anvil-ts--lang-for-file file) file spec)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-ts-rename-import (file old new &key apply)
  "Rename the binding OLD to NEW across imports in FILE.
Walks every `import_statement' and rewrites the unique specifier
whose visible binding name is OLD — default, namespace, or named
(alias wins over raw name).  Signals a `user-error' when OLD is
absent or matches multiple specifiers.  Reference use sites are
not touched (Phase 3 territory).

Returns the edit plan unless APPLY is truthy."
  (let ((plan (anvil-ts--plan-rename-import
               (anvil-ts--lang-for-file file) file old new)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-ts-replace-function (file name new-source &key class apply)
  "Replace the def named NAME in FILE with NEW-SOURCE.
NEW-SOURCE is the full `function foo() { ... }' / `methodName() {
... }' source at column 0; it is dedented and reindented to match
the existing def's column so the same input works for top-level
functions and for methods.

CLASS (keyword) disambiguates when multiple methods share a name.
Without CLASS, an ambiguous NAME errors with the candidate list.

Double-apply with identical NEW-SOURCE is a no-op.  Returns the
edit plan unless APPLY is truthy."
  (let ((plan (anvil-ts--plan-replace-function
               (anvil-ts--lang-for-file file)
               file name new-source
               (and class (if (symbolp class) (symbol-name class) class)))))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-ts-wrap-expr (file start end wrapper &key apply)
  "Wrap the expression [START, END) of FILE with WRAPPER.
WRAPPER is a source template containing `anvil-ts-wrap-placeholder'
\\(the literal string \"|anvil-hole|\") exactly once — that token
is replaced with the original expression text.  The range must
align with a tree-sitter node or the tool errors rather than
produce invalid source.  Returns the edit plan unless APPLY is
truthy."
  (let ((plan (anvil-ts--plan-wrap-expr
               (anvil-ts--lang-for-file file) file start end wrapper)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

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

(defun anvil-ts--coerce-spec (spec)
  "Normalize an MCP-bridge SPEC into the elisp plist shape.
MCP tool calls arrive with string-encoded values and hash-table
shapes; this helper promotes them to a plist with :named entries
expanded into plain plists."
  (let* ((s (cond ((and spec (listp spec)) (copy-sequence spec))
                  ((hash-table-p spec)
                   (let (out)
                     (maphash (lambda (k v)
                                (push v out)
                                (push (intern
                                       (concat ":"
                                               (if (keywordp k)
                                                   (substring
                                                    (symbol-name k) 1)
                                                 (format "%s" k))))
                                      out))
                              spec)
                     out))
                  (t nil)))
         (named (plist-get s :named)))
    (when (stringp named)
      (setq s (plist-put s :named
                         (mapcar #'string-trim
                                 (split-string named "[ ,]+" t)))))
    s))

(defun anvil-ts--tool-add-import (file spec apply)
  "MCP wrapper — add an import.

MCP Parameters:
  file  - absolute path to the .ts or .tsx file to edit
  spec  - plist (:from MODULE :default NAME :named LIST :type-only BOOL)
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-ts-add-import file (anvil-ts--coerce-spec spec) :apply apply)))

(defun anvil-ts--tool-remove-import (file spec apply)
  "MCP wrapper — remove an import.

MCP Parameters:
  file  - absolute path to the .ts or .tsx file to edit
  spec  - plist (:from MODULE :default NAME :named LIST)
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-ts-remove-import file (anvil-ts--coerce-spec spec) :apply apply)))

(defun anvil-ts--tool-rename-import (file old new apply)
  "MCP wrapper — rename an imported binding alias.

MCP Parameters:
  file  - absolute path to the .ts or .tsx file to edit
  old   - current binding name (alias if present, raw name otherwise)
  new   - new binding name
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-ts-rename-import file old new :apply apply)))

(defun anvil-ts--tool-replace-function (file name new-source class apply)
  "MCP wrapper — replace a TS / TSX function or method body.

MCP Parameters:
  file       - absolute path to the .ts or .tsx file to edit
  name       - identifier of the function / method to replace
  new-source - full replacement source (e.g. \\\"function foo() { ... }\\\")
  class      - optional enclosing class name; empty / nil means any class
  apply      - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (let ((cls (cond ((null class) nil)
                    ((and (stringp class) (string-empty-p class)) nil)
                    (t class))))
     (anvil-ts-replace-function file name new-source
                                :class cls :apply apply))))

(defun anvil-ts--tool-wrap-expr (file start end wrapper apply)
  "MCP wrapper — wrap a TS / TSX expression.

MCP Parameters:
  file    - absolute path to the .ts or .tsx file to edit
  start   - 1-based buffer point at the start of the expression
  end     - 1-based buffer point at the end of the expression
  wrapper - source template containing `|anvil-hole|' exactly once
  apply   - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (let ((s (if (stringp start) (string-to-number start) start))
         (e (if (stringp end) (string-to-number end) end)))
     (anvil-ts-wrap-expr file s e wrapper :apply apply))))

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
    "ts-surrounding-form"
    "ts-add-import"
    "ts-remove-import"
    "ts-rename-import"
    "ts-replace-function"
    "ts-wrap-expr")
  "Stable list of MCP tool ids registered by `anvil-ts-enable'.")

(defun anvil-ts--register-tools ()
  "Register the Phase 1b + 2 ts-* MCP tools on `anvil-ts--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-imports)
   :id "ts-list-imports"
   :intent '(ts-read structure)
   :layer 'core
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
   :intent '(ts-read structure)
   :layer 'core
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
   :intent '(ts-read structure)
   :layer 'core
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
   :intent '(ts-read structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "List class declarations in a TypeScript or TSX file.
Returns (:kind 'class :name :bounds) per entry."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-methods)
   :id "ts-list-methods"
   :intent '(ts-read structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "List methods defined directly inside the class named
CLASS-NAME.  Nested classes' methods are excluded; only the first-
level body of a class with that exact name matches.  Returns
\(:kind 'method :class-name :name :async :static :bounds)."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-interfaces)
   :id "ts-list-interfaces"
   :intent '(ts-read structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "List TypeScript `interface' declarations in a .ts /
.tsx file.  Returns (:kind 'interface :name :bounds) per entry.
JavaScript files have no interfaces --- use `js-list-*' for .js /
.jsx."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-list-type-aliases)
   :id "ts-list-type-aliases"
   :intent '(ts-read structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "List TypeScript `type' alias declarations in a .ts /
.tsx file.  Returns (:kind 'type :name :bounds) per entry."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-find-definition)
   :id "ts-find-definition"
   :intent '(ts-read)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "Find the first function / class / interface / type
named NAME in a TypeScript or TSX file.  Source-order wins when the
name is shared across kinds; use the per-kind listers for exhaustive
results."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-surrounding-form)
   :id "ts-surrounding-form"
   :intent '(ts-read structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "Return the innermost function / class / interface /
type whose source range contains the 1-based buffer POINT.  KIND
restricts the match (`function' / `arrow' / `class' / `interface' /
`type'); empty / nil matches any."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-add-import)
   :id "ts-add-import"
   :intent '(ts-edit structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "Add an import to a TypeScript / TSX file.  SPEC is
a plist: (:from MODULE :default NAME :named LIST :type-only BOOL).
:named entries may be plain NAME strings, (NAME . ALIAS) cons
cells, or (:name :alias :type-only) plists.  Idempotent — merges
into an existing `import ... from MODULE' whose type-only flag
matches; splits into a separate statement when the flag differs.
Preview-default; pass :apply t to write.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-remove-import)
   :id "ts-remove-import"
   :intent '(ts-edit structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "Remove an import from a TypeScript / TSX file.
SPEC shape matches `ts-add-import'.  :named entries are removed
from the existing statement; :default drops the default specifier
when it matches.  When nothing remains the statement is deleted
including its trailing newline.  Idempotent.  Preview-default.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-rename-import)
   :id "ts-rename-import"
   :intent '(ts-edit structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "Rename the binding OLD to NEW across TypeScript /
TSX imports.  Walks every `import_statement' and rewrites the
unique specifier (default / named / named-alias / namespace)
whose visible binding is OLD.  Errors on ambiguity or absence.
Reference use sites in the code body are not touched — Phase 3
territory.  Preview-default.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-replace-function)
   :id "ts-replace-function"
   :intent '(ts-edit structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "Replace a TypeScript / TSX function or method
body with NEW-SOURCE.  NEW-SOURCE is the full def at column 0; it
is auto-dedented and reindented to the existing def's column, so
the same input works for top-level functions and for methods.
CLASS disambiguates methods with the same name across classes.
Double-apply with identical NEW-SOURCE is a no-op.  Preview-default.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-ts--tool-wrap-expr)
   :id "ts-wrap-expr"
   :intent '(ts-edit structure)
   :layer 'core
   :server-id anvil-ts--server-id
   :description "Wrap the expression at [START, END) with WRAPPER.
WRAPPER is a source template containing `|anvil-hole|' exactly
once — that token is replaced with the original expression text
from the file.  The range must align to an exact tree-sitter node
boundary, or the tool errors rather than produce invalid source.
Preview-default."))

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

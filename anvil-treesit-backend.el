;;; anvil-treesit-backend.el --- Backend abstraction for anvil-ts/js/py  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Doc 38 Phase F backend abstraction (architecture α pattern).
;;
;; Background: anvil-ts / anvil-js / anvil-py expose MCP tools that
;; structurally analyse TS / JS / Python source.  They were previously
;; thin wrappers over `treesit-*' (= the Emacs builtin tree-sitter
;; binding); that meant they only ran on Emacs, not on NeLisp
;; standalone, and Doc 38 Phase D / E left them with a soft-require
;; on the now-extracted `anvil-ide-treesit.el'.
;;
;; Phase F restores them to first-class citizens of anvil.el by
;; introducing a backend dispatch:
;;
;;     anvil-ts.el / anvil-js.el / anvil-py.el
;;       └─ public API (= MCP tools, unchanged)
;;            └─ internal: anvil-treesit-backend-* helpers
;;                 ├─ if treesit-parser-create available → 'treesit
;;                 │    (today's path on Emacs)
;;                 └─ else                              → 'subprocess
;;                      (PHASE-G-SUBPROCESS-BACKEND-CANDIDATE — stub)
;;
;; The treesit backend re-exports the legacy `anvil-treesit-*' API
;; (parse / query / node-text / node-range / node-bounds / with-root /
;; compile-query / make-plan / apply-plan / truthy) one-for-one with
;; the same shape and semantics as `anvil-ide-treesit.el', so calling
;; sites in anvil-ts/js/py do not need to change.
;;
;; The subprocess backend is intentionally a stub: `parse' / `query'
;; signal a structured `:not-implemented-yet' user-error pointing at
;; Phase G (= shell out to tsc / acorn / python -c "import ast").
;; This keeps the public API loadable on NeLisp; calling a tool there
;; yields an informative message rather than a `void-function' crash.
;;
;; Backend interface (= Phase G implementors target this):
;;
;;   (anvil-treesit-backend-pick LANG)              → 'treesit / 'subprocess / nil
;;   (anvil-treesit-backend-parse SOURCE LANG)      → AST (backend-shaped)
;;   (anvil-treesit-backend-query AST SELECTOR)     → list of nodes
;;   (anvil-treesit-backend-node-text NODE SOURCE)  → string
;;   (anvil-treesit-backend-node-range NODE)        → (cons START END)
;;
;; The legacy `anvil-treesit-*' helpers (with-root / node-bounds /
;; make-plan / apply-plan / truthy / compile-query) sit on top of the
;; interface and are what the language modules call.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
;; treesit is the Emacs 29+ builtin; on NeLisp standalone it does not
;; exist.  Soft-require keeps load clean; the backend dispatcher will
;; return 'subprocess (= stub) instead.
(require 'treesit nil 'noerror)

(defgroup anvil-treesit nil
  "Tree-sitter backend abstraction for anvil-ts/js/py (Doc 38 Phase F)."
  :group 'anvil
  :prefix "anvil-treesit-")

(defcustom anvil-treesit-language-source-alist
  '((python     "https://github.com/tree-sitter/tree-sitter-python" "v0.21.0")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2"
                "typescript/src")
    (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2"
                "tsx/src")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.4"))
  "Grammar source spec merged into `treesit-language-source-alist' on demand.
Each entry is a list accepted by `treesit-install-language-grammar'
\(LANG URL REVISION SOURCE-DIR CC LIBCXX).  Pinned to revisions
compatible with tree-sitter runtime ABI 14 (Emacs 30 / libtree-sitter
0.22)."
  :type '(alist :key-type symbol
                :value-type (repeat string))
  :group 'anvil-treesit)

(defcustom anvil-treesit-backend-preferred 'auto
  "Backend selection policy.
`auto'       — pick `treesit' when the grammar is loadable, else
               `subprocess' (= stubbed pending Phase G).
`treesit'    — force the in-process tree-sitter backend; signals
               `grammar-missing' when the grammar is not installed.
`subprocess' — force the subprocess backend (= today: stub returning
               `:not-implemented-yet').  Useful for testing the
               dispatch wiring."
  :type '(choice (const auto)
                 (const treesit)
                 (const subprocess))
  :group 'anvil-treesit)

(defvar anvil-treesit--query-cache (make-hash-table :test 'equal)
  "Cache of compiled tree-sitter queries, keyed by (LANG . OP-SYMBOL).
Populated by `anvil-treesit-compile-query', emptied by
`anvil-treesit-clear-query-cache'.")

;;;; --- file → language dispatch -------------------------------------------

(defconst anvil-treesit--extension-alist
  '((python     . ("py" "pyi"))
    (typescript . ("ts"))
    (tsx        . ("tsx"))
    (javascript . ("js" "jsx" "mjs" "cjs")))
  "Mapping from tree-sitter language symbol to file extensions.")

(defun anvil-treesit-language-for-file (file)
  "Return the language symbol for FILE by extension, or nil if unknown."
  (let ((ext (and file (file-name-extension file))))
    (and ext
         (car (cl-find-if (lambda (entry)
                            (member ext (cdr entry)))
                          anvil-treesit--extension-alist)))))

;;;; --- backend dispatch ---------------------------------------------------

(defun anvil-treesit-backend--treesit-available-p (lang)
  "Return non-nil when the treesit backend can serve LANG."
  (and (fboundp 'treesit-parser-create)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p lang)))

(defun anvil-treesit-backend-pick (lang)
  "Return the active backend symbol for LANG.
One of `treesit' / `subprocess' / nil.  `nil' means no backend can
serve LANG at all; callers should signal an informative error.
Honours `anvil-treesit-backend-preferred'."
  (pcase anvil-treesit-backend-preferred
    ('treesit
     (and (anvil-treesit-backend--treesit-available-p lang) 'treesit))
    ('subprocess 'subprocess)
    (_
     (cond
      ((anvil-treesit-backend--treesit-available-p lang) 'treesit)
      ;; Subprocess backend is a stub for now (Phase G); we still
      ;; return its symbol so the dispatcher path is exercised end-
      ;; to-end and the caller surfaces the structured stub error.
      (t 'subprocess)))))

;;;; --- grammar availability (treesit backend) -----------------------------

(defun anvil-treesit--install-hint (lang)
  "Return a one-line `M-x' hint for installing LANG."
  (format "M-x treesit-install-language-grammar RET %s RET" lang))

(defun anvil-treesit--source-url (lang)
  "Return the upstream grammar URL for LANG from our pinned source alist."
  (let ((entry (assq lang anvil-treesit-language-source-alist)))
    (and entry (nth 1 entry))))

(defun anvil-treesit-grammar-missing-error (lang)
  "Return a structured `grammar-missing' plist for LANG.
Handlers return this shape as a `user-error' so Claude can relay
the install hint to the user without exposing a backtrace."
  (list :kind 'grammar-missing
        :lang lang
        :install-hint (anvil-treesit--install-hint lang)
        :source-url (anvil-treesit--source-url lang)))

(defun anvil-treesit-ensure-grammar (lang)
  "Ensure a tree-sitter grammar for LANG is loadable on the treesit backend.
Returns t on success.  When the treesit backend itself is unavailable
\(= NeLisp standalone, no `treesit-parser-create'), signals a
structured `:not-implemented-yet' error pointing at Phase G.  When
the backend is available but the grammar is missing, signals the
`grammar-missing' shape used by Doc 21 Phase 1b for Claude relay."
  (cond
   ;; PHASE-G-SUBPROCESS-BACKEND-CANDIDATE: when treesit isn't even
   ;; loaded, dispatch to the subprocess backend stub for a
   ;; consistent "not implemented yet" message.
   ((not (fboundp 'treesit-language-available-p))
    (anvil-treesit-backend-subprocess-not-implemented lang 'ensure-grammar))
   ((treesit-language-available-p lang) t)
   (t
    (let ((entry (assq lang anvil-treesit-language-source-alist)))
      (when (and entry (boundp 'treesit-language-source-alist))
        (setf (alist-get lang treesit-language-source-alist nil nil #'eq)
              (cdr entry))))
    (user-error "%S" (anvil-treesit-grammar-missing-error lang)))))

;;;; --- query cache --------------------------------------------------------

(defun anvil-treesit-compile-query (lang op source)
  "Compile and cache a tree-sitter query for (LANG . OP).
SOURCE is the query source string.  Returns the compiled query
object; a hit in `anvil-treesit--query-cache' returns immediately.
Signals `:not-implemented-yet' on the subprocess backend (Phase G)."
  (unless (fboundp 'treesit-query-compile)
    (anvil-treesit-backend-subprocess-not-implemented lang 'compile-query))
  (let ((key (cons lang op)))
    (or (gethash key anvil-treesit--query-cache)
        (let ((compiled (treesit-query-compile lang source)))
          (puthash key compiled anvil-treesit--query-cache)
          compiled))))

(defun anvil-treesit-clear-query-cache ()
  "Empty the compiled-query cache.
Call after editing query source in an interactive session."
  (clrhash anvil-treesit--query-cache))

;;;; --- subprocess backend stub --------------------------------------------

(defun anvil-treesit-backend-subprocess-not-implemented (lang op)
  "Signal a structured `:not-implemented-yet' user-error for LANG / OP.
Used by every public entry point on the subprocess backend until
Phase G ships real subprocess implementations (= shell out to tsc /
acorn / python -c \"import ast\")."
  ;; PHASE-G-SUBPROCESS-BACKEND-CANDIDATE
  (user-error
   "%S"
   (list :kind 'not-implemented-yet
         :lang lang
         :op op
         :backend 'subprocess
         :hint (format
                "anvil-treesit-backend: subprocess backend for %S is a Doc 38 Phase G TODO; \
install Emacs treesit grammar (M-x treesit-install-language-grammar RET %s RET) \
or wait for the subprocess backend to ship"
                lang lang))))

;; Subprocess parse / query / node-text / node-range stubs.  Real
;; implementations would shell out to:
;;   typescript / tsx → tsc --noEmit + tree-sitter CLI / ts-morph
;;   javascript       → acorn / esbuild --analyze
;;   python           → python -c "import ast; ..."
;;
;; The stub layer below mirrors the documented interface so the
;; Phase G implementor swaps the body without changing the call
;; sites.

(defun anvil-treesit-backend-subprocess-parse (_source lang)
  "PHASE-G-SUBPROCESS-BACKEND-CANDIDATE: parse SOURCE to AST via subprocess."
  (anvil-treesit-backend-subprocess-not-implemented lang 'parse))

(defun anvil-treesit-backend-subprocess-query (_ast _selector)
  "PHASE-G-SUBPROCESS-BACKEND-CANDIDATE: query AST via subprocess."
  (anvil-treesit-backend-subprocess-not-implemented 'unknown 'query))

(defun anvil-treesit-backend-subprocess-node-text (_node _source)
  "PHASE-G-SUBPROCESS-BACKEND-CANDIDATE: node text via subprocess."
  (anvil-treesit-backend-subprocess-not-implemented 'unknown 'node-text))

(defun anvil-treesit-backend-subprocess-node-range (_node)
  "PHASE-G-SUBPROCESS-BACKEND-CANDIDATE: node range via subprocess."
  (anvil-treesit-backend-subprocess-not-implemented 'unknown 'node-range))

;;;; --- treesit backend implementation -------------------------------------

(defun anvil-treesit-backend-treesit-parse (source lang)
  "Parse SOURCE (a string) with the treesit backend for LANG.
Returns the root node.  Internal helper — the documented user-
facing entry is `anvil-treesit-with-root', which handles file IO."
  (anvil-treesit-ensure-grammar lang)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert source))
    (let ((parser (treesit-parser-create lang)))
      (treesit-parser-root-node parser))))

(defun anvil-treesit-backend-treesit-query (root selector)
  "Run SELECTOR (compiled query) against ROOT.  Returns capture alist."
  (treesit-query-capture root selector))

(defun anvil-treesit-backend-treesit-node-text (node &optional _source)
  "Return NODE's text content as a string."
  (treesit-node-text node t))

(defun anvil-treesit-backend-treesit-node-range (node)
  "Return (START . END) for NODE, 1-based buffer positions."
  (cons (treesit-node-start node) (treesit-node-end node)))

;;;; --- backend interface (= dispatch) -------------------------------------

(defun anvil-treesit-backend-parse (source lang)
  "Parse SOURCE for LANG with the active backend.
Dispatches via `anvil-treesit-backend-pick'.  On the subprocess
backend, signals a structured `:not-implemented-yet' error
\(Phase G).  On the treesit backend, returns the root node."
  (pcase (anvil-treesit-backend-pick lang)
    ('treesit (anvil-treesit-backend-treesit-parse source lang))
    ('subprocess (anvil-treesit-backend-subprocess-parse source lang))
    (_ (anvil-treesit-backend-subprocess-not-implemented lang 'parse))))

(defun anvil-treesit-backend-query (ast selector)
  "Run SELECTOR against AST.
Dispatches: assumes the AST shape carries enough info for the
backend; the treesit backend returns `treesit-query-capture' results."
  ;; treesit nodes are opaque; we infer the backend from the node
  ;; predicate.  Subprocess ASTs would be plain plists.
  (cond
   ((and (fboundp 'treesit-node-p) (treesit-node-p ast))
    (anvil-treesit-backend-treesit-query ast selector))
   (t (anvil-treesit-backend-subprocess-query ast selector))))

(defun anvil-treesit-backend-node-text (node &optional source)
  "Return NODE's text.  SOURCE is needed for subprocess nodes only."
  (cond
   ((and (fboundp 'treesit-node-p) (treesit-node-p node))
    (anvil-treesit-backend-treesit-node-text node source))
   (t (anvil-treesit-backend-subprocess-node-text node source))))

(defun anvil-treesit-backend-node-range (node)
  "Return (START . END) for NODE."
  (cond
   ((and (fboundp 'treesit-node-p) (treesit-node-p node))
    (anvil-treesit-backend-treesit-node-range node))
   (t (anvil-treesit-backend-subprocess-node-range node))))

;;;; --- file → parser tree (legacy helpers, treesit backend) ---------------

(defun anvil-treesit--insert-file (file)
  "Insert FILE into the current buffer as UTF-8.
Used inside `with-temp-buffer' from `anvil-treesit-with-root'.
Forces UTF-8 so the tree-sitter input bytes align with the grammar's
UTF-8 expectation regardless of the user's locale."
  (let ((coding-system-for-read 'utf-8-unix))
    (insert-file-contents file)))

(defun anvil-treesit-with-root-fn (file lang fn)
  "Functional form of `anvil-treesit-with-root'.
Opens FILE, creates a parser for LANG via the active backend, and
calls FN with the root node.  Signals a structured `grammar-missing'
or `:not-implemented-yet' `user-error' depending on which backend
is unavailable.  FILE may not be nil."
  (unless (and file (stringp file))
    (user-error "anvil-treesit: FILE must be a string, got %S" file))
  (unless (file-readable-p file)
    (user-error "anvil-treesit: cannot read file %s" file))
  (pcase (anvil-treesit-backend-pick lang)
    ('treesit
     (anvil-treesit-ensure-grammar lang)
     (with-temp-buffer
       (anvil-treesit--insert-file file)
       (let* ((parser (treesit-parser-create lang))
              (root (treesit-parser-root-node parser)))
         (funcall fn root))))
    ('subprocess
     (anvil-treesit-backend-subprocess-not-implemented lang 'with-root))
    (_
     (anvil-treesit-backend-subprocess-not-implemented lang 'with-root))))

(defmacro anvil-treesit-with-root (file lang var &rest body)
  "Bind VAR to the root node of FILE parsed with LANG, then run BODY.
Macro form of `anvil-treesit-with-root-fn'.  The parser and buffer
are torn down when BODY returns."
  (declare (indent 3) (debug (form form symbolp body)))
  `(anvil-treesit-with-root-fn ,file ,lang (lambda (,var) ,@body)))

;;;; --- node helpers (legacy, treesit-shaped) ------------------------------

(defun anvil-treesit-node-range (node)
  "Return (:start POINT :end POINT) for NODE as 1-based point positions."
  (list :start (treesit-node-start node)
        :end (treesit-node-end node)))

(defun anvil-treesit-node-text (node)
  "Return NODE's text content as a string."
  (treesit-node-text node t))

(defun anvil-treesit-node-bounds (node)
  "Return (:start :end :start-line :end-line) for NODE.
Line numbers are 1-based.  Convenience shape for listing operations."
  (let ((s (treesit-node-start node))
        (e (treesit-node-end node)))
    (list :start s
          :end e
          :start-line (line-number-at-pos s)
          :end-line (line-number-at-pos e))))

(defun anvil-treesit-child-by-field (node field-name)
  "Return NODE's child under FIELD-NAME, or nil."
  (treesit-node-child-by-field-name node field-name))

;;;; --- edit-plan primitives ------------------------------------------------

(defun anvil-treesit-truthy (v)
  "Return non-nil when V is a truthy MCP value."
  (not (or (null v)
           (eq v :json-false)
           (eq v :false)
           (and (stringp v)
                (member v '("" "nil" "false" "0" "no" "False" "NIL"))))))

(defun anvil-treesit--unified-diff (file beg end replacement)
  "Return a short unified-diff string for FILE swapping [BEG,END) with REPLACEMENT."
  (let* ((old (with-temp-buffer
                (insert-file-contents file)
                (buffer-substring-no-properties beg end)))
         (base (file-name-nondirectory file))
         (lines-old (split-string old "\n"))
         (lines-new (split-string replacement "\n"))
         (out (list (format "+++ %s (after)" base)
                    (format "--- %s (before)" base))))
    (dolist (l lines-old)
      (push (concat "-" l) out))
    (dolist (l lines-new)
      (push (concat "+" l) out))
    (mapconcat #'identity (nreverse out) "\n")))

(defun anvil-treesit-make-plan (file beg end replacement reason)
  "Build an edit plan for a single file / single range."
  (list :ops (list (list :file file
                         :range (cons beg end)
                         :replacement replacement
                         :reason reason))
        :summary (format "%s: 1 op on %s" reason
                         (file-name-nondirectory file))
        :diff-preview (anvil-treesit--unified-diff file beg end replacement)))

(defun anvil-treesit-make-noop-plan (file reason)
  "Build an empty plan noting that REASON is already satisfied in FILE."
  (list :ops nil
        :summary (format "%s: no-op (already satisfied) on %s"
                         reason (file-name-nondirectory file))
        :diff-preview ""))

(defun anvil-treesit--assert-ops-non-overlapping (ops)
  "Signal an error when any two OPS on the same file overlap."
  (let ((by-file (make-hash-table :test 'equal)))
    (dolist (op ops)
      (push op (gethash (plist-get op :file) by-file)))
    (maphash
     (lambda (file file-ops)
       (let ((sorted (sort (copy-sequence file-ops)
                           (lambda (a b)
                             (< (car (plist-get a :range))
                                (car (plist-get b :range)))))))
         (cl-loop for (a b) on sorted while b do
                  (let ((a-end (cdr (plist-get a :range)))
                        (b-beg (car (plist-get b :range))))
                    (when (> a-end b-beg)
                      (error "anvil-treesit: overlapping ops in %s: %S vs %S"
                             file (plist-get a :range)
                             (plist-get b :range)))))))
     by-file)))

(defun anvil-treesit-apply-plan (plan)
  "Write every op in PLAN to disk.  Return PLAN with :applied-at added."
  (let* ((ops (plist-get plan :ops))
         (by-file (make-hash-table :test 'equal)))
    (when ops
      (anvil-treesit--assert-ops-non-overlapping ops)
      (dolist (op ops)
        (push op (gethash (plist-get op :file) by-file)))
      (maphash
       (lambda (file file-ops)
         (let ((sorted (sort (copy-sequence file-ops)
                             (lambda (a b)
                               (> (car (plist-get a :range))
                                  (car (plist-get b :range)))))))
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8-unix)
                   (coding-system-for-write 'utf-8-unix))
               (insert-file-contents file)
               (dolist (op sorted)
                 (let ((r (plist-get op :range)))
                   (delete-region (car r) (cdr r))
                   (goto-char (car r))
                   (insert (plist-get op :replacement))))
               (write-region (point-min) (point-max) file nil 'silent)))))
       by-file))
    (append plan (list :applied-at (format-time-string "%FT%T%z")))))

;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-treesit-backend-enable ()
  "Enable the anvil-treesit backend layer.
No MCP tools are registered at the backend level — per-language
modules (anvil-py, anvil-ts, anvil-js) own their own surface.
Provided so `anvil--load-module' can discover `treesit-backend' as
a first-class module."
  (interactive)
  t)

(defun anvil-treesit-backend-disable ()
  "Disable the anvil-treesit backend layer.  Clears the query cache."
  (interactive)
  (anvil-treesit-clear-query-cache)
  t)

(provide 'anvil-treesit-backend)
;;; anvil-treesit-backend.el ends here

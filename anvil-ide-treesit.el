;;; anvil-ide-treesit.el --- Tree-sitter shared core for anvil (IDE layer)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Shared substrate for Doc 21 (docs/design/21-treesit-edits.org)
;; language modules: anvil-py, anvil-ts, anvil-js.  Provides:
;;
;; - File → language dispatch by extension.
;; - Grammar availability check + structured `grammar-missing' error
;;   that Claude can relay to the user.
;; - Query-compile cache keyed by (LANG . OP-SYMBOL).
;; - A `with-root' helper that opens a file into a temp buffer, creates
;;   a parser, and runs BODY with the root node bound.
;;
;; No language-specific knowledge lives here — anvil-py / anvil-ts /
;; anvil-js own their own queries and surface API.  The core is
;; intentionally thin so adding a fourth language is ~1 day of work.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'treesit)

(defgroup anvil-treesit nil
  "Tree-sitter shared core for anvil language modules (Doc 21)."
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
\(LANG URL REVISION SOURCE-DIR CC LIBCXX).  The revisions are pinned
to tags compatible with tree-sitter runtime ABI 14 (the version
shipped by Emacs 30 via libtree-sitter 0.22).  Newer grammars tagged
as ABI 15 will silently fail to load; pinning avoids that failure."
  :type '(alist :key-type symbol
                :value-type (repeat string))
  :group 'anvil-treesit)

(defvar anvil-treesit--query-cache (make-hash-table :test 'equal)
  "Cache of compiled tree-sitter queries, keyed by (LANG . OP-SYMBOL).")

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

;;;; --- grammar availability -----------------------------------------------

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
  "Ensure the tree-sitter grammar for LANG is loadable.
Returns t on success.  Signals a `user-error' whose message is the
`prin1' form of `anvil-treesit-grammar-missing-error' when the
grammar is not available — callers reading the error body back as
elisp recover the structured plist for display."
  (unless (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p lang))
    ;; Merge our pinned entry into treesit-language-source-alist so a
    ;; downstream user invoking `treesit-install-language-grammar'
    ;; installs the ABI-compatible revision rather than the current
    ;; master of the grammar repo.
    (let ((entry (assq lang anvil-treesit-language-source-alist)))
      (when (and entry (boundp 'treesit-language-source-alist))
        (setf (alist-get lang treesit-language-source-alist nil nil #'eq)
              (cdr entry))))
    (user-error "%S" (anvil-treesit-grammar-missing-error lang)))
  t)

;;;; --- query cache --------------------------------------------------------

(defun anvil-treesit-compile-query (lang op source)
  "Compile and cache a tree-sitter query for (LANG . OP).
SOURCE is the query source string.  Returns the compiled query
object; a hit in `anvil-treesit--query-cache' returns immediately."
  (let ((key (cons lang op)))
    (or (gethash key anvil-treesit--query-cache)
        (let ((compiled (treesit-query-compile lang source)))
          (puthash key compiled anvil-treesit--query-cache)
          compiled))))

(defun anvil-treesit-clear-query-cache ()
  "Empty the compiled-query cache.
Call after editing query source in an interactive session."
  (clrhash anvil-treesit--query-cache))

;;;; --- file → parser tree -------------------------------------------------

(defun anvil-treesit--insert-file (file)
  "Insert FILE into the current buffer as UTF-8.
Used inside `with-temp-buffer' from `anvil-treesit-with-root'.
Forces UTF-8 so the tree-sitter input bytes align with the grammar's
UTF-8 expectation regardless of the user's locale."
  (let ((coding-system-for-read 'utf-8-unix))
    (insert-file-contents file)))

(defun anvil-treesit-with-root-fn (file lang fn)
  "Functional form of `anvil-treesit-with-root'.
Opens FILE, creates a parser for LANG, and calls FN with the root
node.  Signals a structured `grammar-missing' `user-error' when the
grammar is not installed.  FILE may not be nil."
  (unless (and file (stringp file))
    (user-error "anvil-treesit: FILE must be a string, got %S" file))
  (unless (file-readable-p file)
    (user-error "anvil-treesit: cannot read file %s" file))
  (anvil-treesit-ensure-grammar lang)
  (with-temp-buffer
    (anvil-treesit--insert-file file)
    (let* ((parser (treesit-parser-create lang))
           (root (treesit-parser-root-node parser)))
      (funcall fn root))))

(defmacro anvil-treesit-with-root (file lang var &rest body)
  "Bind VAR to the root node of FILE parsed with LANG, then run BODY.
Macro form of `anvil-treesit-with-root-fn'.  The parser and buffer
are torn down when BODY returns."
  (declare (indent 3) (debug (form form symbolp body)))
  `(anvil-treesit-with-root-fn ,file ,lang (lambda (,var) ,@body)))

;;;; --- node helpers -------------------------------------------------------

(defun anvil-treesit-node-range (node)
  "Return (:start POINT :end POINT) for NODE as 1-based point positions.
Emacs buffer points are 1-based; tree-sitter's underlying byte
offsets come through `treesit-node-start' / `-end' already adjusted
to point coordinates by the Emacs layer, so no +1 shim is needed."
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

;; Phase 2 of per-language tree-sitter editing (Doc 21) uses the same
;; preview-default plan shape as anvil-sexp (Doc 12).  These helpers
;; live here so anvil-py / anvil-ts / anvil-js can all share them —
;; pulling the per-module copies into one audit point.

(defun anvil-treesit-truthy (v)
  "Return non-nil when V is a truthy MCP value.
Accepts elisp nil, :json-false, :false, and the string forms
\"false\" / \"nil\" / \"0\" / \"\" / \"no\" as falsy; anything else
is truthy."
  (not (or (null v)
           (eq v :json-false)
           (eq v :false)
           (and (stringp v)
                (member v '("" "nil" "false" "0" "no" "False" "NIL"))))))

(defun anvil-treesit--unified-diff (file beg end replacement)
  "Return a short unified-diff string for FILE, swapping [BEG,END) with REPLACEMENT."
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
  "Build an edit plan for a single file / single range.
Shape: (:ops (:file :range :replacement :reason) :summary :diff-preview).
An empty BEG == END == REPLACEMENT is legal — callers synthesize
no-op plans via `anvil-treesit-make-noop-plan'."
  (list :ops (list (list :file file
                         :range (cons beg end)
                         :replacement replacement
                         :reason reason))
        :summary (format "%s: 1 op on %s" reason
                         (file-name-nondirectory file))
        :diff-preview (anvil-treesit--unified-diff file beg end replacement)))

(defun anvil-treesit-make-noop-plan (file reason)
  "Build an empty plan noting that REASON is already satisfied in FILE.
Used by idempotent operations like `py-add-import' when a re-apply
would not change the file."
  (list :ops nil
        :summary (format "%s: no-op (already satisfied) on %s"
                         reason (file-name-nondirectory file))
        :diff-preview ""))

(defun anvil-treesit--assert-ops-non-overlapping (ops)
  "Signal an error when any two OPS on the same file overlap.
Ops that abut exactly (a-end == b-start) are allowed."
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
  "Write every op in PLAN to disk.  Return PLAN with :applied-at added.
Ops on the same file are applied back-to-front so earlier ranges
stay valid after later replacements shift text.  Refuses when any
two ops on the same file overlap.  An empty :ops list returns
PLAN unchanged with :applied-at set — a no-op write."
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

(defun anvil-treesit-enable ()
  "Enable the anvil-treesit shared core.
No MCP tools are registered at the core level — per-language modules
\(anvil-py, anvil-ts, anvil-js) own their own surface.  Provided so
`anvil--load-module' can discover `treesit' as a first-class module."
  (interactive)
  t)

(defun anvil-treesit-disable ()
  "Disable the anvil-treesit shared core.  Clears the query cache."
  (interactive)
  (anvil-treesit-clear-query-cache)
  t)

(provide 'anvil-ide-treesit)
;;; anvil-ide-treesit.el ends here

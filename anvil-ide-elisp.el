;;; anvil-ide-elisp.el --- Emacs IDE-only Elisp tools for anvil -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2025-2026 anvil-ide-elisp.el contributors

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: zawatton
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, development
;; URL: https://github.com/zawatton/anvil.el

;;; Commentary:

;; Doc 38 Phase C — IDE-only spinout from anvil-elisp.el.
;;
;; Hosts the single MCP tool that depends on the Emacs `info-look'
;; subsystem, which is not part of the portable runtime contract
;; (= absent under the standalone NeLisp runtime).  The other six
;; anvil-elisp tools remain in `anvil-elisp.el' and stay portable.
;;
;; Tool contained:
;;   - elisp-info-lookup-symbol
;;
;; Helpers `anvil-elisp--validate-symbol' and
;; `anvil-elisp--json-encode-not-found' are reused from `anvil-elisp.el'.

;;; Code:

(require 'anvil-server)
(require 'anvil-elisp)
(require 'info-look)

;;; Info Documentation Helpers

(defun anvil-ide-elisp--extract-info-node-content ()
  "Extract the complete content of the current Info node.
Assumes we're in an Info buffer at the correct node."
  (let ((start nil)
        (end nil))
    ;; Find the start of actual content (after the node header)
    (goto-char (point-min))
    (when (re-search-forward "^File: [^,]+,  Node: [^,\n]+.*\n" nil t)
      (setq start (point)))

    ;; Find the end of content
    (when start
      (goto-char start)
      ;; Look for the next node boundary or end of buffer
      (if (re-search-forward "^\^_" nil t)
          (setq end (match-beginning 0))
        (setq end (point-max))))

    ;; Extract and clean up the content
    (when (and start end)
      (anvil-ide-elisp--clean-info-content
       (buffer-substring-no-properties start end)))))

(defun anvil-ide-elisp--clean-info-content (content)
  "Clean up Info formatting from CONTENT.
Removes navigation markers while preserving documentation structure."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))

    ;; Remove footnote references like (*note ...)
    (while (re-search-forward "\\*[Nn]ote[ \n][^:]*::" nil t)
      (replace-match "[See: \\&]"))

    ;; Return cleaned content
    (buffer-string)))

(defun anvil-ide-elisp--perform-info-lookup (symbol)
  "Perform the actual Info lookup for SYMBOL.
Returns an alist with lookup results or nil if not found."
  (condition-case nil
      (with-temp-buffer
        ;; Set up for info-lookup
        (let ((mode 'emacs-lisp-mode)
              (info-buf nil)
              (node nil)
              (manual nil)
              (content nil))

          ;; info-lookup-symbol needs a buffer in the right mode
          (emacs-lisp-mode)

          ;; Perform the lookup - this will open an Info buffer
          (save-window-excursion
            (info-lookup-symbol symbol mode)

            ;; Get the Info buffer that was opened
            (setq info-buf (get-buffer "*info*"))

            (when info-buf
              (with-current-buffer info-buf
                ;; Extract node information
                (goto-char (point-min))
                (when (re-search-forward
                       "^File: \\([^,]+\\),  Node: \\([^,\n]+\\)"
                       nil t)
                  (setq manual (match-string 1))
                  (setq node (match-string 2))
                  ;; Remove .info extension if present
                  (when (string-match "\\.info\\'" manual)
                    (setq manual
                          (substring manual 0 (match-beginning 0)))))

                ;; Extract content
                (setq content
                      (anvil-ide-elisp--extract-info-node-content)))))

          ;; Return results if we found something
          (when (and node content)
            `((found . t)
              (symbol . ,symbol)
              (node . ,node)
              (manual . ,manual)
              (content . ,content)
              (info-ref . ,(format "(%s)%s" manual node))))))
    ;; If lookup fails, return nil
    (error
     nil)))

(defun anvil-ide-elisp--info-lookup-symbol (symbol)
  "Look up SYMBOL in Elisp Info documentation.

This tool depends on the Emacs `info-look' subsystem, which is
not part of the portable runtime contract.  When `info-lookup-symbol'
is unavailable (for example under the standalone NeLisp runtime),
the call returns a JSON object with `error: \"info-look-unavailable\"'
instead of signalling, so MCP clients can degrade gracefully.

MCP Parameters:
  symbol - The symbol to look up (string)"
  (anvil-server-with-error-handling
   ;; Validate input first so callers always get a uniform validation error.
   (anvil-elisp--validate-symbol symbol "symbol")
   (if (not (fboundp 'info-lookup-symbol))
       (json-encode
        `((found . :json-false)
          (symbol . ,symbol)
          (error . "info-look-unavailable")
          (message . "info-lookup-symbol is not available in this runtime")))
     ;; Perform lookup
     (let ((result (anvil-ide-elisp--perform-info-lookup symbol)))
       (if result
           (json-encode result)
         (anvil-elisp--json-encode-not-found
          symbol
          (format "Symbol '%s' not found in Elisp Info documentation"
                  symbol)))))))

(defconst anvil-ide-elisp--server-id "emacs-eval"
  "Server ID for this MCP server.
Matches `anvil-elisp--server-id' so the IDE tool joins the same MCP
connection as the portable elisp-* tools.")

;;;###autoload
(defun anvil-ide-elisp-enable ()
  "Register the IDE-only Elisp tools (= info-lookup-symbol).

Call after `anvil-elisp-enable' (= ordering does not strictly matter
because each tool registers under its own id, but this preserves the
historical surface order)."
  (anvil-server-register-tool
   #'anvil-ide-elisp--info-lookup-symbol
   :id "elisp-info-lookup-symbol"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-ide-elisp--server-id
   :description
   "Look up Elisp symbols in Info documentation and return the complete
documentation node. Returns the full content of the Info node containing
the symbol's documentation from the Emacs Lisp Reference Manual.

Parameters:
  symbol - The Elisp symbol to look up (string)

Returns JSON with:
  found - Whether documentation was found (boolean)
  symbol - The symbol that was looked up (string)
  node - The Info node name containing the documentation (string, when found)
  manual - The Info manual name, typically 'elisp' (string, when found)
  content - The complete Info node content including all examples,
            cross-references, and related information (string, when found)
  info-ref - Info reference like '(elisp)Node Name' for direct access
             (string, when found)
  message - Error or not-found message (string, when not found)

The content field contains the entire Info node, ensuring you have full
context including:
- Complete function/variable descriptions
- All code examples and usage patterns
- Cross-references to related concepts
- Any warnings, notes, or special considerations

Common symbols that can be looked up:
- Special forms: defun, defvar, let, if, cond, lambda
- Functions: mapcar, apply, funcall, concat
- Macros: when, unless, dolist, defmacro
- Variables: load-path, emacs-version
- Concepts: 'lexical binding', 'dynamic binding'

Error cases:
- Symbol not found in documentation
- Invalid symbol name
- Info system unavailable"
   :read-only t))

;;;###autoload
(defun anvil-ide-elisp-disable ()
  "Unregister the IDE-only Elisp tools."
  (anvil-server-unregister-tool
   "elisp-info-lookup-symbol" anvil-ide-elisp--server-id))

(provide 'anvil-ide-elisp)
;;; anvil-ide-elisp.el ends here

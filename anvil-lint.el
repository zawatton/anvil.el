;;; anvil-lint.el --- Repo hygiene scanner registry  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 16 Phase 1 — pluggable lint scanner registry plus three org
;; scanners: conflict-markers (error), orphan-ids (info), and
;; broken-scheduled (warning).  Targets the `committed garbage'
;; failure mode — auto-cron commits that silently include unresolved
;; merge markers, broken SCHEDULED repeaters, or stranded :ID:
;; properties.
;;
;; Public Elisp API:
;;   (anvil-lint-register-scanner :id :family :description :scan
;;                                &optional :severity-default)
;;   (anvil-lint-unregister-scanner ID)
;;   (anvil-lint-scanners)            -> list of descriptor plists
;;   (anvil-lint &key family scope severity-min)
;;       -> list of finding plists
;;
;; A finding is:
;;   (:scanner ID :severity SEV :file PATH :line N
;;    :message STR :hint STR)
;;
;; SEVERITY ranks (`error' > `warning' > `info'); :severity-min filters
;; out lower-rank findings.  SCOPE accepts a directory path, a file
;; path, a list of paths, or nil (uses `default-directory').
;;
;; MCP tools (read-only): `lint', `lint-scanners'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'anvil-server)


;;;; --- group + defcustoms ---------------------------------------------------

(defgroup anvil-lint nil
  "Repo hygiene scanner family for anvil."
  :group 'anvil
  :prefix "anvil-lint-")

(defconst anvil-lint--server-id "emacs-eval"
  "Server id under which lint-* MCP tools register.")

(defcustom anvil-lint-default-severity-min 'info
  "Default severity floor when no `:severity-min' is passed.
Findings below this rank are dropped.  Severity rank order is
`error' (highest) > `warning' > `info' (lowest)."
  :type '(choice (const info) (const warning) (const error))
  :group 'anvil-lint)

(defcustom anvil-lint-org-extensions '("org" "org_archive")
  "File extensions (without dot) considered org files for org scanners."
  :type '(repeat string)
  :group 'anvil-lint)


;;;; --- registry ------------------------------------------------------------

(defvar anvil-lint--scanners nil
  "Alist (ID . PLIST) of registered scanners.
PLIST keys: `:family' `:description' `:scan' `:severity-default'.
Order is registration order; `anvil-lint-scanners' returns oldest-first.")

(defun anvil-lint-register-scanner (&rest props)
  "Register a scanner described by PROPS plist.
Required keys: `:id' (symbol or string), `:family' (symbol),
`:description' (string), `:scan' (function of one argument FILES,
returning a list of finding plists).
Optional: `:severity-default' (symbol used when a finding omits
`:severity').  Re-registering the same ID replaces the prior entry."
  (let* ((id (plist-get props :id))
         (family (plist-get props :family))
         (desc (plist-get props :description))
         (scan (plist-get props :scan)))
    (unless id (error "anvil-lint: scanner :id is required"))
    (unless (and family (symbolp family))
      (error "anvil-lint: scanner :family must be a non-nil symbol"))
    (unless (and (stringp desc) (not (string-empty-p desc)))
      (error "anvil-lint: scanner :description must be a non-empty string"))
    (unless (functionp scan)
      (error "anvil-lint: scanner :scan must be a function"))
    (let ((entry (cons id (list :family family
                                :description desc
                                :scan scan
                                :severity-default
                                (or (plist-get props :severity-default)
                                    'warning)))))
      (setq anvil-lint--scanners
            (cons entry (assq-delete-all id anvil-lint--scanners)))
      id)))

(defun anvil-lint-unregister-scanner (id)
  "Remove scanner ID from the registry.
Returns t when an entry was removed."
  (let ((before (length anvil-lint--scanners)))
    (setq anvil-lint--scanners (assq-delete-all id anvil-lint--scanners))
    (/= before (length anvil-lint--scanners))))

(defun anvil-lint-scanners ()
  "Return registered scanner descriptors as a list of plists.
Each entry: (:id ID :family SYM :description STR :severity-default SYM).
Order is registration order (oldest first)."
  (mapcar (lambda (cell)
            (let ((id (car cell))
                  (p (cdr cell)))
              (list :id id
                    :family (plist-get p :family)
                    :description (plist-get p :description)
                    :severity-default (plist-get p :severity-default))))
          (reverse anvil-lint--scanners)))


;;;; --- severity / scope helpers --------------------------------------------

(defconst anvil-lint--severity-rank
  '((info . 0) (warning . 1) (error . 2))
  "Severity rank table; higher number = more severe.")

(defun anvil-lint--severity-rank (sev)
  "Return numeric rank of severity symbol SEV, or 0 when unknown."
  (or (alist-get sev anvil-lint--severity-rank) 0))

(defun anvil-lint--severity-passes-p (sev floor)
  "Non-nil when severity SEV meets or exceeds the FLOOR symbol."
  (>= (anvil-lint--severity-rank sev)
      (anvil-lint--severity-rank floor)))

(defun anvil-lint--coerce-symbol (val)
  "Coerce VAL (string or symbol) to a symbol; nil/empty -> nil."
  (cond
   ((null val) nil)
   ((symbolp val) val)
   ((and (stringp val) (string-empty-p val)) nil)
   ((stringp val) (intern val))
   (t (error "anvil-lint: cannot coerce %S to symbol" val))))

(defun anvil-lint--scope->files (scope)
  "Return a list of files under SCOPE.
SCOPE may be nil (uses `default-directory'), a directory path, a file
path, or a list of paths.  Directories are walked recursively.  Files
are returned regardless of extension; per-family file filtering is
each scanner's responsibility."
  (cond
   ((null scope) (anvil-lint--directory-files default-directory))
   ((listp scope)
    (apply #'append (mapcar #'anvil-lint--scope->files scope)))
   ((file-directory-p scope) (anvil-lint--directory-files scope))
   ((file-regular-p scope) (list (expand-file-name scope)))
   (t nil)))

(defun anvil-lint--directory-files (dir)
  "Return list of regular files under DIR (recursive).
Skips dot-directories (`.git', `.worktrees', `.claude') so that
worktree clones do not double-count and `.git' internals stay out
of scope.  Symlinks pointing to directories are not followed."
  (let (out)
    (dolist (entry (directory-files dir t "[^.]" t))
      (let ((base (file-name-nondirectory entry)))
        (cond
         ((member base '("." "..")) nil)
         ((and (file-directory-p entry)
               (not (file-symlink-p entry))
               (not (string-prefix-p "." base)))
          (setq out (nconc (anvil-lint--directory-files entry) out)))
         ((file-regular-p entry)
          (push (expand-file-name entry) out)))))
    out))

(defun anvil-lint--filter-org-files (files)
  "Return only org-suffixed FILES per `anvil-lint-org-extensions'."
  (let ((re (concat "\\."
                    (regexp-opt anvil-lint-org-extensions)
                    "\\'")))
    (seq-filter (lambda (f) (string-match-p re f)) files)))


;;;; --- main entry ----------------------------------------------------------

(cl-defun anvil-lint (&key family scope severity-min)
  "Run registered scanners and return aggregated findings.

Keyword arguments:
  :family       — symbol limiting scanners to that family (e.g. `lint-org').
                  nil runs every registered scanner.
  :scope        — file path, directory path, list of paths, or nil
                  (uses `default-directory').
  :severity-min — minimum severity to surface (`info' / `warning' /
                  `error').  Defaults to `anvil-lint-default-severity-min'.

Returns a list of finding plists in the order scanners produced them.
Each finding:
  (:scanner ID :severity SYM :file PATH :line N :message STR :hint STR)"
  (let* ((files (anvil-lint--scope->files scope))
         (floor (or (anvil-lint--coerce-symbol severity-min)
                    anvil-lint-default-severity-min))
         (fam (anvil-lint--coerce-symbol family))
         out)
    (dolist (cell (reverse anvil-lint--scanners))
      (let* ((id (car cell))
             (p (cdr cell))
             (entry-fam (plist-get p :family))
             (default-sev (plist-get p :severity-default))
             (scan (plist-get p :scan)))
        (when (or (null fam) (eq fam entry-fam))
          (let ((findings (funcall scan files)))
            (dolist (f findings)
              (let* ((sev (or (plist-get f :severity) default-sev))
                     (annotated (anvil-lint--ensure-fields
                                 f id sev)))
                (when (anvil-lint--severity-passes-p sev floor)
                  (push annotated out))))))))
    (nreverse out)))

(defun anvil-lint--ensure-fields (finding scanner-id severity)
  "Return FINDING with `:scanner' and `:severity' filled when missing."
  (let ((out (copy-sequence finding)))
    (unless (plist-get out :scanner)
      (setq out (plist-put out :scanner scanner-id)))
    (unless (plist-get out :severity)
      (setq out (plist-put out :severity severity)))
    out))


;;;; --- built-in: lint-org/conflict-markers ---------------------------------

(defconst anvil-lint--conflict-start-re "^<<<<<<< "
  "Regexp matching git's conflict `ours' marker.
Anchored with a trailing space so org tables containing `<<<' do
not produce false positives.")

(defconst anvil-lint--conflict-mid-re "^=======[[:space:]]*$"
  "Regexp matching git's conflict separator (exactly 7 `=' alone on the line).")

(defconst anvil-lint--conflict-end-re "^>>>>>>> "
  "Regexp matching git's conflict `theirs' marker.")

(defun anvil-lint--scan-conflict-markers (files)
  "Scan FILES for unresolved git merge conflict markers.
Returns one finding per offending line, severity `error'."
  (let (out)
    (dolist (file (anvil-lint--filter-org-files files))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((line 1))
            (while (not (eobp))
              (let ((bol (line-beginning-position)))
                (cond
                 ((looking-at anvil-lint--conflict-start-re)
                  (push (anvil-lint--make-finding
                         file line "Unresolved merge marker: <<<<<<<"
                         "Resolve manually or abort the in-flight merge")
                        out))
                 ((looking-at anvil-lint--conflict-mid-re)
                  (push (anvil-lint--make-finding
                         file line "Unresolved merge marker: ======="
                         "Resolve manually or abort the in-flight merge")
                        out))
                 ((looking-at anvil-lint--conflict-end-re)
                  (push (anvil-lint--make-finding
                         file line "Unresolved merge marker: >>>>>>>"
                         "Resolve manually or abort the in-flight merge")
                        out)))
                (ignore bol))
              (forward-line 1)
              (cl-incf line))))))
    (nreverse out)))

(defun anvil-lint--make-finding (file line message hint)
  "Construct a finding plist (severity / scanner filled by `--ensure-fields')."
  (list :file file :line line :message message :hint hint))


;;;; --- built-in: lint-org/orphan-ids ---------------------------------------

(defconst anvil-lint--id-property-re
  "^[[:space:]]*:ID:[[:space:]]+\\([^[:space:]\n]+\\)"
  "Match an org `:ID:' property line; capture group 1 is the value.")

(defconst anvil-lint--id-link-re
  "\\[\\[id:\\([^]]+\\)\\]"
  "Match an org `[[id:...]]' link prefix; capture group 1 is the value.")

(defun anvil-lint--scan-orphan-ids (files)
  "Scan FILES for `:ID:' properties that no `[[id:...]]' link references.
Repo-wide: collects every ID and every link across all org files in
the input set, then reports definitions with zero incoming links."
  (let ((org-files (anvil-lint--filter-org-files files))
        (defs nil)        ; list of (id file line)
        (refs (make-hash-table :test 'equal))
        out)
    (dolist (file org-files)
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((line 1))
            (while (not (eobp))
              (when (looking-at anvil-lint--id-property-re)
                (push (list (match-string 1) file line) defs))
              (save-excursion
                (let ((bol (line-beginning-position))
                      (eol (line-end-position)))
                  (goto-char bol)
                  (while (re-search-forward anvil-lint--id-link-re eol t)
                    (puthash (match-string 1) t refs))))
              (forward-line 1)
              (cl-incf line))))))
    (dolist (def defs)
      (unless (gethash (nth 0 def) refs)
        (push (anvil-lint--make-finding
               (nth 1 def) (nth 2 def)
               (format "Orphan :ID: %s — no [[id:…]] link points here"
                       (nth 0 def))
               "Either link to it from another headline or remove the property")
              out)))
    (nreverse out)))


;;;; --- built-in: lint-org/broken-scheduled ---------------------------------

(defconst anvil-lint--scheduled-re
  "^[[:space:]]*\\(?:SCHEDULED\\|DEADLINE\\):[[:space:]]+<\\([^>]+\\)>"
  "Match SCHEDULED / DEADLINE timestamp head; capture group 1 is the body.")

(defconst anvil-lint--repeater-re
  "[+.]\\{1,2\\}[0-9]+[hdwmy]"
  "Validates the trailing repeater fragment if present
\(`+1w', `++3d', `.+1m', etc.).  Anchored loosely; the date head
is parsed separately via `org-time-string-to-time'.")

(defun anvil-lint--broken-scheduled-finding (file line body why)
  (anvil-lint--make-finding
   file line
   (format "Broken SCHEDULED/DEADLINE timestamp: %S (%s)" body why)
   "Use `+1w' / `+1m' / `+1y' style repeaters; verify with `C-c .'"))

(defun anvil-lint--scan-broken-scheduled (files)
  "Scan FILES for SCHEDULED/DEADLINE timestamps with unparseable bodies."
  (let (out)
    (dolist (file (anvil-lint--filter-org-files files))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((line 1))
            (while (not (eobp))
              (when (looking-at anvil-lint--scheduled-re)
                (let* ((body (match-string 1))
                       (problem (anvil-lint--scheduled-problem body)))
                  (when problem
                    (push (anvil-lint--broken-scheduled-finding
                           file line body problem)
                          out))))
              (forward-line 1)
              (cl-incf line))))))
    (nreverse out)))

(defun anvil-lint--scheduled-problem (body)
  "Return a short explanation when BODY is an unparseable scheduled stamp,
or nil when it parses OK."
  (let* ((trimmed (string-trim body))
         (date-part (if (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
                                      trimmed)
                        (match-string 1 trimmed)
                      nil))
         (rest (and date-part
                    (string-trim (substring trimmed (match-end 1))))))
    (cond
     ((null date-part) "missing YYYY-MM-DD date head")
     ((string-empty-p rest) nil)
     ;; rest begins with optional weekday word, optional HH:MM, optional repeater.
     (t (anvil-lint--scheduled-suffix-problem rest)))))

(defun anvil-lint--scheduled-suffix-problem (rest)
  "Return a problem string for the post-date fragment REST, or nil if OK."
  (let ((s rest)
        (consumed nil))
    ;; Optional weekday (3-char abbreviation like Mon / 月).
    (when (string-match "\\`\\([A-Z][a-z]\\{2\\}\\|[一-龥]\\)[[:space:]]*" s)
      (setq s (substring s (match-end 0)))
      (setq consumed t))
    ;; Optional time HH:MM (allow HH:MM-HH:MM ranges).
    (when (string-match "\\`[0-9]\\{1,2\\}:[0-9]\\{2\\}\\(?:-[0-9]\\{1,2\\}:[0-9]\\{2\\}\\)?[[:space:]]*" s)
      (setq s (substring s (match-end 0)))
      (setq consumed t))
    ;; Repeater family: + / ++ / .+ followed by N[hdwmy].
    (cond
     ((string-empty-p s) nil)
     ((string-match anvil-lint--repeater-re s)
      ;; Ensure the repeater consumes the rest.
      (let ((tail (substring s (match-end 0))))
        (if (string-match-p "\\`[[:space:]]*\\'" tail)
            nil
          (format "trailing junk after repeater: %S" (string-trim tail)))))
     (consumed
      (format "trailing fragment %S not a recognised repeater" s))
     (t (format "unparseable suffix %S" s)))))


;;;; --- registration helpers ------------------------------------------------

(defun anvil-lint--register-builtin-scanners ()
  "Register the Phase 1 org scanners."
  (anvil-lint-register-scanner
   :id 'lint-org/conflict-markers
   :family 'lint-org
   :description "Unresolved git merge conflict markers in org files."
   :severity-default 'error
   :scan #'anvil-lint--scan-conflict-markers)
  (anvil-lint-register-scanner
   :id 'lint-org/orphan-ids
   :family 'lint-org
   :description ":ID: properties with no incoming [[id:…]] link in scope."
   :severity-default 'info
   :scan #'anvil-lint--scan-orphan-ids)
  (anvil-lint-register-scanner
   :id 'lint-org/broken-scheduled
   :family 'lint-org
   :description "SCHEDULED/DEADLINE timestamps with unparseable bodies."
   :severity-default 'warning
   :scan #'anvil-lint--scan-broken-scheduled))

(defun anvil-lint--unregister-builtin-scanners ()
  "Remove the Phase 1 org scanners from the registry."
  (dolist (id '(lint-org/conflict-markers
                lint-org/orphan-ids
                lint-org/broken-scheduled))
    (anvil-lint-unregister-scanner id)))


;;;; --- MCP handlers --------------------------------------------------------

(defun anvil-lint--coerce-string (s)
  (and (stringp s) (not (string-empty-p s)) s))

(defun anvil-lint--tool-lint (&optional family scope severity-min)
  "MCP wrapper for `anvil-lint'.

MCP Parameters:
  family       - Optional family symbol as string (e.g. \"lint-org\").
                 Empty / omitted runs every registered scanner.
  scope        - Optional file or directory path.  Empty / omitted
                 uses the daemon's current `default-directory'.
  severity-min - Optional minimum severity (\"info\" / \"warning\" /
                 \"error\").  Empty / omitted uses
                 `anvil-lint-default-severity-min'."
  (anvil-server-with-error-handling
    (let* ((fam (anvil-lint--coerce-symbol (anvil-lint--coerce-string family)))
           (sc (anvil-lint--coerce-string scope))
           (floor (anvil-lint--coerce-symbol
                   (anvil-lint--coerce-string severity-min)))
           (findings (anvil-lint :family fam
                                 :scope sc
                                 :severity-min floor)))
      (list :scope (or sc default-directory)
            :family fam
            :severity-min (or floor anvil-lint-default-severity-min)
            :count (length findings)
            :findings findings))))

(defun anvil-lint--tool-scanners ()
  "MCP wrapper for `anvil-lint-scanners'.

MCP Parameters: none."
  (anvil-server-with-error-handling
    (anvil-lint-scanners)))


;;;; --- lifecycle -----------------------------------------------------------

(defun anvil-lint--register-tools ()
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-lint--tool-lint)
   :id "lint"
   :intent '(lint)
   :layer 'dev
   :server-id anvil-lint--server-id
   :description
   "Run anvil hygiene scanners over a scope.  Returns
\(:scope :family :severity-min :count :findings).  Each finding:
\(:scanner :severity :file :line :message :hint).  Phase 1 ships
three org scanners (conflict-markers, orphan-ids, broken-scheduled);
families and individual scanners can be added via
`anvil-lint-register-scanner'.  Read-only — never mutates files."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-lint--tool-scanners)
   :id "lint-scanners"
   :intent '(lint)
   :layer 'dev
   :server-id anvil-lint--server-id
   :description
   "List the currently registered lint scanners.  Returns a list of
\(:id :family :description :severity-default).  Read-only."
   :read-only t))

(defun anvil-lint--unregister-tools ()
  (dolist (id '("lint" "lint-scanners"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-lint--server-id))))

;;;###autoload
(defun anvil-lint-enable ()
  "Register lint scanners and MCP tools."
  (interactive)
  (anvil-lint--register-builtin-scanners)
  (anvil-lint--register-tools))

;;;###autoload
(defun anvil-lint-disable ()
  "Unregister lint scanners and MCP tools."
  (interactive)
  (anvil-lint--unregister-tools)
  (anvil-lint--unregister-builtin-scanners))

(provide 'anvil-lint)

;;; anvil-lint.el ends here

;;; anvil-standalone-org.el --- GNU Org functions anvil-org needs on NeLisp  -*- lexical-binding: t; -*-

;;; Commentary:

;; Standalone ships only a lightweight Org subset (emacs-org-outline.el and
;; friends).  anvil-org calls a few GNU Org functions and variables that the
;; subset lacks.  Everything here is guarded with `fboundp'/`boundp' and is
;; copied from GNU Org (org.el, org-macs.el) with `org-element' based lookups
;; replaced by their regexp equivalents (inlinetasks are not supported).
;; Loaded by Port 11 of anvil-standalone-mcp.el before anvil-org.

;;; Code:

(unless (boundp 'user-init-file) (defvar user-init-file nil))
(unless (boundp 'org-outline-regexp) (defvar org-outline-regexp "\\*+ "))
(unless (boundp 'org-outline-regexp-bol) (defvar org-outline-regexp-bol "^\\*+ "))
(unless (boundp 'org-odd-levels-only) (defvar org-odd-levels-only nil))
(unless (boundp 'org-inlinetask-min-level) (defvar org-inlinetask-min-level 15))
(unless (boundp 'org-element-use-cache) (defvar org-element-use-cache nil))
(unless (boundp 'org-scheduled-string) (defvar org-scheduled-string "SCHEDULED:"))
(unless (boundp 'org-deadline-string) (defvar org-deadline-string "DEADLINE:"))
(unless (boundp 'org-closed-string) (defvar org-closed-string "CLOSED:"))
(unless (boundp 'org-planning-line-re)
  (defvar org-planning-line-re
    "^[ \t]*\\(?:SCHEDULED:\\|DEADLINE:\\|CLOSED:\\)"))
(unless (boundp 'org-clock-string) (defvar org-clock-string "CLOCK:"))
(unless (boundp 'org-clock-line-re)
  (defvar org-clock-line-re "^[ \t]*CLOCK:"))
(unless (boundp 'org-drawer-regexp)
  (defvar org-drawer-regexp "^[ \t]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ \t]*$"))
(unless (boundp 'org-logbook-drawer-re)
  (defvar org-logbook-drawer-re
    "^[ \t]*:LOGBOOK:[ \t]*\n\\(?:[ \t]*[^ \t\n].*\n\\)*?[ \t]*:END:[ \t]*$"))
(unless (boundp 'org-property-drawer-re)
  (defvar org-property-drawer-re
    "^[ \t]*:PROPERTIES:[ \t]*\n\\(?:[ \t]*:\\S-+:\\(?: .*\\)?[ \t]*\n\\)*?[ \t]*:END:[ \t]*$"))

(unless (fboundp 'org-before-first-heading-p)
  (defun org-before-first-heading-p ()
    "Before first heading?"
    (save-excursion
      (end-of-line)
      (null (re-search-backward org-outline-regexp-bol nil t)))))

(unless (fboundp 'org-outline-level)
  (defun org-outline-level ()
    "Level of the heading at point (number of stars), 0 before the first."
    (save-excursion
      (save-restriction
        (widen)
        (end-of-line)
        (if (re-search-backward org-outline-regexp-bol nil t)
            (1- (- (match-end 0) (match-beginning 0)))
          0)))))

(unless (fboundp 'org-current-level)
  (defun org-current-level ()
    "Return the level of the current entry, or nil if before the first headline."
    (let ((level (org-outline-level)))
      (and (> level 0) level))))

(unless (fboundp 'org-back-to-heading-or-point-min)
  (defun org-back-to-heading-or-point-min (&optional invisible-ok)
    "Go back to heading or first point in buffer."
    (if (org-before-first-heading-p)
        (goto-char (point-min))
      (org-back-to-heading invisible-ok))))

(unless (fboundp 'org-at-property-p)
  (defun org-at-property-p ()
    "Non-nil when point is inside a property drawer."
    (save-excursion
      (beginning-of-line)
      (and (looking-at "[ \t]*:\\S-+:\\(?: .*\\)?[ \t]*$")
           (not (looking-at "[ \t]*:\\(?:PROPERTIES\\|END\\):"))
           (let ((pos (point)))
             (save-excursion
               (and (re-search-backward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
                    (not (re-search-forward "^[ \t]*:END:" pos t)))))))))

(unless (fboundp 'org-end-of-meta-data)
  (defun org-end-of-meta-data (&optional full)
    "Skip planning line and properties drawer in current entry (GNU org.el)."
    (org-back-to-heading t)
    (forward-line)
    (when (looking-at-p org-planning-line-re) (forward-line))
    (when (looking-at org-property-drawer-re)
      (goto-char (match-end 0))
      (forward-line))
    (when (and full (not (org-at-heading-p)))
      (catch 'exit
        (let ((end (save-excursion (outline-next-heading) (point)))
              (re (concat "[ \t]*$" "\\|" org-clock-line-re)))
          (while (not (eobp))
            (cond
             ((looking-at-p re) (forward-line))
             ((looking-at-p org-logbook-drawer-re)
              (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
                  (forward-line)
                (throw 'exit t)))
             ((and (eq full t) (looking-at-p org-drawer-regexp))
              (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
                  (forward-line)
                (throw 'exit t)))
             (t (throw 'exit t)))))))))

(unless (fboundp 'org-goto-first-child)
  (defun org-goto-first-child (&optional _element)
    "Goto the first child, even if it is invisible.
Return t when a child was found, else leave point and return nil."
    (let ((level (org-current-level))
          (pos (point)))
      (when level
        (end-of-line)
        (if (and (re-search-forward org-outline-regexp-bol nil t)
                 (> (- (match-end 0) (match-beginning 0) 1) level))
            (progn (goto-char (match-beginning 0)) t)
          (goto-char pos)
          nil)))))

(unless (fboundp 'org-get-next-sibling)
  (defun org-get-next-sibling ()
    "Move to next heading of the same level, and return point (GNU org.el)."
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (while (and (not (eobp)) (> (funcall outline-level) level))
        (outline-next-heading))
      (unless (or (eobp) (< (funcall outline-level) level))
        (point)))))

(unless (fboundp 'org-find-property)
  (defun org-find-property (property &optional value)
    "Find first entry in buffer that sets PROPERTY (to VALUE when non-nil).
Return the position where the entry begins, or nil (GNU org.el)."
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (re (org-re-property property nil (not value) value)))
        (catch 'exit
          (while (re-search-forward re nil t)
            (when (if value (org-at-property-p)
                    (org-entry-get (point) property nil t))
              (throw 'exit (progn (org-back-to-heading-or-point-min t)
                                  (point))))))))))

;; NeLisp `line-end-position' (nelisp--motion-eol) overwrites the match data
;; (a search on "\n"), which GNU never does.  The lightweight outline helper
;; `emacs-outline--heading-level-at-point' matches a heading, calls
;; `line-end-position', then reads `match-beginning 1' and signalled
;; args-out-of-range inside `outline-next-heading'.  Detect the bug at load
;; time and wrap the primitive with `save-match-data' only when present.
(let ((orig (symbol-function 'line-end-position)))
  (when (with-temp-buffer
          (insert "a\nb\n")
          (goto-char (point-min))
          (string-match "[a-z]" "zz")
          (let ((before (match-data)))
            (line-end-position)
            (not (equal before (match-data)))))
    (defalias 'line-end-position
      (lambda (&optional n) (save-match-data (funcall orig n))))))

(unless (fboundp 'shell-command-to-string)
  (defun shell-command-to-string (command)
    "Execute shell COMMAND and return its output as a string."
    (with-temp-buffer
      (call-process (if (boundp 'shell-file-name) shell-file-name "/bin/sh")
                    nil t nil
                    (if (boundp 'shell-command-switch) shell-command-switch "-c")
                    command)
      (buffer-string))))

;; `time-convert' is a C primitive missing here; `org-time-convert-to-list'
;; (used by `org-id-new') calls it with FORM `list'.  Only nil/list/integer.
(unless (fboundp 'time-convert)
  (defun time-convert (time &optional form)
    "Convert TIME to FORM `list' (HI LO US PS) or `integer' seconds."
    (let ((tm (cond ((null time) (current-time))
                    ((numberp time) (seconds-to-time time))
                    (t time))))
      (cond
       ((eq form 'integer) (truncate (float-time tm)))
       ((memq form '(list nil))
        (let ((l (append tm nil)))
          (while (< (length l) 4) (setq l (append l (list 0))))
          (if (> (length l) 4) (butlast l (- (length l) 4)) l)))
       (t (error "time-convert: unsupported form %S" form))))))

;; `revert-buffer' (files.el) is missing; anvil-org refreshes visiting buffers
;; with (revert-buffer t t t) after a disk write.  Re-read the file, run the
;; revert hooks and keep point, like GNU `revert-buffer' for a file buffer.
(unless (fboundp 'revert-buffer)
  (defvar before-revert-hook nil)
  (defvar after-revert-hook nil)
  (defun revert-buffer (&optional _ignore-auto _noconfirm _preserve-modes)
    "Replace the buffer text with the text of the visited file."
    (when buffer-file-name
      (let ((pos (point)))
        (run-hooks 'before-revert-hook)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents buffer-file-name))
        (set-buffer-modified-p nil)
        (goto-char (min pos (point-max)))
        (run-hooks 'after-revert-hook)
        t))))

;; `org-id-uuid' (the fallback when uuidgen output is unusable) hashes these.
(unless (fboundp 'recent-keys) (defun recent-keys (&optional _include-cmds) []))
(unless (fboundp 'user-uid) (defun user-uid () 0))
(unless (fboundp 'emacs-pid) (defun emacs-pid () 0))

;; Standalone has no windows or frames, so window-configuration restoring is a
;; plain `progn' (anvil-org wraps capture and agenda in `save-window-excursion').
(unless (fboundp 'save-window-excursion)
  (defmacro save-window-excursion (&rest body)
    "Execute BODY; there is no window configuration to restore here."
    (declare (indent 0))
    `(progn ,@body)))

;; org-capture saves/restores the window configuration and shows its buffer.
;; Without windows these degrade to buffer switching only.
(unless (fboundp 'current-window-configuration)
  (defun current-window-configuration (&optional _frame) nil))
(unless (fboundp 'set-window-configuration)
  (defun set-window-configuration (_configuration &optional _dont-set-frame _dont-set-miniwindow) t))
(unless (fboundp 'delete-other-windows)
  (defun delete-other-windows (&optional _window _interactive) nil))
(unless (fboundp 'switch-to-buffer)
  (defun switch-to-buffer (buffer-or-name &optional _norecord _force-same-window)
    (set-buffer (get-buffer-create buffer-or-name))))
(unless (fboundp 'pop-to-buffer)
  (defun pop-to-buffer (buffer-or-name &optional _action _norecord)
    (set-buffer (get-buffer-create buffer-or-name))))
(unless (fboundp 'pop-to-buffer-same-window)
  (defun pop-to-buffer-same-window (buffer &optional _norecord)
    (set-buffer (get-buffer-create buffer))))
(unless (fboundp 'display-buffer)
  (defun display-buffer (buffer-or-name &optional _action _frame) nil))

(unless (boundp 'org-tags-column) (defvar org-tags-column -77))
(unless (boundp 'org-auto-align-tags) (defvar org-auto-align-tags t))
(unless (boundp 'org-tags-sort-function) (defvar org-tags-sort-function nil))
(unless (boundp 'org-log-done) (defvar org-log-done nil))

(defun anvil-standalone-org--split-heading ()
  "Split the headline at point into a plist (:stars :rest :tags).
:tags is the trailing \":a:b:\" string or nil."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (unless (string-match "\\`\\(\\*+\\)\\(?: \\(.*\\)\\)?\\'" line)
      (error "Not on a headline"))
    (let ((stars (match-string 1 line))
          (rest (or (match-string 2 line) ""))
          tags)
      (when (string-match "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*\\'" rest)
        (setq tags (match-string 1 rest)
              rest (substring rest 0 (match-beginning 0))))
      (setq rest (replace-regexp-in-string "[ \t]+\\'" "" rest))
      (list :stars stars :rest rest :tags tags))))

(defun anvil-standalone-org--write-heading (stars rest tags)
  "Replace the headline at point with STARS REST and TAGS (a list) aligned."
  (let* ((head (concat stars (if (string-empty-p rest) "" (concat " " rest))))
         (tagstr (and tags (concat ":" (mapconcat #'identity tags ":") ":")))
         (new (if tagstr
                  (let* ((target (if (>= org-tags-column 0)
                                     org-tags-column
                                   (- (abs org-tags-column) (string-width tagstr))))
                         (col (max target (1+ (string-width head)))))
                    (concat head (make-string (- col (string-width head)) ?\s)
                            tagstr))
                head)))
    (delete-region (line-beginning-position) (line-end-position))
    (insert new)))

(unless (fboundp 'org-set-tags)
  (defun org-set-tags (tags)
    "Set the tags of the headline at point to TAGS (list or \":a:b:\" string)."
    (let* ((tags (cond ((listp tags) tags)
                       ((stringp tags) (split-string (string-trim tags) ":" t))
                       (t (error "Invalid tag specification: %S" tags))))
           (parts (save-excursion (beginning-of-line)
                                  (anvil-standalone-org--split-heading)))
           (old (and (plist-get parts :tags)
                     (split-string (plist-get parts :tags) ":" t))))
      (unless (equal tags old)
        (save-excursion
          (org-back-to-heading t)
          (anvil-standalone-org--write-heading
           (plist-get parts :stars) (plist-get parts :rest) tags))))))

(unless (fboundp 'org-edit-headline)
  (defun org-edit-headline (&optional heading)
    "Set the title of the headline at point to HEADING, keeping keyword, priority
and tags, then re-align the tags (GNU `org-edit-headline')."
    (save-excursion
      (org-back-to-heading t)
      (let* ((parts (anvil-standalone-org--split-heading))
             (rest (plist-get parts :rest))
             (prefix ""))
        (let ((kw (and (string-match "\\`\\([A-Z][0-9A-Z_-]*\\)\\(?: \\|\\'\\)" rest)
                       (member (match-string 1 rest) (org-todo--all-keywords))
                       (match-string 1 rest))))
          (when kw
            (setq prefix kw rest (substring rest (min (length rest) (1+ (length kw)))))))
        (when (string-match "\\`\\(\\[#[A-Z0-9]\\]\\)\\(?: \\|\\'\\)" rest)
          (setq prefix (concat prefix (if (string-empty-p prefix) "" " ")
                               (match-string 1 rest))
                rest (substring rest (min (length rest) (1+ (length (match-string 1 rest)))))))
        (anvil-standalone-org--write-heading
         (plist-get parts :stars)
         (string-trim (concat prefix (if (string-empty-p prefix) "" " ") heading))
         (and (plist-get parts :tags)
              (split-string (plist-get parts :tags) ":" t)))))))

;; The lightweight `org-todo' ignores a STATE string and prompts for one.
;; Keep it for the cycling cases and handle a string STATE like GNU
;; `org-todo' does without logging or repeaters: set the keyword, add a
;; CLOSED stamp when `org-log-done' is set and the state is a done state,
;; drop CLOSED when leaving one, run `org-after-todo-state-change-hook'.
(when (and (fboundp 'org-todo) (not (get 'org-todo 'anvil-standalone-state-aware)))
  (let ((orig (symbol-function 'org-todo)))
    (defalias 'org-todo
      (lambda (&optional arg)
        (if (not (stringp arg))
            (funcall orig arg)
          (org-back-to-heading t)
          (let* ((new (if (string-empty-p arg) nil arg))
                 (res (org-todo--replace-heading-keyword new))
                 (done (and new (org-todo--done-keyword-p new)))
                 (org-state new))
            (ignore res)
            (save-excursion
              (let ((bol (progn (forward-line 1) (point))))
                (when (looking-at "[ \t]*\\(?:SCHEDULED:\\|DEADLINE:\\|CLOSED:\\)")
                  (let ((eol (line-end-position)))
                    (if done
                        (when (and org-log-done (not (looking-at ".*CLOSED:")))
                          (goto-char bol)
                          (insert (format-time-string "CLOSED: [%Y-%m-%d %a %H:%M] "))
                          (goto-char bol))
                      (when (re-search-forward " *CLOSED: \\[[^]]*\\]" eol t)
                        (replace-match "")
                        (goto-char bol)
                        (when (looking-at "[ \t]*$")
                          (delete-region bol (min (point-max) (1+ (line-end-position)))))))))))
            (when (boundp 'org-after-todo-state-change-hook)
              (run-hooks 'org-after-todo-state-change-hook))
            new))))
    (put 'org-todo 'anvil-standalone-state-aware t)))

;; GNU `org-entry-put' formats a newly created property line with
;; `org-property-format' ("%-10s %s"); the lightweight one writes ":NAME: VALUE".
;; Pad only lines the call created; an existing property keeps its spacing.
(unless (boundp 'org-property-format) (defvar org-property-format "%-10s %s"))
(when (and (fboundp 'org-entry-put)
           (not (get 'org-entry-put 'anvil-standalone-padded)))
  (let ((orig (symbol-function 'org-entry-put)))
    (defalias 'org-entry-put
      (lambda (pom property value)
        (let* ((existed (save-excursion
                          (when pom (goto-char pom))
                          (org-entry-get (point) property nil t)))
               (res (funcall orig pom property value)))
          (when (and (not existed) (stringp value)
                     (not (member (upcase property) '("TODO" "TAGS" "ALLTAGS" "PRIORITY"))))
            (save-excursion
              (when pom (goto-char pom))
              (org-back-to-heading t)
              (let ((end (save-excursion (if (outline-next-heading) (point) (point-max))))
                    (case-fold-search t))
                (when (re-search-forward
                       (concat "^\\([ \t]*\\):" (regexp-quote property)
                               ":[ \t]+" (regexp-quote value) "[ \t]*$")
                       end t)
                  (let ((indent (match-string 1)))
                    (replace-match
                     (concat indent (format org-property-format
                                            (concat ":" property ":") value))
                     t t))))))
          res)))
    (put 'org-entry-put 'anvil-standalone-padded t)))

;; Vendor-first: evaluate selected genuine definitions straight from the vendored
;; GNU org.el (found through `load-path') instead of copying their source.
;; Only defuns/defmacros/defvars named in NAMES that are still unbound are read.
(defun anvil-standalone-org--vendor-defs (library names)
  "Eval the top-level definitions of NAMES (symbols) found in LIBRARY.el."
  ;; `locate-library' would find the lightweight src/org.el first, so resolve
  ;; LIBRARY next to the vendored org-id.el.
  (let* ((anchor (locate-library "org-id.el"))
         (file (and anchor (expand-file-name (concat library ".el")
                                             (file-name-directory anchor)))))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (name names)
          (unless (or (fboundp name) (boundp name))
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "^(\\(?:defun\\|defmacro\\|defvar\\|defconst\\|defcustom\\) "
                           (regexp-quote (symbol-name name)) "[ \n]")
                   nil t)
              (goto-char (match-beginning 0))
              (condition-case nil (eval (read (current-buffer)) t) (error nil)))))))))

(anvil-standalone-org--vendor-defs
 "org" '(org-contextualize-keys org-contextualize-validate-key
         org-ts-regexp-inactive org-overriding-default-time org-time-was-given
         org-end-time-was-given org-time-stamp-rounding-minutes
         org-extend-today-until org-use-last-clock-out-time-as-effective-time
         org-current-time org-today org-time-string-to-time org-parse-time-string
         org-time-string-to-absolute org-time-string-to-seconds
         org-time-stamp-formats org-ts-regexp org-ts-regexp-both
         org-timestamp-rounding-minutes org-capture-plist org-store-link-plist))

(provide 'anvil-standalone-org)
;;; anvil-standalone-org.el ends here

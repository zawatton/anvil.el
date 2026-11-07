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
;; Only for the lightweight legacy `org-todo': the genuine GNU `org-todo' (emacs-lib
;; tree mode) handles a STATE string itself and has no
;; `org-todo--replace-heading-keyword', which this wrapper calls.
(when (and (fboundp 'org-todo) (fboundp 'org-todo--replace-heading-keyword)
           (not (get 'org-todo 'anvil-standalone-state-aware)))
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

;; Lazy GNU org-capture (S13.8).  Nothing below runs at load time (reading
;; the vendored org.el at start-up hangs the NeLisp HEAD build, S16.2):
;; `anvil-standalone-org--ensure-capture' runs on the first org-capture-string
;; call.  Missing GNU definitions are read from the vendored org.el by name.
(declare-function anvil-standalone--elib-vendor-dir "anvil-standalone-mcp" ())
(defun anvil-standalone-org--vendor-dir ()
  "Return the vendored emacs-lisp directory, or nil."
  (let ((dir (anvil-standalone--elib-vendor-dir)))
    (and dir (file-directory-p dir) dir)))

(defun anvil-standalone-org--vendor-def (name files)
  "Evaluate the first top-level definition of NAME found in FILES."
  (let ((dir (anvil-standalone-org--vendor-dir)) done)
    (dolist (f files)
      (unless done
        (let ((path (expand-file-name f dir)))
          (when (file-readable-p path)
            (with-temp-buffer
              (insert-file-contents path)
              (goto-char (point-min))
              (when (re-search-forward
                     (concat "^(\\(?:defun\\|defmacro\\|defsubst\\|defvar\\|defcustom\\|defconst\\) "
                             (regexp-quote name) "[ \n)]")
                     nil t)
                (goto-char (match-beginning 0))
                (let ((form (read (current-buffer))))
                  (eval form t)
                  (setq done t))))))))
    done))

(defvar anvil-standalone-org--capture-ready nil)
(defvar anvil-standalone-org--capture-active nil
  "Non-nil while a capture runs on its base buffer instead of an indirect one.")

(defun anvil-standalone-org--ensure-capture ()
  "Load genuine org-capture and the stand-ins it needs, once."
  (unless anvil-standalone-org--capture-ready
    ;; emacs-lib tree mode: `find-file-noselect' leaves an .org file buffer in
    ;; `fundamental-mode', where GNU `org-element-at-point' calls
    ;; `display-warning'; the runtime's `display-warning' leaves the CURRENT
    ;; buffer read-only (substrate defect), which breaks the capture insert.
    ;; Enter `org-mode' in such buffers, as GNU's `set-auto-mode' does.
    (when (and (getenv "NELISP_ROOT_DIR") (fboundp 'advice-add)
               (not (advice-member-p 'anvil-standalone-org--auto-org-mode
                                     'find-file-noselect)))
      (defun anvil-standalone-org--auto-org-mode (buf)
        (when (and (bufferp buf) (buffer-live-p buf)
                   (buffer-file-name buf)
                   (string-match-p "\\.org\\'" (buffer-file-name buf)))
          (with-current-buffer buf
            ;; `kill-all-local-variables' wipes `buffer-file-name' here
            ;; (permanent in GNU Emacs), so restore it after the mode switch.
            (unless (derived-mode-p 'org-mode)
              (let ((file buffer-file-name))
                (org-mode)
                (setq buffer-file-name file)))))
        buf)
      (advice-add 'find-file-noselect :filter-return
                  #'anvil-standalone-org--auto-org-mode))
    (let ((dir (anvil-standalone-org--vendor-dir)))
      (unless dir (error "Vendored emacs-lisp directory not found"))
      (unless (fboundp 'org-combine-change-calls)
        (defmacro org-combine-change-calls (_beg _end &rest body)
          "Run BODY: stub as in org-compat.el for Emacs < 29."
          (declare (indent 2))
          (cons 'progn body)))
      (dolist (n '("org-contextualize-keys" "org-overriding-default-time"
                   "org-current-time" "org-timestamp-rounding-minutes"
                   "org-extend-today-until" "org-time-stamp-format"
                   "org-timestamp-formats" "org-kill-is-subtree-p"
                   "org-get-valid-level" "org-back-over-empty-lines"
                   "org-up-heading-safe" "org-entry-end-position"
                   "org-file-contents" "org-today" "org-note-abort"
                   "org-time-was-given" "org-end-time-was-given"
                   "org-inhibit-startup" "org-at-table-p"
                   "org-refile-targets" "org-blank-before-new-entry"
                   "org-directory" "org-default-notes-file"
                   "org-complex-heading-regexp-format" "org-tags-history"
                   "org-adapt-indentation" "org-log-note-clock-out"
                   "org-reverse-note-order" "org-paste-subtree" "org-markers-to-move"
                   "org-reinstall-markers-in-region"
                   "org-save-markers-in-region"
                   "org-bookmark-names-plist"))
        (unless (or (fboundp (intern n)) (boundp (intern n)))
          (anvil-standalone-org--vendor-def
           n '("org/org.el" "org/org-macs.el" "org/org-compat.el"))))
      (unless (fboundp 'with-silent-modifications)
        (anvil-standalone-org--vendor-def "with-silent-modifications"
                                          '("subr.el")))
      (unless (fboundp 'restore-buffer-modified-p)
        (defalias 'restore-buffer-modified-p #'set-buffer-modified-p))
      ;; Capture-only hooks with nothing to do for a plain entry template.
      (dolist (fn '(org-update-parent-todo-statistics org-clock-update-mode-line))
        (unless (fboundp fn) (defalias fn (lambda (&rest _) nil))))
      (unless (fboundp 'org-clock-is-active)
        (defalias 'org-clock-is-active (lambda () nil)))
      (dolist (fn '(font-lock-flush font-lock-ensure font-lock-fontify-buffer
                    force-mode-line-update redisplay))
        (unless (fboundp fn) (defalias fn (lambda (&rest _) nil))))
      ;; GNU moves to the end of the buffer when no visible heading follows;
      ;; the lightweight commands signal a user-error instead.
      (dolist (fn '(org-next-visible-heading outline-next-visible-heading))
        (when (and (fboundp fn) (not (get fn 'anvil-standalone-org)))
          (let ((orig (symbol-function fn)))
            (defalias fn
              (lambda (&rest args)
                (condition-case err (apply orig args)
                  (user-error
                   (if (equal (cadr err) "No next visible heading")
                       (goto-char (point-max))
                     (signal (car err) (cdr err)))))))
            (put fn 'anvil-standalone-org t))))
      (unless (fboundp 'untabify)
        (anvil-standalone-org--vendor-def "untabify" '("tabify.el")))
      (unless (fboundp 'get-file-buffer)
        (defun get-file-buffer (filename)
          "Return the buffer visiting FILENAME, or nil."
          (let ((f (expand-file-name filename)) res)
            (dolist (b (buffer-list))
              (when (and (not res) (buffer-file-name b)
                         (string= (expand-file-name (buffer-file-name b)) f))
                (setq res b)))
            res)))
      (unless (boundp 'kill-ring) (defvar kill-ring nil))
      (unless (boundp 'kill-ring-yank-pointer) (defvar kill-ring-yank-pointer nil))
      (defvar org-clock-marker nil)
      (unless (markerp org-clock-marker) (setq org-clock-marker (make-marker)))
      (unless (boundp 'org-clock-heading) (defvar org-clock-heading ""))
      (dolist (spec '((switch-to-buffer-other-window
                       . (lambda (b &optional _n) (set-buffer (get-buffer-create b))))
                      (pop-to-buffer
                       . (lambda (b &rest _) (set-buffer (get-buffer-create b))))
                      (delete-other-windows . (lambda (&rest _) nil))
                      (current-window-configuration . (lambda (&rest _) nil))
                      (set-window-configuration . (lambda (&rest _) nil))))
        (unless (fboundp (car spec)) (defalias (car spec) (eval (cdr spec) t))))
      (unless (fboundp 'move-to-column)
        (defun move-to-column (column &optional force)
          "Move point to COLUMN in the current line (tabs count `tab-width')."
          (beginning-of-line)
          (let ((col 0) (eol (line-end-position)))
            (while (and (< col column) (< (point) eol))
              (setq col (if (eq (char-after) ?\t)
                            (* (1+ (/ col tab-width)) tab-width)
                          (1+ col)))
              (forward-char 1))
            (when (and force (< col column))
              (insert (make-string (- column col) ?\s))
              (setq col column))
            col)))
      ;; Killing the current buffer must leave another one current.
      (unless (get 'kill-buffer 'anvil-standalone-org)
        (defalias 'anvil-standalone-org--kill-buffer-orig
          (symbol-function 'kill-buffer))
        (defun kill-buffer (&optional buffer-or-name)
          "Kill BUFFER-OR-NAME, first switching away when it is current."
          (let ((buf (if buffer-or-name (get-buffer buffer-or-name)
                       (current-buffer))))
            (when (and buf (eq buf (current-buffer)))
              (set-buffer (get-buffer-create "*scratch*")))
            ;; The capture buffer is the target's base buffer here (no
            ;; indirect buffers), and finalize must not kill it.
            (unless (and anvil-standalone-org--capture-active
                         buf (buffer-file-name buf))
              (anvil-standalone-org--kill-buffer-orig buf))))
        (put 'kill-buffer 'anvil-standalone-org t))
      (let ((load-path (cons (expand-file-name "org" dir) load-path)))
        (load "org-capture" nil t))
      (unless (fboundp 'org-capture-finalize)
        (error "GNU org-capture did not load"))
      ;; `make-indirect-buffer' returns a dead buffer here, so the capture
      ;; runs on the base buffer and `buffer-base-buffer' reports the buffer
      ;; itself while a capture is active.
      (defalias 'anvil-standalone-org--get-indirect-orig
        (symbol-function 'org-capture-get-indirect-buffer))
      (defun org-capture-get-indirect-buffer (&optional buffer _prefix)
        "Return BUFFER itself: indirect buffers are unavailable here."
        (setq anvil-standalone-org--capture-active t)
        (or buffer (current-buffer)))
      ;; `kill-all-local-variables' (run by every major mode) wipes
      ;; `buffer-file-name' in this runtime, though it is permanent in GNU
      ;; Emacs.  Keep it across `org-mode' so the capture target can be saved.
      (unless (get 'org-mode 'anvil-standalone-org)
        (advice-add 'org-mode :around
                    (lambda (orig &rest args)
                      (let ((file buffer-file-name))
                        (prog1 (apply orig args)
                          (when (and file (not buffer-file-name))
                            (setq buffer-file-name file)))))
                    '((name . anvil-standalone-org--keep-file-name)))
        (put 'org-mode 'anvil-standalone-org t))
      ;; Even so the capture target buffer loses its file name before
      ;; `save-buffer'; remember the file per target buffer and put it back.
      (unless (get 'save-buffer 'anvil-standalone-org)
        (defvar anvil-standalone-org--capture-files nil
          "Alist of (BUFFER . FILE) for buffers made by `org-capture-target-buffer'.")
        (advice-add 'org-capture-target-buffer :around
                    (lambda (orig file)
                      (let ((buf (funcall orig file)))
                        (when (bufferp buf)
                          (push (cons buf (with-current-buffer buf
                                            (or buffer-file-name
                                                (org-capture-expand-file file))))
                                anvil-standalone-org--capture-files))
                        buf)))
        (advice-add 'save-buffer :before
                    (lambda (&rest _)
                      (let ((file (cdr (assq (current-buffer)
                                             anvil-standalone-org--capture-files))))
                        (when (and file (not buffer-file-name))
                          (setq buffer-file-name file)))))
        (put 'save-buffer 'anvil-standalone-org t))
      (unless (get 'buffer-base-buffer 'anvil-standalone-org)
        (let ((orig (symbol-function 'buffer-base-buffer)))
          (defun buffer-base-buffer (&optional buffer)
            "Like `buffer-base-buffer'; a capture buffer is its own base."
            (or (funcall orig buffer)
                (and anvil-standalone-org--capture-active
                     (or buffer (current-buffer))))))
        (put 'buffer-base-buffer 'anvil-standalone-org t))
      (setq anvil-standalone-org--capture-ready t))))

;; The lightweight `org-entry-get' ignores a planning line before the property
;; drawer and the SCHEDULED/DEADLINE/CLOSED pseudo-properties; org-habit needs
;; both.  This stand-in is swapped in only while a habit call runs.
(defun anvil-standalone-org--entry-get (pom property &optional _inherit _literal-nil)
  "Return PROPERTY of the entry at POM, reading the planning line and drawer."
  (save-excursion
    (when pom (goto-char pom))
    (org-back-to-heading t)
    (let* ((end (save-excursion
                  (forward-line 1)
                  (if (re-search-forward "^\\*+ " nil t)
                      (match-beginning 0) (point-max))))
           (case-fold-search t)
           (name (upcase property)))
      (forward-line 1)
      (let ((plan (and (< (point) end)
                       (looking-at "^[ \t]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):")
                       (buffer-substring-no-properties
                        (point) (line-end-position)))))
        (when plan (forward-line 1))
        (if (member name '("SCHEDULED" "DEADLINE" "CLOSED"))
            (and plan
                 (string-match (concat name ": *\\([[<][^]>]*[]>]\\)") plan)
                 (match-string 1 plan))
          (when (and (< (point) end)
                     (looking-at "^[ \t]*:PROPERTIES:[ \t]*$"))
            (forward-line 1)
            (let (val done)
              (while (and (not done) (< (point) end)
                          (not (looking-at "^[ \t]*:END:[ \t]*$")))
                (when (looking-at (concat "^[ \t]*:" (regexp-quote name)
                                          ":[ \t]*\\(.*?\\)[ \t]*$"))
                  (setq val (match-string-no-properties 1) done t))
                (forward-line 1))
              val)))))))

;; Lazy GNU org-habit (S13.8): same pattern, first org-habit-summary call.
(defvar anvil-standalone-org--habit-ready nil)

(defun anvil-standalone-org--done-keywords ()
  "Return the done keywords of `org-todo-keywords' (GNU keeps them per buffer)."
  (let (res)
    (dolist (seq org-todo-keywords)
      (when (consp seq)
        (let* ((words (mapcar (lambda (w)
                                (if (string-match "\\`\\([^(@!/]+\\)" w)
                                    (match-string 1 w) w))
                              (cdr seq)))
               (bar (member "|" words)))
          (setq res (append res (if bar (cdr bar)
                                  (last words)))))))
    res))

(defun anvil-standalone-org--ensure-habit ()
  "Load genuine org-habit and the org.el definitions it needs, once."
  (unless anvil-standalone-org--habit-ready
    (let ((dir (anvil-standalone-org--vendor-dir)))
      (unless dir (error "Vendored emacs-lisp directory not found"))
      (dolist (n '("org-heading-regexp" "org-ts--internal-regexp" "org-ts-regexp" "org-ts-regexp-inactive"
                   "org-ts-regexp-both" "org-ts-regexp0"
                   "org-log-note-headings" "org-log-states-order-reversed"
                   "org-repeat-re" "org-get-repeat" "org-time-string-to-time"
                   "org-entry-end-position"
                   "org-today"))
        (unless (or (fboundp (intern n)) (boundp (intern n)))
          (anvil-standalone-org--vendor-def
           n '("org/org.el" "org/org-macs.el" "org/org-compat.el"))))
      (unless (boundp 'org-done-keywords) (defvar org-done-keywords nil))
      ;; The genuine org-heading-components needs org-complex-heading-regexp and
      ;; org-fold-core; org-habit reads only the headline text (element 4).
      (unless (fboundp 'org-heading-components)
        (defun org-heading-components ()
          "Return (LEVEL LEVEL TODO PRIORITY HEADLINE TAGS) of the heading at point."
          (save-excursion
            (org-back-to-heading t)
            (list (org-current-level) (org-current-level)
                  (org-get-todo-state) nil (org-get-heading t t t t) nil))))
      ;; org-habit only needs org-agenda for its graph code, not for parsing.
      (unless (featurep 'org-agenda) (provide 'org-agenda))
      (unless (fboundp 'display-color-cells)
        (defalias 'display-color-cells (lambda (&optional _frame) 16777216)))
      ;; Its last form binds a key in the agenda map; there is no agenda here.
      (unless (fboundp 'org-defkey)
        (defalias 'org-defkey (lambda (&rest _) nil)))
      ;; The genuine `org-defkey' (emacs-lib tree mode) rejects a nil keymap, so
      ;; use an empty one; org-agenda.el is not loaded here.
      (unless (boundp 'org-agenda-mode-map)
        (defvar org-agenda-mode-map (make-sparse-keymap)))
      (let ((load-path (cons (expand-file-name "org" dir) load-path)))
        (load "org-habit" nil t))
      (unless (fboundp 'org-habit-parse-todo)
        (error "GNU org-habit did not load"))
      (setq anvil-standalone-org--habit-ready t))))

(defun anvil-standalone-org-install-lazy-wrappers ()
  "Wrap the capture tool handler so GNU org-capture loads on first use."
  (when (and (fboundp 'anvil-org--tool-capture-string)
             (not (get 'anvil-org--tool-capture-string 'anvil-standalone-lazy)))
    (let ((orig (symbol-function 'anvil-org--tool-capture-string))
          (doc (documentation 'anvil-org--tool-capture-string t)))
      (defalias 'anvil-standalone-org--capture-orig orig)
      (eval `(defun anvil-org--tool-capture-string
                 (keys content &optional allow_unvalidated_target)
               ,doc
               (anvil-standalone-org--ensure-capture)
               (unwind-protect
                   (anvil-standalone-org--capture-orig
                    keys content allow_unvalidated_target)
                 (setq anvil-standalone-org--capture-active nil)))
            t))
    (put 'anvil-org--tool-capture-string 'anvil-standalone-lazy t))
  (when (and (fboundp 'anvil-org--tool-habit-summary)
             (not (get 'anvil-org--tool-habit-summary 'anvil-standalone-lazy)))
    (let ((orig (symbol-function 'anvil-org--tool-habit-summary))
          (doc (documentation 'anvil-org--tool-habit-summary t)))
      (defalias 'anvil-standalone-org--habit-orig orig)
      (eval `(defun anvil-org--tool-habit-summary (&optional files_json today)
               ,doc
               (anvil-standalone-org--ensure-habit)
               (let ((org-done-keywords (anvil-standalone-org--done-keywords))
                     (saved (mapcar (lambda (f) (cons f (symbol-function f)))
                                    '(org-entry-get org-get-scheduled-time))))
                 (unwind-protect
                     (progn
                       (defalias 'org-entry-get
                         #'anvil-standalone-org--entry-get)
                       (defalias 'org-get-scheduled-time
                         (lambda (pom &optional _inherit)
                           (let ((ts (anvil-standalone-org--entry-get
                                      pom "SCHEDULED")))
                             (and ts (org-time-string-to-time ts)))))
                       (anvil-standalone-org--habit-orig files_json today))
                   (dolist (c saved) (defalias (car c) (cdr c)))))) 
            t))
    (put 'anvil-org--tool-habit-summary 'anvil-standalone-lazy t)))

(provide 'anvil-standalone-org)
;;; anvil-standalone-org.el ends here

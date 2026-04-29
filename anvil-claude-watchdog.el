;;; anvil-claude-watchdog.el --- Detect Claude Code TUI deadlock -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 MVP — detection + notification only.  Watches Linux Claude
;; Code (Anthropic CLI) processes via /proc and the session jsonl
;; mtime.  If the process is consuming CPU while wchar/syscw stop
;; advancing for the configured threshold AND the jsonl persist file
;; goes stale, raises a desktop notification and records the event in
;; anvil-state.  Auto-recovery is intentionally Phase 2 (separate PR)
;; because false positives during long thinking turns would otherwise
;; risk killing live sessions.
;;
;; The hang signature (rchar increasing while wchar=0, signal handlers
;; not delivered) is documented in
;; docs/design/40-claude-watchdog.org.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)
(require 'anvil-state)
(require 'notifications nil 'noerror)

(defgroup anvil-claude-watchdog nil
  "External watchdog for Claude Code TUI deadlock."
  :group 'anvil
  :prefix "anvil-claude-watchdog-")

(defcustom anvil-claude-watchdog-poll-interval 30
  "Seconds between procfs samples.
Each tick scans /proc for claude PIDs and updates per-PID state.
30 s keeps overhead negligible while catching the typical hang
window (jsonl writes normally happen within a few seconds)."
  :type 'integer
  :group 'anvil-claude-watchdog)

(defcustom anvil-claude-watchdog-stagnation-threshold 60
  "Seconds of zero wchar delta before suspicion turns into hang.
Set higher to reduce false positives during long thinking turns."
  :type 'integer
  :group 'anvil-claude-watchdog)

(defcustom anvil-claude-watchdog-jsonl-staleness 120
  "Seconds since last session jsonl mtime before persist is stale.
Combined with the wchar criterion to confirm a hang."
  :type 'integer
  :group 'anvil-claude-watchdog)

(defcustom anvil-claude-watchdog-event-ttl (* 7 24 60 60)
  "Seconds to retain event records in anvil-state."
  :type 'integer
  :group 'anvil-claude-watchdog)

(defcustom anvil-claude-watchdog-on-hang-functions nil
  "Hook called when a confirmed hang is detected.
Each function receives one argument, a plist with keys:
  :pid          Process id (integer)
  :state        kernel State letter (string, e.g. \"R\")
  :rchar-delta  Bytes read since last sample (integer)
  :wchar-delta  Bytes written since last sample (integer)
  :wchar-stale-secs  Seconds wchar has been static
  :jsonl-mtime-age  Seconds since last jsonl mtime (or nil)
  :jsonl-path   Path to the latest jsonl (or nil)
  :verdict      Symbol :hang
  :detected-at  Float-time at detection"
  :type 'hook
  :group 'anvil-claude-watchdog)

(defcustom anvil-claude-watchdog-claude-cmd-regex "claude\\s-+--"
  "Regex matched against /proc/PID/cmdline to identify claude processes."
  :type 'regexp
  :group 'anvil-claude-watchdog)

(defconst anvil-claude-watchdog--state-ns "claude-watchdog-events"
  "anvil-state namespace for event history.")

(defconst anvil-claude-watchdog--server-id "emacs-eval"
  "MCP server-id under which watchdog tools are registered.")

;;; Internal state

(defvar anvil-claude-watchdog--samples (make-hash-table :test #'eql)
  "Hash table of PID → last sample plist.
Each value is a plist with keys :sampled-at :rchar :wchar :state
:jsonl-mtime :jsonl-path :wchar-stagnant-since :verdict.")

(defvar anvil-claude-watchdog--timer nil
  "The polling timer object, or nil when disabled.")

;;; procfs helpers

(defun anvil-claude-watchdog--read-file (path)
  "Read PATH and return its contents, or nil if unreadable."
  (when (file-readable-p path)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      (error nil))))

(defun anvil-claude-watchdog--proc-io (pid)
  "Return (rchar wchar syscw) ints from /proc/PID/io, or nil."
  (when-let ((s (anvil-claude-watchdog--read-file
                 (format "/proc/%d/io" pid))))
    (let (rchar wchar syscw)
      (dolist (line (split-string s "\n" t))
        (cond
         ((string-match "^rchar:\\s-+\\([0-9]+\\)" line)
          (setq rchar (string-to-number (match-string 1 line))))
         ((string-match "^wchar:\\s-+\\([0-9]+\\)" line)
          (setq wchar (string-to-number (match-string 1 line))))
         ((string-match "^syscw:\\s-+\\([0-9]+\\)" line)
          (setq syscw (string-to-number (match-string 1 line))))))
      (when (and rchar wchar)
        (list rchar wchar (or syscw 0))))))

(defun anvil-claude-watchdog--proc-state (pid)
  "Return the State letter (e.g. \"R\") from /proc/PID/status, or nil."
  (when-let ((s (anvil-claude-watchdog--read-file
                 (format "/proc/%d/status" pid))))
    (when (string-match "^State:\\s-+\\([A-Z]\\)" s)
      (match-string 1 s))))

(defun anvil-claude-watchdog--proc-cwd (pid)
  "Resolve /proc/PID/cwd to an absolute path, or nil."
  (let ((link (format "/proc/%d/cwd" pid)))
    (when (file-symlink-p link)
      (condition-case nil
          (file-truename link)
        (error nil)))))

(defun anvil-claude-watchdog--proc-cmdline (pid)
  "Return the cmdline of PID as a single space-separated string."
  (when-let ((s (anvil-claude-watchdog--read-file
                 (format "/proc/%d/cmdline" pid))))
    ;; cmdline uses NUL separators
    (replace-regexp-in-string "\0+" " " (string-trim-right s "\0+"))))

(defun anvil-claude-watchdog--list-claude-pids ()
  "Return a list of PIDs whose cmdline matches the claude regex.
The regex is `anvil-claude-watchdog-claude-cmd-regex'."
  (let (pids)
    (dolist (entry (directory-files "/proc" nil "\\`[0-9]+\\'"))
      (let ((pid (string-to-number entry)))
        (when (> pid 0)
          (when-let ((cmdline (anvil-claude-watchdog--proc-cmdline pid)))
            (when (string-match-p anvil-claude-watchdog-claude-cmd-regex cmdline)
              (push pid pids))))))
    (nreverse pids)))

;;; jsonl resolution

(defun anvil-claude-watchdog--cwd-to-project-slug (cwd)
  "Convert CWD path to Claude Code project-dir slug.
Claude stores sessions under ~/.claude/projects/<slug>/ where
SLUG = path with `/' replaced by `-' and a leading dash."
  (when (and cwd (stringp cwd))
    (replace-regexp-in-string "/" "-" cwd)))

(defun anvil-claude-watchdog--latest-jsonl-for-cwd (cwd)
  "Return (PATH MTIME-FLOAT) for the most recently modified jsonl for CWD, or nil."
  (when-let* ((slug (anvil-claude-watchdog--cwd-to-project-slug cwd))
              (dir (expand-file-name
                    (concat "~/.claude/projects/" slug)))
              (_ (file-directory-p dir))
              (files (directory-files dir t "\\.jsonl\\'" t)))
    (let (best best-mtime)
      (dolist (f files)
        (let ((m (float-time (file-attribute-modification-time
                              (file-attributes f)))))
          (when (or (null best-mtime) (> m best-mtime))
            (setq best f best-mtime m))))
      (when best (list best best-mtime)))))

;;; Heuristic

(cl-defun anvil-claude-watchdog--classify
    (&key prev now stagnation-threshold jsonl-staleness)
  "Return verdict symbol based on PREV (or nil) vs NOW samples.

PREV and NOW are plists with at least :rchar :wchar :state :sampled-at
and optionally :jsonl-mtime.  STAGNATION-THRESHOLD and JSONL-STALENESS
are integers in seconds.

Returns one of: `:healthy', `:suspicious', `:hang'.

`:hang' requires:
  - State R (running, consuming CPU)
  - wchar delta == 0 since the most recent observed change AND that
    stagnation is at least STAGNATION-THRESHOLD seconds old
  - either rchar delta > 0 since last sample OR the latest jsonl
    mtime is older than JSONL-STALENESS seconds (i.e. persist
    really has nothing fresh)."
  (cl-block nil
    (unless prev (cl-return :healthy))
    (let* ((dt (- (plist-get now :sampled-at)
                  (plist-get prev :sampled-at)))
           (drchar (- (plist-get now :rchar) (plist-get prev :rchar)))
           (dwchar (- (plist-get now :wchar) (plist-get prev :wchar)))
           (state (plist-get now :state))
           (mtime (plist-get now :jsonl-mtime))
           (now-time (plist-get now :sampled-at))
           (jsonl-age (when mtime (- now-time mtime)))
           (stagnant-since
            (or (plist-get prev :wchar-stagnant-since)
                (and (zerop dwchar) (plist-get prev :sampled-at))))
           (stagnant-secs
            (when stagnant-since (- now-time stagnant-since))))
      (cond
       ;; Output advanced — clearly healthy.
       ((> dwchar 0) :healthy)
       ;; Not running OR no input arriving and jsonl fresh — idle or normal pause.
       ((or (not (equal state "R"))
            (and (<= drchar 0)
                 (or (null jsonl-age) (< jsonl-age jsonl-staleness))))
        :healthy)
       ;; Below stagnation threshold but worth tracking.
       ((or (null stagnant-secs)
            (< stagnant-secs stagnation-threshold)
            (and dt (zerop dwchar) (< stagnant-secs stagnation-threshold)))
        :suspicious)
       ;; Stagnant long enough AND (input arriving OR jsonl stale) AND State R.
       ((and (>= stagnant-secs stagnation-threshold)
             (equal state "R")
             (or (> drchar 0)
                 (and jsonl-age (>= jsonl-age jsonl-staleness))))
        :hang)
       (t :suspicious)))))

(defun anvil-claude-watchdog--update-stagnation (prev now)
  "Carry forward or reset :wchar-stagnant-since on NOW given PREV."
  (let* ((dwchar (when prev
                   (- (plist-get now :rchar)
                      (- (plist-get prev :rchar) ;; pacify
                         (plist-get prev :rchar)))))
         (delta (when prev
                  (- (plist-get now :wchar) (plist-get prev :wchar)))))
    (ignore dwchar)
    (cond
     ((null prev) (plist-put now :wchar-stagnant-since nil))
     ((and delta (> delta 0))
      (plist-put now :wchar-stagnant-since nil))
     ((plist-get prev :wchar-stagnant-since)
      (plist-put now :wchar-stagnant-since
                 (plist-get prev :wchar-stagnant-since)))
     (t (plist-put now :wchar-stagnant-since
                   (plist-get prev :sampled-at))))))

;;; Sampling + dispatch

(defun anvil-claude-watchdog--sample-pid (pid)
  "Take a sample for PID and return its plist, or nil if unreadable."
  (when-let* ((io (anvil-claude-watchdog--proc-io pid))
              (state (anvil-claude-watchdog--proc-state pid))
              (cwd (anvil-claude-watchdog--proc-cwd pid)))
    (let* ((jsonl-info (anvil-claude-watchdog--latest-jsonl-for-cwd cwd))
           (jsonl-path (car jsonl-info))
           (jsonl-mtime (cadr jsonl-info)))
      (list :pid pid
            :sampled-at (float-time)
            :rchar (nth 0 io)
            :wchar (nth 1 io)
            :syscw (nth 2 io)
            :state state
            :cwd cwd
            :jsonl-path jsonl-path
            :jsonl-mtime jsonl-mtime))))

(defun anvil-claude-watchdog--record-event (now verdict)
  "Persist NOW + VERDICT into anvil-state under the watchdog ns."
  (let* ((key (format "%s-%d-%s"
                      (format-time-string "%Y%m%dT%H%M%S"
                                          (seconds-to-time
                                           (plist-get now :sampled-at)))
                      (plist-get now :pid)
                      (substring (symbol-name verdict) 1)))
         (val (append now (list :verdict verdict))))
    (condition-case err
        (anvil-state-set key val
                         :ns anvil-claude-watchdog--state-ns
                         :ttl anvil-claude-watchdog-event-ttl)
      (error (message "anvil-claude-watchdog: state-set failed: %S" err)))
    key))

(defun anvil-claude-watchdog--notify (now)
  "Send a desktop notification for confirmed hang NOW."
  (let* ((pid (plist-get now :pid))
         (jsonl (plist-get now :jsonl-path))
         (age (when (plist-get now :jsonl-mtime)
                (round (- (plist-get now :sampled-at)
                          (plist-get now :jsonl-mtime)))))
         (body (format "PID %d looks hung (wchar static, jsonl %s s old)%s"
                       pid (or age "?")
                       (if jsonl (concat "\n" (file-name-nondirectory jsonl))
                         ""))))
    (when (fboundp 'notifications-notify)
      (condition-case err
          (notifications-notify
           :title "Claude Code possibly hung"
           :body body
           :urgency 'normal
           :app-name "anvil-claude-watchdog"
           :timeout 30000)
        (error (message "anvil-claude-watchdog: notify failed: %S" err))))))

(defun anvil-claude-watchdog--dispatch (now verdict)
  "Run side effects for VERDICT given NOW sample."
  (when (eq verdict :hang)
    (anvil-claude-watchdog--notify now)
    (anvil-claude-watchdog--record-event now verdict)
    (run-hook-with-args 'anvil-claude-watchdog-on-hang-functions
                        (append now (list :verdict verdict)))))

(defun anvil-claude-watchdog--tick ()
  "One polling tick: sample every claude PID and dispatch verdicts."
  (condition-case err
      (let* ((pids (anvil-claude-watchdog--list-claude-pids))
             (alive (make-hash-table :test #'eql)))
        (dolist (pid pids)
          (puthash pid t alive)
          (when-let ((now (anvil-claude-watchdog--sample-pid pid)))
            (let* ((prev (gethash pid anvil-claude-watchdog--samples))
                   (_ (anvil-claude-watchdog--update-stagnation prev now))
                   (verdict (anvil-claude-watchdog--classify
                             :prev prev :now now
                             :stagnation-threshold
                             anvil-claude-watchdog-stagnation-threshold
                             :jsonl-staleness
                             anvil-claude-watchdog-jsonl-staleness)))
              (puthash pid (plist-put now :verdict verdict)
                       anvil-claude-watchdog--samples)
              (anvil-claude-watchdog--dispatch now verdict))))
        ;; Reap exited PIDs.
        (let (dead)
          (maphash (lambda (k _v)
                     (unless (gethash k alive) (push k dead)))
                   anvil-claude-watchdog--samples)
          (dolist (pid dead)
            (remhash pid anvil-claude-watchdog--samples))))
    (error (message "anvil-claude-watchdog: tick error: %S" err))))

;;; MCP tool

(defun anvil-claude-watchdog--tool-recent (limit)
  "Return the most recent watchdog events.

MCP Parameters:
  limit - integer (string) maximum events to return; default 10."
  (anvil-server-with-error-handling
    (let* ((n (max 1 (min 200
                          (cond
                           ((null limit) 10)
                           ((numberp limit) limit)
                           ((stringp limit) (string-to-number limit))
                           (t 10)))))
           (keys (anvil-state-list-keys
                  :ns anvil-claude-watchdog--state-ns
                  :limit n))
           (rows (mapcar (lambda (k)
                           (anvil-state-get k
                                            :ns anvil-claude-watchdog--state-ns))
                         keys)))
      (format "%S" (list :ns anvil-claude-watchdog--state-ns
                         :count (length rows)
                         :events rows)))))

;;; Lifecycle

;;;###autoload
(defun anvil-claude-watchdog-enable ()
  "Start the watchdog timer and register the MCP tool."
  (interactive)
  (anvil-claude-watchdog-disable)
  (anvil-server-register-tool
   #'anvil-claude-watchdog--tool-recent
   :id "anvil-claude-watchdog-recent"
   :intent '(diagnostic)
   :layer 'dev
   :description "Recent claude-code hang detection events"
   :read-only t
   :server-id anvil-claude-watchdog--server-id)
  (setq anvil-claude-watchdog--timer
        (run-with-timer anvil-claude-watchdog-poll-interval
                        anvil-claude-watchdog-poll-interval
                        #'anvil-claude-watchdog--tick))
  (when (called-interactively-p 'interactive)
    (message "anvil-claude-watchdog enabled (poll=%ds)"
             anvil-claude-watchdog-poll-interval)))

;;;###autoload
(defun anvil-claude-watchdog-disable ()
  "Stop the watchdog timer and unregister the MCP tool."
  (interactive)
  (when (timerp anvil-claude-watchdog--timer)
    (cancel-timer anvil-claude-watchdog--timer)
    (setq anvil-claude-watchdog--timer nil))
  (when (fboundp 'anvil-server-unregister-tool)
    (anvil-server-unregister-tool
     "anvil-claude-watchdog-recent"
     :server-id anvil-claude-watchdog--server-id)))

;;;###autoload
(defun anvil-claude-watchdog-status ()
  "Show currently watched claude PIDs and their last verdicts."
  (interactive)
  (let (rows)
    (maphash (lambda (pid sample)
               (push (format "  PID %d: %s (wchar=%d, state=%s, jsonl=%s)"
                             pid
                             (or (plist-get sample :verdict) :unknown)
                             (or (plist-get sample :wchar) 0)
                             (or (plist-get sample :state) "?")
                             (or (file-name-nondirectory
                                  (or (plist-get sample :jsonl-path) ""))
                                 "-"))
                     rows))
             anvil-claude-watchdog--samples)
    (message "anvil-claude-watchdog (%d watched):\n%s"
             (hash-table-count anvil-claude-watchdog--samples)
             (mapconcat #'identity (nreverse rows) "\n"))))

(provide 'anvil-claude-watchdog)
;;; anvil-claude-watchdog.el ends here

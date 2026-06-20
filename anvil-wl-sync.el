;;; anvil-wl-sync.el --- IMAP -> Maildir incremental sync driver -*- lexical-binding: t; -*-

;; The top-level sync engine: open the IMAP-over-tunnel connection, log in,
;; figure out which messages are new (UID-based incremental), fetch them and
;; deliver into a local Maildir.  Designed to run in a process SEPARATE from
;; the interactive Emacs (host `emacs --batch' today, pure NeLisp later) so
;; syncing never blocks the editor.  Sync state (UIDVALIDITY + highest UID
;; seen) is persisted next to the Maildir.

;;; Code:

(require 'cl-lib)
(require 'anvil-wl-imap)
(require 'anvil-wl-maildir)

(defun anvil-wl-sync--folder-key (mailbox)
  (replace-regexp-in-string "[^A-Za-z0-9]" "_" mailbox))

(defun anvil-wl-sync--state-path (root mailbox)
  (expand-file-name (format ".anvil-wl-state-%s.epl"
                            (anvil-wl-sync--folder-key mailbox))
                    root))

(defun anvil-wl-sync--load-state (path)
  (when (file-exists-p path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents path)
        (read (current-buffer))))))

(defun anvil-wl-sync--save-state (path plist)
  (with-temp-file path (prin1 plist (current-buffer))))

(defun anvil-wl-sync--select-info (conn mailbox)
  "SELECT MAILBOX; return plist (:exists :uidvalidity :uidnext)."
  (let* ((res (anvil-wl-imap-command conn (format "SELECT \"%s\"" mailbox)))
         (text (plist-get res :text)))
    (unless (equal (plist-get res :status) "OK")
      (error "anvil-wl-sync: SELECT %s failed (%s)"
             mailbox (plist-get res :status)))
    (list :exists (when (string-match "^\\* \\([0-9]+\\) EXISTS" text)
                    (string-to-number (match-string 1 text)))
          :uidvalidity (when (string-match "\\[UIDVALIDITY \\([0-9]+\\)\\]" text)
                         (string-to-number (match-string 1 text)))
          :uidnext (when (string-match "\\[UIDNEXT \\([0-9]+\\)\\]" text)
                     (string-to-number (match-string 1 text))))))

(cl-defun anvil-wl-sync-mailbox (&key host port user password-file root mailbox (max 0))
  "Incrementally sync MAILBOX from HOST:PORT into the Maildir ROOT/<mailbox>.
USER authenticates with the app password read from PASSWORD-FILE.
With MAX>0, fetch at most the MAX newest messages (for bounded testing);
MAX=0 means no cap.  Returns a summary plist."
  (let* ((mdir (expand-file-name (anvil-wl-sync--folder-key mailbox) root))
         (spath (anvil-wl-sync--state-path root mailbox))
         (conn nil))
    (anvil-wl-maildir-ensure mdir)
    (unwind-protect
        (progn
          (setq conn (anvil-wl-imap-open-tunnel host port))
          (anvil-wl-imap-login
           conn user
           (anvil-wl-imap-read-app-password (expand-file-name password-file)))
          (let* ((info (anvil-wl-sync--select-info conn mailbox))
                 (uidnext (plist-get info :uidnext))
                 (uidvalidity (plist-get info :uidvalidity))
                 (state (anvil-wl-sync--load-state spath))
                 (last-uid (if (and state
                                    (equal (plist-get state :uidvalidity) uidvalidity))
                               (or (plist-get state :last-uid) 0)
                             0)))
            ;; First run with a test cap: seed last-uid so the UID range search
            ;; returns roughly the MAX newest messages instead of all of them.
            (when (and (> max 0) (= last-uid 0) uidnext)
              (setq last-uid (max 0 (- uidnext max 1))))
            (let* ((raw-uids (anvil-wl-imap-uid-search
                              conn (format "UID %d:*" (1+ last-uid))))
                   (uids (cl-remove-if-not
                          (lambda (u) (> (string-to-number u) last-uid)) raw-uids))
                   (uids (sort uids (lambda (a b) (< (string-to-number a)
                                                     (string-to-number b)))))
                   (uids (if (> max 0) (last uids max) uids))
                   (delivered 0)
                   (newlast last-uid))
              (dolist (u uids)
                (let ((bytes (anvil-wl-imap-uid-fetch-message conn u)))
                  (when bytes
                    (anvil-wl-maildir-deliver mdir bytes)
                    (cl-incf delivered)
                    (setq newlast (max newlast (string-to-number u))))))
              (anvil-wl-sync--save-state
               spath (list :uidvalidity uidvalidity :last-uid newlast))
              (list :exists (plist-get info :exists)
                    :uidnext uidnext :uidvalidity uidvalidity
                    :found (length uids) :delivered delivered
                    :last-uid newlast :maildir mdir))))
      (when conn (ignore-errors (anvil-wl-imap-logout conn))))))

(provide 'anvil-wl-sync)
;;; anvil-wl-sync.el ends here

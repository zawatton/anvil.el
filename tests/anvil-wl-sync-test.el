;;; anvil-wl-sync-test.el --- Tests for the anvil-wl sync engine -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for the anvil-wl IMAP->Maildir sync engine: the Maildir dedup
;; helpers and the `anvil-wl-sync-backfill' driver.  The IMAP layer is stubbed
;; with a canned 10-message mailbox, so these tests need no network and no
;; real mail.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-wl-maildir)
(require 'anvil-wl-sync)

(defun anvil-wl-sync-test--msg (uid)
  "Return a canned RFC822 message string for UID."
  (concat "From: s@example.com\n"
          "To: me@example.com\n"
          (format "Subject: Message %d\n" uid)
          (format "Message-ID: <msg%d@example>\n" uid)
          "\n"
          (format "Body of message %d.\n" uid)))

(defun anvil-wl-sync-test--put (mdir sub uid)
  "Write the canned message for UID into MDIR/SUB; return its path."
  (let* ((d (expand-file-name sub mdir))
         (name (format "170000%04d.%d_%d.host%s" uid (emacs-pid) uid
                       (if (equal sub "cur") ":2,S" "")))
         (p (expand-file-name name d))
         (coding-system-for-write 'binary))
    (make-directory d t)
    (write-region (anvil-wl-sync-test--msg uid) nil p nil 'silent)
    p))

(defun anvil-wl-sync-test--search (criteria server-uids)
  "Emulate `anvil-wl-imap-uid-search' for a \"UID lo:hi\" CRITERIA."
  (when (string-match "UID \\([0-9]+\\):\\([0-9]+\\)" criteria)
    (let ((lo (string-to-number (match-string 1 criteria)))
          (hi (string-to-number (match-string 2 criteria))))
      (mapcar #'number-to-string
              (cl-remove-if-not (lambda (n) (and (>= n lo) (<= n hi))) server-uids)))))

(defmacro anvil-wl-sync-test--with-stubbed-imap (server-uids &rest body)
  "Run BODY with the IMAP layer stubbed against a mailbox of SERVER-UIDS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'anvil-wl-imap-open-tunnel) (lambda (&rest _) 'conn))
             ((symbol-function 'anvil-wl-imap-login) (lambda (&rest _) t))
             ((symbol-function 'anvil-wl-imap-read-app-password) (lambda (&rest _) "pw"))
             ((symbol-function 'anvil-wl-imap-logout) (lambda (&rest _) t))
             ((symbol-function 'anvil-wl-sync--select-info)
              (lambda (&rest _) (list :exists (length ,server-uids)
                                      :uidvalidity 1
                                      :uidnext (1+ (apply #'max ,server-uids)))))
             ((symbol-function 'anvil-wl-imap-uid-search)
              (lambda (_conn criteria)
                (anvil-wl-sync-test--search criteria ,server-uids)))
             ((symbol-function 'anvil-wl-imap-uid-fetch-message)
              (lambda (_conn u) (anvil-wl-sync-test--msg (string-to-number u)))))
     ,@body))

;;;; --- Maildir dedup helpers ------------------------------------------------

(ert-deftest anvil-wl-sync-test-maildir-extract-message-id ()
  (should (equal "<m@x>"
                 (anvil-wl-maildir--extract-message-id
                  "From: a\nMessage-ID: <m@x>\nSubject: s\n\nbody")))
  (should-not (anvil-wl-maildir--extract-message-id "From: a\nSubject: s\n\nbody")))

(ert-deftest anvil-wl-sync-test-maildir-count-and-ids ()
  (let ((mdir (make-temp-file "anvil-wl-sync-test-" t)))
    (unwind-protect
        (progn
          (anvil-wl-sync-test--put mdir "cur" 1)
          (anvil-wl-sync-test--put mdir "cur" 2)
          (anvil-wl-sync-test--put mdir "new" 3)
          (should (= 3 (anvil-wl-maildir-count mdir)))
          (let ((ids (anvil-wl-maildir-message-ids mdir)))
            (should (gethash "<msg1@example>" ids))
            (should (gethash "<msg3@example>" ids))
            (should-not (gethash "<msg99@example>" ids))))
      (ignore-errors (delete-directory mdir t)))))

;;;; --- backfill -------------------------------------------------------------

(defmacro anvil-wl-sync-test--with-root (&rest body)
  "Run BODY with a temp ROOT and INBOX maildir bound."
  (declare (indent 0))
  `(let* ((root (make-temp-file "anvil-wl-sync-root-" t))
          (mdir (expand-file-name "INBOX" root)))
     (anvil-wl-maildir-ensure mdir)
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory root t)))))

(defun anvil-wl-sync-test--save-state (root plist)
  (with-temp-file (anvil-wl-sync--state-path root "INBOX")
    (prin1 plist (current-buffer))))

(ert-deftest anvil-wl-sync-test-backfill-fetches-older-uids ()
  "Backfill walks below the floor (estimated from held count) to UID 1."
  (anvil-wl-sync-test--with-root
    ;; Forward sync already recorded last-uid 10; we hold the newest 3 (8,9,10).
    (anvil-wl-sync-test--save-state root '(:uidvalidity 1 :last-uid 10))
    (dolist (u '(8 9 10)) (anvil-wl-sync-test--put mdir "cur" u))
    (anvil-wl-sync-test--with-stubbed-imap '(1 2 3 4 5 6 7 8 9 10)
      (let ((r (anvil-wl-sync-backfill
                :host "h" :port 1 :user "u" :password-file "/x"
                :root root :mailbox "INBOX" :batch 10 :rounds 1)))
        ;; Estimated floor = 11 - 3 = 8 -> fetch UID 1..7, all new.
        (should (= 7 (plist-get r :delivered)))
        (should (= 0 (plist-get r :skipped)))
        (should (eq t (plist-get r :reached-bottom)))
        (should (= 1 (plist-get r :floor)))
        (should (= 10 (anvil-wl-maildir-count mdir)))))))

(ert-deftest anvil-wl-sync-test-backfill-dedups-by-message-id ()
  "A fetched UID whose Message-ID is already held is skipped, not duplicated."
  (anvil-wl-sync-test--with-root
    ;; Explicit floor 5 -> fetch UID 1..4; we already hold msg3.
    (anvil-wl-sync-test--save-state root '(:uidvalidity 1 :last-uid 10 :floor-uid 5))
    (anvil-wl-sync-test--put mdir "cur" 3)
    (anvil-wl-sync-test--with-stubbed-imap '(1 2 3 4 5 6 7 8 9 10)
      (let ((r (anvil-wl-sync-backfill
                :host "h" :port 1 :user "u" :password-file "/x"
                :root root :mailbox "INBOX" :batch 10 :rounds 1)))
        (should (= 3 (plist-get r :delivered)))   ; 1,2,4
        (should (= 1 (plist-get r :skipped)))     ; 3 deduped
        (should (= 1 (plist-get r :floor)))
        (should (eq t (plist-get r :reached-bottom)))
        (should (= 4 (anvil-wl-maildir-count mdir)))))))

(ert-deftest anvil-wl-sync-test-backfill-persists-floor ()
  "Backfill persists the lowered floor so a later run continues downward."
  (anvil-wl-sync-test--with-root
    (anvil-wl-sync-test--save-state root '(:uidvalidity 1 :last-uid 10 :floor-uid 9))
    (anvil-wl-sync-test--with-stubbed-imap '(1 2 3 4 5 6 7 8 9 10)
      ;; Round 1: floor 9 -> fetch 4..8 (batch 5), floor -> 4.
      (anvil-wl-sync-backfill :host "h" :port 1 :user "u" :password-file "/x"
                              :root root :mailbox "INBOX" :batch 5 :rounds 1)
      (let ((st (anvil-wl-sync--load-state (anvil-wl-sync--state-path root "INBOX"))))
        (should (= 4 (plist-get st :floor-uid)))
        ;; last-uid is preserved untouched.
        (should (= 10 (plist-get st :last-uid)))))))

(provide 'anvil-wl-sync-test)
;;; anvil-wl-sync-test.el ends here

;;; anvil-wl-test.el --- Tests for the anvil-wl Maildir MCP tools -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for anvil-wl.el (read / search / compose over a locally-synced
;; Maildir).  Fixtures are built programmatically into a throwaway Maildir, so
;; the tests need no network and no real mail.  Tests that exercise CJK header
;; decode/encode `skip-unless' FLIM's eword-decode/eword-encode are present, so
;; the suite still runs (degraded) on a minimal image without FLIM.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil-wl)

(defconst anvil-wl-test--cjk-subject "請求書 6月分"
  "CJK subject used to exercise RFC2047 header decode/encode.")

(defconst anvil-wl-test--cjk-body
  "請求書を送付いたします。よろしくお願いいたします。"
  "CJK body used to exercise charset + transfer-encoding decode.")

(defun anvil-wl-test--b-header (s)
  "Return S as an RFC2047 =?UTF-8?B?...?= encoded-word."
  (format "=?UTF-8?B?%s?=" (base64-encode-string (encode-coding-string s 'utf-8) t)))

(defun anvil-wl-test--b64 (s)
  "Return UTF-8 S base64-encoded with no line breaks."
  (base64-encode-string (encode-coding-string s 'utf-8) t))

(defun anvil-wl-test--write (inbox base flags content)
  "Write CONTENT (string) as INBOX/cur/BASE:2,FLAGS (binary); return BASE."
  (let* ((name (concat base ":2," flags))
         (path (expand-file-name (concat "cur/" name) inbox))
         (coding-system-for-write 'binary))
    (make-directory (file-name-directory path) t)
    (write-region (encode-coding-string content 'utf-8) nil path nil 'silent)
    base))

(defun anvil-wl-test--populate (inbox)
  "Write the standard fixture messages into INBOX."
  (anvil-wl-test--write
   inbox "msg1" "S"
   (concat
    "From: Alice <alice@example.com>\n"
    "To: bob@example.com\n"
    "Subject: Invoice for May\n"
    "Date: Mon, 01 Jun 2026 10:00:00 +0900\n"
    "Message-ID: <inv-may@example.com>\n"
    "Content-Type: text/plain; charset=us-ascii\n"
    "\n"
    "Please find the invoice. Total: 100 USD.\n"))
  (anvil-wl-test--write
   inbox "msg2" "S"
   (concat
    "From: Keiri <keiri@example.jp>\n"
    "To: me@example.com\n"
    "Subject: " (anvil-wl-test--b-header anvil-wl-test--cjk-subject) "\n"
    "Date: Tue, 02 Jun 2026 11:00:00 +0900\n"
    "Message-ID: <seikyu-june@example.jp>\n"
    "MIME-Version: 1.0\n"
    "Content-Type: text/plain; charset=UTF-8\n"
    "Content-Transfer-Encoding: base64\n"
    "\n"
    (anvil-wl-test--b64 anvil-wl-test--cjk-body) "\n"))
  (anvil-wl-test--write
   inbox "msg3" "S"
   (concat
    "From: Carol <carol@example.com>\n"
    "To: me@example.com\n"
    "Subject: Multipart hello\n"
    "Date: Wed, 03 Jun 2026 12:00:00 +0900\n"
    "Message-ID: <multi@example.com>\n"
    "MIME-Version: 1.0\n"
    "Content-Type: multipart/alternative; boundary=\"BOUND123\"\n"
    "\n"
    "This is a multi-part message in MIME format.\n"
    "--BOUND123\n"
    "Content-Type: text/plain; charset=us-ascii\n"
    "\n"
    "This is the PLAIN part.\n"
    "--BOUND123\n"
    "Content-Type: text/html; charset=us-ascii\n"
    "\n"
    "<html><body><p>This is the HTML part.</p></body></html>\n"
    "--BOUND123--\n"))
  (anvil-wl-test--write
   inbox "msg4" "S"
   (concat
    "From: Dave <dave@example.com>\n"
    "To: me@example.com\n"
    "Subject: QP test\n"
    "Date: Thu, 04 Jun 2026 13:00:00 +0900\n"
    "Message-ID: <qp@example.com>\n"
    "MIME-Version: 1.0\n"
    "Content-Type: text/plain; charset=UTF-8\n"
    "Content-Transfer-Encoding: quoted-printable\n"
    "\n"
    "Let us meet at the Caf=C3=A9 tomorrow.\n")))

(defmacro anvil-wl-test--with-maildir (&rest body)
  "Run BODY with a throwaway Maildir populated with the fixture messages.
Binds `anvil-wl-maildir-root' to a fresh temp dir and the relevant defcustoms."
  (declare (indent 0))
  `(let* ((root (make-temp-file "anvil-wl-test-" t))
          (anvil-wl-maildir-root root)
          (anvil-wl-inbox-folder "INBOX")
          (anvil-wl-drafts-folder "Drafts")
          (anvil-wl-search-body t)
          (anvil-wl-max-results 50)
          (anvil-wl-from "Tester <tester@example.com>")
          (inbox (expand-file-name "INBOX" root)))
     (dolist (sub '("new" "cur")) (make-directory (expand-file-name sub inbox) t))
     (anvil-wl-test--populate inbox)
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory root t)))))

(defun anvil-wl-test--parse (s)
  "Parse JSON string S with `false' mapped to nil (so should-not works)."
  (let ((json-false nil))
    (json-read-from-string s)))

;;;; --- parse-ct (match-data clobber regression) ----------------------------

(ert-deftest anvil-wl-test-parse-ct-preserves-match-data ()
  "Both Content-Type params survive parsing.
Guards the match-data clobber fix: `string-trim' calls `string-match'
internally, so reading capture group 2 only after trimming group 1 would
yield garbage (the original bug)."
  (let* ((parsed (anvil-wl--parse-ct
                  "text/plain; charset=\"UTF-8\"; format=flowed"))
         (params (cdr parsed)))
    (should (equal "text/plain" (car parsed)))
    (should (equal "UTF-8" (cdr (assoc "charset" params))))
    (should (equal "flowed" (cdr (assoc "format" params))))))

(ert-deftest anvil-wl-test-parse-ct-empty-defaults-to-text-plain ()
  (should (equal '("text/plain") (anvil-wl--parse-ct nil)))
  (should (equal '("text/plain") (anvil-wl--parse-ct "   "))))

(ert-deftest anvil-wl-test-parse-ct-extracts-boundary ()
  (let ((params (cdr (anvil-wl--parse-ct
                      "multipart/alternative; boundary=\"BOUND123\""))))
    (should (equal "BOUND123" (cdr (assoc "boundary" params))))))

;;;; --- list-mails ----------------------------------------------------------

(ert-deftest anvil-wl-test-list-mails-structure-and-sort ()
  "list-mails returns all fixtures, newest-first by Date."
  (anvil-wl-test--with-maildir
    (let* ((resp (anvil-wl-test--parse (anvil-wl--tool-list-mails)))
           (msgs (alist-get 'messages resp)))
      (should (= 4 (alist-get 'total resp)))
      (should (= 4 (alist-get 'count resp)))
      (should (= 4 (length msgs)))
      (should (equal "msg4" (alist-get 'message_id (aref msgs 0))))
      (should (equal "msg1" (alist-get 'message_id (aref msgs 3)))))))

(ert-deftest anvil-wl-test-list-mails-respects-max-results ()
  (anvil-wl-test--with-maildir
    (let ((resp (anvil-wl-test--parse (anvil-wl--tool-list-mails nil 2))))
      (should (= 2 (alist-get 'count resp)))
      (should (= 4 (alist-get 'total resp))))))

;;;; --- search --------------------------------------------------------------

(ert-deftest anvil-wl-test-search-ascii-subject ()
  (anvil-wl-test--with-maildir
    (let ((resp (anvil-wl-test--parse (anvil-wl--tool-search "Invoice"))))
      (should (= 1 (alist-get 'matched resp)))
      (should (equal "msg1"
                     (alist-get 'message_id (aref (alist-get 'messages resp) 0)))))))

(ert-deftest anvil-wl-test-search-requires-all-terms ()
  (anvil-wl-test--with-maildir
    (should (= 1 (alist-get 'matched
                            (anvil-wl-test--parse (anvil-wl--tool-search "Invoice May")))))
    (should (= 0 (alist-get 'matched
                            (anvil-wl-test--parse
                             (anvil-wl--tool-search "Invoice zzznope")))))))

(ert-deftest anvil-wl-test-search-cjk-subject ()
  "CJK search matches the decoded subject (needs FLIM)."
  (skip-unless (fboundp 'eword-decode-string))
  (anvil-wl-test--with-maildir
    (let ((resp (anvil-wl-test--parse (anvil-wl--tool-search "請求"))))
      (should (= 1 (alist-get 'matched resp)))
      (should (equal "msg2"
                     (alist-get 'message_id (aref (alist-get 'messages resp) 0)))))))

;;;; --- read-mail ------------------------------------------------------------

(ert-deftest anvil-wl-test-read-mail-base64-cjk-body ()
  (anvil-wl-test--with-maildir
    (let ((resp (anvil-wl-test--parse (anvil-wl--tool-read-mail "msg2"))))
      (should (string-match-p "請求書を送付" (alist-get 'body_plain resp)))
      (should (equal "<seikyu-june@example.jp>"
                     (alist-get 'message_id_header resp))))))

(ert-deftest anvil-wl-test-read-mail-decodes-cjk-subject ()
  (skip-unless (fboundp 'eword-decode-string))
  (anvil-wl-test--with-maildir
    (let ((resp (anvil-wl-test--parse (anvil-wl--tool-read-mail "msg2"))))
      (should (equal anvil-wl-test--cjk-subject (alist-get 'subject resp))))))

(ert-deftest anvil-wl-test-read-mail-multipart-prefers-plain ()
  (anvil-wl-test--with-maildir
    (let ((body (alist-get 'body_plain
                           (anvil-wl-test--parse (anvil-wl--tool-read-mail "msg3")))))
      (should (string-match-p "PLAIN part" body))
      (should-not (string-match-p "HTML part" body)))))

(ert-deftest anvil-wl-test-read-mail-quoted-printable ()
  (anvil-wl-test--with-maildir
    (let ((body (alist-get 'body_plain
                           (anvil-wl-test--parse (anvil-wl--tool-read-mail "msg4")))))
      (should (string-match-p "Café" body)))))

(ert-deftest anvil-wl-test-read-mail-unknown-id-errors ()
  (anvil-wl-test--with-maildir
    (should-error (anvil-wl--tool-read-mail "nope-does-not-exist"))))

;;;; --- compose-draft --------------------------------------------------------

(defun anvil-wl-test--draft-content (resp)
  "Return the file content of the draft described by RESP (an alist)."
  (let ((path (expand-file-name
               (concat "Drafts/cur/" (alist-get 'draft_id resp))
               anvil-wl-maildir-root)))
    (should (file-exists-p path))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents path))
      (buffer-string))))

(ert-deftest anvil-wl-test-compose-draft-writes-and-never-sends ()
  (anvil-wl-test--with-maildir
    (let* ((d (anvil-wl-test--parse
               (anvil-wl--tool-compose-draft
                "alice@example.com" "carol@example.com" "Hi there" "Hello body" nil))))
      (should (eq t (alist-get 'saved d)))
      (should-not (alist-get 'sent d))
      (let ((content (anvil-wl-test--draft-content d)))
        (should (string-match-p "^To: alice@example.com" content))
        (should (string-match-p "^Cc: carol@example.com" content))
        (should (string-match-p "^Subject: Hi there" content))
        (should (string-match-p "Hello body" content))))))

(ert-deftest anvil-wl-test-compose-draft-cjk-subject-encoded ()
  "A CJK subject is RFC2047-encoded in the draft, not written as raw bytes."
  (skip-unless (fboundp 'eword-encode-string))
  (anvil-wl-test--with-maildir
    (let* ((d (anvil-wl-test--parse
               (anvil-wl--tool-compose-draft
                "to@example.com" nil anvil-wl-test--cjk-subject "body" nil)))
           (content (anvil-wl-test--draft-content d))
           (headers (car (split-string content "\n\n"))))
      (should (string-match-p "Subject: =?" content))
      (should-not (string-match-p anvil-wl-test--cjk-subject headers)))))

(ert-deftest anvil-wl-test-compose-draft-sets-threading ()
  (anvil-wl-test--with-maildir
    (let* ((d (anvil-wl-test--parse
               (anvil-wl--tool-compose-draft
                "to@example.com" nil "Re: x" "ok" "parent@example.com")))
           (content (anvil-wl-test--draft-content d)))
      (should (string-match-p "In-Reply-To: <parent@example.com>" content))
      (should (string-match-p "References: <parent@example.com>" content)))))

(provide 'anvil-wl-test)
;;; anvil-wl-test.el ends here

;;; anvil-mu4e-test.el --- tests for anvil-mu4e -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises the Phase 1 mu4e read/search tools against the *real* `mu'
;; binary: each test builds a disposable maildir, indexes it into a
;; throwaway muhome, and asserts the JSON the tools return.  Tests
;; `skip-unless' mu is installed, so CI without mu skips rather than fails.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-mu4e)

(defconst anvil-mu4e-test--msg1
  "From: Alice Example <alice@example.com>
To: Bob User <bob@example.com>
Cc: Carol <carol@example.com>
Subject: Invoice for May
Date: Thu, 18 Jun 2026 09:31:00 +0900
Message-ID: <inv-may-2026@example.com>

Hello Bob,

Please find the invoice for May attached.

Regards,
Alice
")

(defconst anvil-mu4e-test--msg2
  "From: Dave <dave@example.com>
To: Bob User <bob@example.com>
Subject: Re: Invoice for May
Date: Thu, 18 Jun 2026 10:05:00 +0900
Message-ID: <reply1@example.com>
In-Reply-To: <inv-may-2026@example.com>
References: <inv-may-2026@example.com>

Got it, thanks.
")

(defun anvil-mu4e-test--write (path content)
  "Write CONTENT to PATH (creating parents) as utf-8-unix."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path (insert content))))

(defun anvil-mu4e-test--build-index ()
  "Create a throwaway maildir with two messages and index it with mu.
Return a plist (:root ROOT :muhome MUHOME)."
  (let* ((root (make-temp-file "anvil-mu4e-test-" t))
         (md (expand-file-name "Maildir" root))
         (mh (expand-file-name "muhome" root))
         (inbox (expand-file-name "INBOX" md)))
    (dolist (d '("cur" "new" "tmp"))
      (make-directory (expand-file-name d inbox) t))
    (anvil-mu4e-test--write (expand-file-name "cur/msg1:2,S" inbox)
                            anvil-mu4e-test--msg1)
    (anvil-mu4e-test--write (expand-file-name "new/msg2" inbox)
                            anvil-mu4e-test--msg2)
    (unless (= 0 (call-process "mu" nil nil nil "init"
                               (concat "--maildir=" md)
                               (concat "--muhome=" mh)
                               "--my-address=bob@example.com"))
      (error "anvil-mu4e-test: `mu init' failed"))
    (unless (= 0 (call-process "mu" nil nil nil "index"
                               (concat "--muhome=" mh)))
      (error "anvil-mu4e-test: `mu index' failed"))
    (list :root root :muhome mh)))

(defmacro anvil-mu4e-test--with-index (&rest body)
  "Build a throwaway mu index, bind module vars to it, run BODY, clean up."
  (declare (indent 0))
  `(let* ((ctx (anvil-mu4e-test--build-index))
          (anvil-mu4e-muhome (plist-get ctx :muhome))
          (anvil-mu4e-mu-bin (or (executable-find "mu") "mu")))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory (plist-get ctx :root) t)))))

(defun anvil-mu4e-test--parse (json)
  "Parse JSON into alists/lists for assertion (false/null -> nil)."
  (json-parse-string json :object-type 'alist :array-type 'list
                     :false-object nil :null-object nil))

(defmacro anvil-mu4e-test--with-drafts (&rest body)
  "Run BODY with a throwaway drafts maildir and inert send defaults.
Binds `anvil-mu4e-drafts-dir', `anvil-mu4e-from', and resets every send
gate so no test can ever transmit mail unless it opts in explicitly."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "anvil-mu4e-drafts-" t))
          (anvil-mu4e-drafts-dir dir)
          (anvil-mu4e-from "Bob User <bob@example.com>")
          (anvil-mu4e-allow-send nil)
          (anvil-mu4e-send-allowlist nil)
          (anvil-mu4e-send-log nil)
          ;; Hard safety net: even if a test forgets to stub it, the
          ;; transport raises instead of sending.
          (anvil-mu4e-send-function
           (lambda (&rest _)
             (error "anvil-mu4e-test: real send attempted"))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

(ert-deftest anvil-mu4e-test-search-returns-structured-results ()
  "mu4e-search returns typed message summaries from the index."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    ;; `from:alice' matches only msg1 (msg2 is from dave); a `subject:'
    ;; query would also hit msg2's "Re: Invoice for May".
    (let* ((resp (anvil-mu4e-test--parse
                  (anvil-mu4e--tool-search "from:alice" nil nil)))
           (msgs (alist-get 'messages resp))
           (m (car msgs)))
      (should (= 1 (alist-get 'count resp)))
      (should (equal "inv-may-2026@example.com" (alist-get 'message_id m)))
      (should (equal "Invoice for May" (alist-get 'subject m)))
      (should (equal "alice@example.com"
                     (alist-get 'email (car (alist-get 'from m)))))
      (should (equal "Alice Example"
                     (alist-get 'name (car (alist-get 'from m)))))
      ;; date is present but its exact value is TZ-dependent; just assert
      ;; it is a non-empty ISO-ish string.
      (should (stringp (alist-get 'date m)))
      (should-not (string-empty-p (alist-get 'date m)))
      (should (member "seen" (alist-get 'flags m))))))

(ert-deftest anvil-mu4e-test-search-no-match-is-empty ()
  "A query with no matches returns count 0 and an empty array, not an error."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((resp (anvil-mu4e-test--parse
                 (anvil-mu4e--tool-search "subject:zzznomatchzzz" nil nil))))
      (should (= 0 (alist-get 'count resp)))
      (should (null (alist-get 'messages resp))))))

(ert-deftest anvil-mu4e-test-read-mail-headers-and-body ()
  "mu4e-read-mail returns structured headers plus the decoded body."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((m (anvil-mu4e-test--parse
              (anvil-mu4e--tool-read-mail "inv-may-2026@example.com" nil))))
      (should (equal "Invoice for May" (alist-get 'subject m)))
      (should (equal "carol@example.com"
                     (alist-get 'email (car (alist-get 'cc m)))))
      (should (string-match-p "Please find the invoice"
                              (alist-get 'body_plain m)))
      ;; the header preamble must be stripped from the body
      (should-not (string-match-p "^Subject:" (alist-get 'body_plain m))))))

(ert-deftest anvil-mu4e-test-read-mail-accepts-angle-brackets ()
  "Message-ID lookup tolerates surrounding angle brackets."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((m (anvil-mu4e-test--parse
              (anvil-mu4e--tool-read-mail "<inv-may-2026@example.com>" nil))))
      (should (equal "Invoice for May" (alist-get 'subject m))))))

(ert-deftest anvil-mu4e-test-read-mail-reply-threading ()
  "A reply carries its References back to the parent message."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((m (anvil-mu4e-test--parse
              (anvil-mu4e--tool-read-mail "reply1@example.com" nil))))
      (should (equal "Re: Invoice for May" (alist-get 'subject m)))
      (should (member "inv-may-2026@example.com"
                      (alist-get 'references m))))))

(ert-deftest anvil-mu4e-test-list-mails-defaults-to-inbox ()
  "mu4e-list-mails with no query lists the inbox (both messages)."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((resp (anvil-mu4e-test--parse
                 (anvil-mu4e--tool-list-mails nil nil))))
      (should (= 2 (alist-get 'count resp))))))

(ert-deftest anvil-mu4e-test-max-results-caps-output ()
  "max_results caps the number of returned messages (string-coerced)."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((resp (anvil-mu4e-test--parse
                 ;; pass as a string, as an MCP client would
                 (anvil-mu4e--tool-list-mails "" "1"))))
      (should (= 1 (alist-get 'count resp))))))

(ert-deftest anvil-mu4e-test-missing-mu-binary-errors-actionably ()
  "An absent mu binary yields an actionable error, not a bare failure."
  (let* ((anvil-mu4e-mu-bin "/nonexistent/mu-binary-zzz")
         (err (should-error (anvil-mu4e--tool-search "x" nil nil)
                            :type 'error)))
    (should (string-match-p "mu binary not found"
                            (error-message-string err)))))

;;;; --- Phase 2: compose / send --------------------------------------------

(ert-deftest anvil-mu4e-test-compose-draft-writes-draft ()
  "mu4e-compose-draft saves a well-formed draft and never sends."
  (anvil-mu4e-test--with-drafts
    (let* ((d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft
                "alice@example.com" "carol@example.com" "Hi there"
                "Hello!\n" nil)))
           (path (alist-get 'draft_id d)))
      (should (eq t (alist-get 'saved d)))
      (should-not (alist-get 'sent d))
      (should (file-exists-p path))
      (should (file-in-directory-p path (expand-file-name anvil-mu4e-drafts-dir)))
      (should (string-match-p ":2,D\\'" path))
      (should (equal "Hi there" (anvil-mu4e--header-value path "Subject")))
      (should (equal "alice@example.com" (anvil-mu4e--header-value path "To")))
      (should (equal "carol@example.com" (anvil-mu4e--header-value path "Cc")))
      (should (string-match-p "bob@example.com"
                              (anvil-mu4e--header-value path "From")))
      (should (string-match-p
               "Hello!"
               (with-temp-buffer (insert-file-contents path) (buffer-string)))))))

(ert-deftest anvil-mu4e-test-compose-reply-sets-threading ()
  "A reply sets In-Reply-To and References even when the parent is unknown."
  (anvil-mu4e-test--with-drafts
    (let* ((d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft
                "alice@example.com" nil "Re: x" "ok"
                "ghost-msg@nowhere.example")))
           (path (alist-get 'draft_id d)))
      (should (equal "<ghost-msg@nowhere.example>"
                     (anvil-mu4e--header-value path "In-Reply-To")))
      (should (string-match-p "ghost-msg@nowhere.example"
                              (anvil-mu4e--header-value path "References"))))))

(ert-deftest anvil-mu4e-test-compose-reply-to-indexed-message ()
  "Replying to an indexed message inherits its Re: subject and references."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (anvil-mu4e-test--with-drafts
      (let* ((d (anvil-mu4e-test--parse
                 (anvil-mu4e--tool-compose-draft
                  "alice@example.com" nil nil "thanks"
                  "inv-may-2026@example.com")))
             (path (alist-get 'draft_id d)))
        (should (equal "Re: Invoice for May" (alist-get 'subject d)))
        (should (string-match-p "inv-may-2026@example.com"
                                (anvil-mu4e--header-value path "References")))))))

(ert-deftest anvil-mu4e-test-send-previews-without-confirm ()
  "Without confirm=true, mu4e-send previews and does not call the transport."
  (anvil-mu4e-test--with-drafts
    (let* ((calls '())
           (anvil-mu4e-send-function (lambda (p) (push p calls) t))
           (anvil-mu4e-allow-send t)
           (anvil-mu4e-send-allowlist '("@example\\.com\\'"))
           (d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft "alice@example.com" nil "Hi" "yo" nil)))
           (did (alist-get 'draft_id d))
           (r (anvil-mu4e-test--parse (anvil-mu4e--tool-send did :json-false))))
      (should-not (alist-get 'sent r))
      (should (string-match-p "confirm" (alist-get 'reason r)))
      (should (null calls)))))

(ert-deftest anvil-mu4e-test-send-refused-when-disabled ()
  "With confirm but `anvil-mu4e-allow-send' nil, send is refused, no transport."
  (anvil-mu4e-test--with-drafts
    (let* ((calls '())
           (anvil-mu4e-send-function (lambda (p) (push p calls) t))
           (anvil-mu4e-allow-send nil)
           (anvil-mu4e-send-allowlist '(".*"))
           (d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft "alice@example.com" nil "Hi" "yo" nil)))
           (did (alist-get 'draft_id d))
           (r (anvil-mu4e-test--parse (anvil-mu4e--tool-send did t))))
      (should-not (alist-get 'sent r))
      (should (string-match-p "disabled" (alist-get 'reason r)))
      (should (null calls)))))

(ert-deftest anvil-mu4e-test-send-refused-for-disallowed-recipient ()
  "A recipient outside the allowlist blocks the send."
  (anvil-mu4e-test--with-drafts
    (let* ((calls '())
           (anvil-mu4e-send-function (lambda (p) (push p calls) t))
           (anvil-mu4e-allow-send t)
           (anvil-mu4e-send-allowlist '("@trusted\\.example\\'"))
           (d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft "alice@example.com" nil "Hi" "yo" nil)))
           (did (alist-get 'draft_id d))
           (r (anvil-mu4e-test--parse (anvil-mu4e--tool-send did t))))
      (should-not (alist-get 'sent r))
      (should (string-match-p "allowlist" (alist-get 'reason r)))
      (should (null calls)))))

(ert-deftest anvil-mu4e-test-send-succeeds-when-gates-open ()
  "With all three gates satisfied, send calls the transport once and audits."
  (anvil-mu4e-test--with-drafts
    (let* ((calls '())
           (anvil-mu4e-send-function (lambda (p) (push p calls) t))
           (anvil-mu4e-allow-send t)
           (anvil-mu4e-send-allowlist '("@example\\.com\\'"))
           (anvil-mu4e-send-log (expand-file-name "send.log" anvil-mu4e-drafts-dir))
           (d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft "alice@example.com" nil "Hi" "yo" nil)))
           (did (alist-get 'draft_id d))
           (r (anvil-mu4e-test--parse (anvil-mu4e--tool-send did t))))
      (should (eq t (alist-get 'sent r)))
      (should (= 1 (length calls)))
      (should (equal did (car calls)))
      (should (file-exists-p anvil-mu4e-send-log))
      (should (string-match-p
               "alice@example.com"
               (with-temp-buffer
                 (insert-file-contents anvil-mu4e-send-log)
                 (buffer-string)))))))

(ert-deftest anvil-mu4e-test-send-rejects-path-outside-drafts ()
  "A draft_id outside the drafts directory is rejected (path-injection guard)."
  (anvil-mu4e-test--with-drafts
    (let ((anvil-mu4e-allow-send t)
          (anvil-mu4e-send-allowlist '(".*"))
          (anvil-mu4e-send-function (lambda (_p) t)))
      (should-error (anvil-mu4e--tool-send "/etc/hostname" t) :type 'error))))

(provide 'anvil-mu4e-test)
;;; anvil-mu4e-test.el ends here

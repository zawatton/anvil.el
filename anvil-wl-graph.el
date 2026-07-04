;;; anvil-wl-graph.el --- Microsoft Graph -> Maildir sync -*- lexical-binding: t; -*-

;;; Commentary:

;; Fetch Outlook messages through Microsoft Graph and deliver the MIME content
;; into a local Maildir.  This avoids IMAP XOAUTH2 permission drift while still
;; leaving Wanderlust and Anvil to read ordinary local Maildir files.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-http)
(require 'url-util)
(require 'anvil-wl-maildir)
(require 'anvil-wl-oauth)

(defgroup anvil-wl-graph nil
  "Microsoft Graph mail sync helpers for anvil-wl."
  :group 'anvil-wl
  :prefix "anvil-wl-graph-")

(defcustom anvil-wl-graph-endpoint "https://graph.microsoft.com/v1.0"
  "Microsoft Graph API endpoint."
  :type 'string
  :group 'anvil-wl-graph)

(defun anvil-wl-graph--headers-end ()
  "Return the byte position after HTTP headers in the current buffer."
  (or (and (boundp 'url-http-end-of-headers)
           (markerp url-http-end-of-headers)
           (marker-position url-http-end-of-headers))
      (save-excursion
        (goto-char (point-min))
        (and (re-search-forward "\r?\n\r?\n" nil t)
             (point)))))

(defun anvil-wl-graph--request (path token &optional raw absolute)
  "GET PATH with bearer TOKEN.
Return parsed JSON unless RAW is non-nil, in which case return response bytes
after the HTTP headers.  When ABSOLUTE is non-nil, PATH is a full URL."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " token))))
         (url (if absolute path (concat anvil-wl-graph-endpoint path)))
         (buf (url-retrieve-synchronously url t t 60)))
    (unless buf
      (error "anvil-wl-graph: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (let ((body-start (anvil-wl-graph--headers-end)))
            (unless body-start
              (error "anvil-wl-graph: malformed HTTP response from %s" url))
            (goto-char body-start)
            (if raw
                (let ((coding-system-for-read 'binary))
                  (buffer-substring-no-properties body-start (point-max)))
              (let ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json-false nil))
                (json-read)))))
      (kill-buffer buf))))

(defun anvil-wl-graph--post-json (path token payload)
  "POST JSON PAYLOAD to Graph PATH with bearer TOKEN."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " token))
            ("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (url (concat anvil-wl-graph-endpoint path))
         (buf (url-retrieve-synchronously url t t 60)))
    (unless buf
      (error "anvil-wl-graph: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (or (anvil-wl-graph--headers-end) (point-min)))
          ;; sendMail returns 202 Accepted with no JSON body on success.
          (let ((body (string-trim
                       (buffer-substring-no-properties (point) (point-max)))))
            (if (string-empty-p body)
                '(:accepted t)
              (let ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json-false nil))
                (json-read-from-string body)))))
      (kill-buffer buf))))

(defun anvil-wl-graph--post-mime (path token mime)
  "POST base64-encoded RFC822 MIME to Graph PATH with bearer TOKEN."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " token))
            ("Content-Type" . "text/plain")))
         (url-request-data
          (base64-encode-string (encode-coding-string mime 'utf-8-unix) t))
         (url (concat anvil-wl-graph-endpoint path))
         (buf (url-retrieve-synchronously url t t 60)))
    (unless buf
      (error "anvil-wl-graph: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (or (anvil-wl-graph--headers-end) (point-min)))
          (let ((body (string-trim
                       (buffer-substring-no-properties (point) (point-max)))))
            (if (string-empty-p body)
                '(:accepted t)
              (let ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json-false nil))
                (json-read-from-string body)))))
      (kill-buffer buf))))

(defun anvil-wl-graph--folder-key (folder)
  "Return a filesystem-safe key for FOLDER."
  (replace-regexp-in-string "[^A-Za-z0-9]" "_" folder))

(defun anvil-wl-graph--state-path (root folder)
  "Return the Graph sync state path for ROOT and FOLDER."
  (expand-file-name (format ".anvil-wl-graph-state-%s.epl"
                            (anvil-wl-graph--folder-key folder))
                    root))

(defun anvil-wl-graph--load-state (path)
  "Load Graph sync state from PATH, or nil."
  (when (file-exists-p path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents path)
        (read (current-buffer))))))

(defun anvil-wl-graph--save-state (path state)
  "Save Graph sync STATE to PATH."
  (with-temp-file path
    (let ((print-length nil)
          (print-level nil))
      (prin1 state (current-buffer)))))

(defun anvil-wl-graph--message-page (folder top token &optional next-link)
  "Return a Graph message page for FOLDER with TOP messages using TOKEN."
  (if next-link
      (anvil-wl-graph--request next-link token nil t)
    (let ((query (url-build-query-string
                  `(("$top" ,(number-to-string top))
                    ("$select" "id,receivedDateTime,subject,internetMessageId")
                    ("$orderby" "receivedDateTime desc")))))
      (anvil-wl-graph--request
       (format "/me/mailFolders/%s/messages?%s"
               (url-hexify-string folder) query)
       token))))

(defun anvil-wl-graph--message-mime (id token)
  "Return MIME bytes for Graph message ID using TOKEN."
  (anvil-wl-graph--request
   (format "/me/messages/%s/$value" (url-hexify-string id))
   token t))

;;;###autoload
(cl-defun anvil-wl-graph-sync-mailbox (&key root (folder "Inbox") local-folder
                                            (max 10) (page-size 50)
                                            token client-id)
  "Sync Outlook messages from Graph FOLDER into ROOT/<folder> Maildir.
TOKEN can provide a bearer token directly; otherwise
`anvil-wl-oauth-ensure-access-token' is used.  CLIENT-ID overrides the
configured OAuth client id when refreshing.  MAX limits the newest messages
inspected in this run; MAX=0 follows Graph paging until all available messages
have been inspected.  Returns a summary plist."
  (let* ((root (expand-file-name root))
         (mdir (expand-file-name (or local-folder
                                     (anvil-wl-graph--folder-key folder))
                                 root))
         (state-path (anvil-wl-graph--state-path root folder))
         (state (or (anvil-wl-graph--load-state state-path)
                    '(:seen-ids nil)))
         (seen-ids (copy-sequence (or (plist-get state :seen-ids) nil)))
         (seen-table (make-hash-table :test 'equal))
         (message-ids (anvil-wl-maildir-message-ids mdir))
         (token (or token (anvil-wl-oauth-ensure-access-token client-id)))
         (remaining max)
         (next-link nil)
         (fetched 0)
         (delivered 0)
         (skipped 0)
         (checked 0)
         (done nil))
    (anvil-wl-maildir-ensure mdir)
    (dolist (id seen-ids) (puthash id t seen-table))
    (while (not done)
      (let* ((top (if (> max 0)
                      (max 1 (min page-size remaining))
                    page-size))
             (page (anvil-wl-graph--message-page folder top token next-link))
             (messages (alist-get 'value page)))
        ;; Graph returns newest first; deliver oldest first for stable Maildir order.
        (dolist (msg (reverse messages))
          (let* ((id (alist-get 'id msg))
                 (mid (alist-get 'internetMessageId msg)))
            (cl-incf checked)
            (cond
             ((or (null id) (gethash id seen-table)
                  (and mid (gethash mid message-ids)))
              (cl-incf skipped))
             (t
              (let ((raw (anvil-wl-graph--message-mime id token)))
                (cl-incf fetched)
                (anvil-wl-maildir-deliver mdir raw)
                (cl-incf delivered)
                (puthash id t seen-table)
                (push id seen-ids)
                (when mid (puthash mid t message-ids)))))))
        (setq remaining (if (> max 0) (- remaining (length messages)) 0)
              next-link (alist-get '@odata.nextLink page))
        (when (or (null next-link)
                  (null messages)
                  (= (length messages) 0)
                  (and (> max 0) (<= remaining 0)))
          (setq done t))))
    (setq seen-ids (cl-subseq seen-ids 0 (min (length seen-ids) 5000)))
    (anvil-wl-graph--save-state state-path (list :seen-ids seen-ids))
    (list :folder folder
          :maildir mdir
          :checked checked
          :fetched fetched
          :delivered delivered
          :skipped skipped)))

;;;###autoload
(cl-defun anvil-wl-graph-send-mail (&key to subject body
                                         (content-type "Text")
                                         (save-to-sent-items t)
                                         token client-id)
  "Send a mail through Microsoft Graph.
TO may be a string address or a list of addresses.  SUBJECT and BODY are
strings.  CONTENT-TYPE is \"Text\" or \"HTML\".  TOKEN can provide a bearer
token directly; otherwise `anvil-wl-oauth-ensure-access-token' is used."
  (unless (and to subject body)
    (error "anvil-wl-graph-send-mail: :to, :subject, and :body are required"))
  (let* ((token (or token (anvil-wl-oauth-ensure-access-token client-id)))
         (recipients (if (listp to) to (list to)))
         (payload
          `((message .
                     ((subject . ,subject)
                      (body .
                            ((contentType . ,content-type)
                             (content . ,body)))
                      (toRecipients .
                                    ,(vconcat
                                      (mapcar
                                       (lambda (addr)
                                         `((emailAddress . ((address . ,addr)))))
                                       recipients)))))
            (saveToSentItems . ,(if save-to-sent-items t :json-false)))))
    (anvil-wl-graph--post-json "/me/sendMail" token payload)
    (list :sent t :to recipients :subject subject)))

;;;###autoload
(cl-defun anvil-wl-graph-send-mime (&key mime path token client-id)
  "Send an RFC822 MIME message through Microsoft Graph.
MIME is the full RFC822 message string.  Alternatively PATH may point to a
UTF-8 RFC822 file.  TOKEN can provide a bearer token directly; otherwise
`anvil-wl-oauth-ensure-access-token' is used."
  (let* ((token (or token (anvil-wl-oauth-ensure-access-token client-id)))
         (mime (or mime
                   (and path
                        (with-temp-buffer
                          (let ((coding-system-for-read 'utf-8))
                            (insert-file-contents path))
                          (buffer-string))))))
    (unless mime
      (error "anvil-wl-graph-send-mime: :mime or :path is required"))
    (anvil-wl-graph--post-mime "/me/sendMail" token mime)
    (list :sent t :mime t :path path)))

(provide 'anvil-wl-graph)
;;; anvil-wl-graph.el ends here

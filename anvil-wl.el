;;; anvil-wl.el --- MCP tools for the anvil-wl Maildir (Wanderlust) -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Structured MCP tools over the local Maildir that the anvil-wl sync engine
;; fills (see dev/anvil-wl/anvil-wl-sync.el).  Reads files directly and decodes
;; MIME/RFC2047 with FLIM (eword-decode) — so CJK subjects/bodies come out
;; correct, unlike mu4e's Xapian index which cannot tokenize CJK.  No network
;; here: fetching is done by the separate sync process; these tools only read
;; the local maildir, so they never block.

;;; Code:

(require 'cl-lib)
(require 'json)                  ; json-encode
(require 'mail-utils)            ; mail-fetch-field
(require 'qp nil t)              ; quoted-printable-decode-string
(require 'eword-decode nil t)   ; FLIM: RFC2047 header decode (CJK)
(require 'eword-encode nil t)   ; FLIM: RFC2047 header encode (compose)

(declare-function anvil-server-register-tool "anvil-server")
(declare-function anvil-server-unregister-tool "anvil-server")
(declare-function eword-decode-string "eword-decode" (string &optional must-unfold))
(declare-function eword-encode-string "eword-encode" (string &optional column mode))
(declare-function anvil-wl-smtp-send "anvil-wl-smtp"
                  (host port user pass from recipients message))
(declare-function wl-draft-reply "wl-draft" (buf with-arg summary-buf &optional number))
(declare-function wl-draft-yank-from-mail-reply-buffer "wl-draft"
                  (decode-it &optional ignored-fields))

(defvar elmo-localdir-folder-path)
(defvar wl-auto-flush-queue)
(defvar wl-draft-add-in-reply-to)
(defvar wl-draft-add-references)
(defvar wl-draft-buffer-style)
(defvar wl-draft-folder)
(defvar wl-draft-reply-buffer-style)
(defvar wl-draft-use-frame)
(defvar wl-from)
(defvar wl-mail-reply-buffer)
(defvar wl-queue-folder)

(defconst anvil-wl--server-id "emacs-eval"
  "MCP server id the anvil-wl tools register under (the main eval server).")

(defgroup anvil-wl nil
  "Structured MCP tools over a locally-synced Maildir (Wanderlust)."
  :group 'anvil
  :prefix "anvil-wl-")

(defcustom anvil-wl-maildir-root (expand-file-name "~/Mail-wl")
  "Root directory holding the synced Maildir folders."
  :type 'directory :group 'anvil-wl)

(defcustom anvil-wl-inbox-folder "INBOX"
  "Default folder (subdirectory of `anvil-wl-maildir-root') for listing."
  :type 'string :group 'anvil-wl)

(defcustom anvil-wl-drafts-folder "Drafts"
  "Folder where composed drafts are written."
  :type 'string :group 'anvil-wl)

(defcustom anvil-wl-max-results 50
  "Default cap on the number of messages returned."
  :type 'integer :group 'anvil-wl)

(defcustom anvil-wl-from ""
  "Default From header used when composing drafts."
  :type 'string :group 'anvil-wl)

(defcustom anvil-wl-search-body t
  "When non-nil, `wl-search' also matches the decoded message body."
  :type 'boolean :group 'anvil-wl)

(defcustom anvil-wl-body-max-chars 100000
  "Cap on the number of body characters returned by `wl-read-mail'."
  :type 'integer :group 'anvil-wl)

(defcustom anvil-wl-allow-send nil
  "Master switch: when nil, `wl-send' refuses to transmit anything."
  :type 'boolean :group 'anvil-wl)

(defcustom anvil-wl-send-allowlist nil
  "List of regexps; every recipient of a send must match one of them."
  :type '(repeat regexp) :group 'anvil-wl)

(defcustom anvil-wl-send-function #'anvil-wl-send-with-tunnel
  "Function (PATH RECIPIENTS) used to actually send a draft file.
Must signal on failure and return non-nil on success."
  :type 'function :group 'anvil-wl)

(defcustom anvil-wl-send-log nil
  "When set to a file path, append an audit line per successful send."
  :type '(choice (const nil) file) :group 'anvil-wl)

(defcustom anvil-wl-smtp-host "smtp.gmail.com"
  "SMTP server host for the default tunnel transport."
  :type 'string :group 'anvil-wl)

(defcustom anvil-wl-smtp-port 465
  "SMTP server port (implicit-TLS SMTPS) for the default tunnel transport."
  :type 'integer :group 'anvil-wl)

(defcustom anvil-wl-smtp-user ""
  "SMTP auth user for the default tunnel transport (often the From address)."
  :type 'string :group 'anvil-wl)

(defcustom anvil-wl-smtp-password-file ""
  "Path to a file holding the SMTP app password (whitespace stripped)."
  :type 'string :group 'anvil-wl)

;;;; --- coercion -------------------------------------------------------------

(defun anvil-wl--coerce-int (v default)
  "Coerce V (integer or numeric string) to an integer, else DEFAULT."
  (cond ((integerp v) v)
        ((and (stringp v) (string-match-p "\\`[0-9]+\\'" (string-trim v)))
         (string-to-number (string-trim v)))
        (t default)))

(defun anvil-wl--truthy (v)
  "Return non-nil if V is a truthy MCP value (t, \"true\", \"t\", \"1\")."
  (cond ((eq v t) t)
        ((stringp v) (member (downcase (string-trim v)) '("true" "t" "1" "yes")))
        (t nil)))

;;;; --- maildir scanning -----------------------------------------------------

(defun anvil-wl--folder-dir (folder)
  (expand-file-name folder anvil-wl-maildir-root))

(defun anvil-wl--info-suffix (flags)
  "Return a Maildir info suffix for FLAGS.
Windows file systems reject `:', so use a private `!2,' variant there.
Readers accept both the standard `:2,' suffix and this Windows-safe form."
  (concat (if (eq system-type 'windows-nt) "!2," ":2,") (or flags "")))

(defun anvil-wl--base (filename)
  "Maildir base name of FILENAME (strip the :2,FLAGS info section).
Also accepts the Windows-safe !2,FLAGS suffix produced by anvil-wl."
  (if (string-match "[!:]2,[A-Za-z]*\\'" filename)
      (substring filename 0 (match-beginning 0))
    filename))

(defun anvil-wl--flags (filename)
  "Return the Maildir flags from FILENAME.
Accepts both standard :2,FLAGS and Windows-safe !2,FLAGS suffixes."
  (if (string-match "[!:]2,\\([A-Za-z]*\\)\\'" filename)
      (match-string 1 filename)
    ""))

(defun anvil-wl--list-files (folder)
  "Return a list of (PATH BASE FLAGS) for messages in FOLDER's new/ and cur/."
  (let (out)
    (dolist (sub '("new" "cur"))
      (let ((d (expand-file-name sub (anvil-wl--folder-dir folder))))
        (when (file-directory-p d)
          (dolist (f (directory-files d t "^[^.]"))
            (when (file-regular-p f)
              (let ((n (file-name-nondirectory f)))
                (push (list f (anvil-wl--base n) (anvil-wl--flags n)) out)))))))
    out))

;;;; --- header / body decode -------------------------------------------------

(defun anvil-wl--slurp (path &optional headers-only)
  "Return PATH as a unibyte string; with HEADERS-ONLY, just the header block."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary))
      (insert-file-contents-literally path))
    (if headers-only
        (progn (goto-char (point-min))
               (if (re-search-forward "\r?\n\r?\n" nil t)
                   (buffer-substring-no-properties (point-min) (match-beginning 0))
                 (buffer-string)))
      (buffer-string)))) 

(defun anvil-wl--field (headers name)
  "Extract (folded) header NAME from the HEADERS string, or nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert headers) (insert "\n\n")
    (goto-char (point-min))
    (mail-fetch-field name)))

(defun anvil-wl--decode-hdr (raw)
  "RFC2047-decode header value RAW (CJK), returning a string (\"\" if nil)."
  (cond ((null raw) "")
        ((fboundp 'eword-decode-string)
         (or (ignore-errors (eword-decode-string raw)) raw))
        (t raw)))

(defun anvil-wl--parse-ct (ct)
  "Parse a Content-Type string CT into (TYPE . PARAMS-alist)."
  (if (or (null ct) (string-empty-p (string-trim ct)))
      '("text/plain")
    (let* ((parts (split-string ct ";" t "[ \t\n]+"))
           (type (downcase (string-trim (car parts))))
           (params nil))
      (dolist (p (cdr parts))
        (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" p)
          ;; Capture both groups BEFORE any string-trim, which calls
          ;; string-match internally and would clobber the match data.
          (let ((k (match-string 1 p))
                (v (match-string 2 p)))
            (push (cons (downcase (string-trim k))
                        (string-trim v "[ \t\r\n\"]+" "[ \t\r\n\"]+"))
                  params))))
      (cons type (nreverse params)))))

(defun anvil-wl--charset->coding (cs)
  (or (and cs (let ((c (intern (downcase (string-trim cs)))))
                (and (coding-system-p c) c)))
      'undecided))

(defun anvil-wl--decode-transfer (body cte)
  (let ((e (and cte (downcase (string-trim cte)))))
    (cond ((equal e "base64")
           (or (ignore-errors
                 (base64-decode-string
                  (replace-regexp-in-string "[^A-Za-z0-9+/=]" "" body)))
               body))
          ((and (equal e "quoted-printable") (fboundp 'quoted-printable-decode-string))
           (or (ignore-errors
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert body)
                   (quoted-printable-decode-region (point-min) (point-max))
                   (buffer-string)))
               body))
          (t body))))

(defun anvil-wl--strip-html (s)
  (let ((x (replace-regexp-in-string "<[^>]*>" "" s)))
    (dolist (pair '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<")
                    ("&gt;" . ">") ("&quot;" . "\"") ("&#39;" . "'")))
      (setq x (replace-regexp-in-string (car pair) (cdr pair) x)))
    (string-trim x)))

(defun anvil-wl--split-multipart (body boundary)
  "Split multipart BODY into raw part strings by BOUNDARY."
  (let* ((delim (concat "--" (regexp-quote boundary)))
         (regexp (concat "\r?\n?" delim "\\(--\\)?\r?\n?"))
         (chunks (split-string body regexp t))
         (starts-at-boundary
          (string-match-p (concat "\\`" delim "\\(--\\)?\r?\n?") body)))
    (if starts-at-boundary
        chunks
      (cdr chunks))))

(defun anvil-wl--part-text (body ct cte &optional depth)
  "Return decoded text of a MIME part with Content-Type CT, encoding CTE.
Recurses into multipart, preferring text/plain.  DEPTH guards recursion."
  (setq depth (or depth 0))
  (let* ((parsed (anvil-wl--parse-ct ct))
         (type (car parsed))
         (params (cdr parsed)))
    (cond
     ((and (< depth 6) (string-prefix-p "multipart/" type))
      (let ((boundary (cdr (assoc "boundary" params)))
            (chosen nil) (first-text nil))
        (when boundary
          (dolist (p (anvil-wl--split-multipart body boundary))
            (let* ((psep (string-match "\r?\n\r?\n" p))
                   (ph (if psep (substring p 0 psep) ""))
                   (pb (if psep (substring p (match-end 0)) p))
                   (hdrs (concat ph "\n"))
                   (pct (anvil-wl--field hdrs "Content-Type"))
                   (pcte (anvil-wl--field hdrs "Content-Transfer-Encoding"))
                   (ptype (car (anvil-wl--parse-ct pct)))
                   (txt (and psep (anvil-wl--part-text pb pct pcte (1+ depth)))))
              (when (and txt (> (length (string-trim txt)) 0))
                (cond ((string-prefix-p "text/plain" ptype) (unless chosen (setq chosen txt)))
                      ((string-prefix-p "multipart/" ptype) (unless chosen (setq chosen txt)))
                      ((and (string-prefix-p "text/" ptype) (not first-text))
                       (setq first-text txt)))))))
        (or chosen first-text "")))
     ((string-prefix-p "text/" type)
      (let* ((decoded (anvil-wl--decode-transfer body cte))
             (text (decode-coding-string
                    decoded (anvil-wl--charset->coding (cdr (assoc "charset" params))))))
        (if (string-prefix-p "text/html" type) (anvil-wl--strip-html text) text)))
     (t ""))))

(defun anvil-wl--decode-body (path)
  "Return the decoded plain-text body of the message file at PATH."
  (let* ((raw (anvil-wl--slurp path))
         (sep (string-match "\r?\n\r?\n" raw))
         (headers (if sep (substring raw 0 sep) raw))
         (body (if sep (substring raw (match-end 0)) ""))
         (text (anvil-wl--part-text body
                                    (anvil-wl--field headers "Content-Type")
                                    (anvil-wl--field headers "Content-Transfer-Encoding"))))
    (if (> (length text) anvil-wl-body-max-chars)
        (concat (substring text 0 anvil-wl-body-max-chars) "\n[...truncated]")
      text)))

;;;; --- summaries / sorting --------------------------------------------------

(defun anvil-wl--summary (entry)
  "ENTRY is (PATH BASE FLAGS).  Return an alist of decoded summary fields."
  (let* ((path (nth 0 entry)) (base (nth 1 entry)) (flags (nth 2 entry))
         (h (anvil-wl--slurp path t)))
    `((message_id . ,base)
      (date . ,(or (anvil-wl--field h "Date") ""))
      (from . ,(anvil-wl--decode-hdr (anvil-wl--field h "From")))
      (to . ,(anvil-wl--decode-hdr (anvil-wl--field h "To")))
      (cc . ,(anvil-wl--decode-hdr (anvil-wl--field h "Cc")))
      (subject . ,(anvil-wl--decode-hdr (anvil-wl--field h "Subject")))
      (flags . ,flags)
      (folder . ,anvil-wl-inbox-folder)
      (file . ,path))))

(defun anvil-wl--entry-time (entry)
  "Sortable float time for ENTRY, from the Date header or file mtime."
  (let* ((path (nth 0 entry))
         (d (anvil-wl--field (anvil-wl--slurp path t) "Date")))
    (or (and d (ignore-errors (float-time (date-to-time d))))
        (ignore-errors (float-time (file-attribute-modification-time
                                    (file-attributes path))))
        0.0)))

(defun anvil-wl--sorted-entries (folder)
  "Return FOLDER entries newest-first."
  (sort (anvil-wl--list-files folder)
        (lambda (a b) (> (anvil-wl--entry-time a) (anvil-wl--entry-time b)))))

;;;; --- tools: read / search -------------------------------------------------

(defun anvil-wl--tool-list-mails (&optional folder max_results)
  "List recent mail in a Maildir folder as JSON.

MCP Parameters:
  folder - Optional folder name (default `anvil-wl-inbox-folder', e.g. INBOX).
  max_results - Optional integer cap (default `anvil-wl-max-results')."
  (let* ((fld (if (and folder (stringp folder) (not (string-empty-p (string-trim folder))))
                  folder anvil-wl-inbox-folder))
         (maxn (anvil-wl--coerce-int max_results anvil-wl-max-results))
         (anvil-wl-inbox-folder fld)
         (entries (anvil-wl--sorted-entries fld))
         (picked (seq-take entries maxn)))
    (json-encode
     `((folder . ,fld)
       (count . ,(length picked))
       (total . ,(length entries))
       (messages . ,(vconcat (mapcar #'anvil-wl--summary picked)))))))

(defun anvil-wl--match-entry-p (entry terms)
  "Non-nil if every term in TERMS appears in ENTRY's decoded text."
  (let* ((path (nth 0 entry))
         (h (anvil-wl--slurp path t))
         (hay (concat (anvil-wl--decode-hdr (anvil-wl--field h "Subject")) "\n"
                      (anvil-wl--decode-hdr (anvil-wl--field h "From")) "\n"
                      (anvil-wl--decode-hdr (anvil-wl--field h "To")) "\n"
                      (anvil-wl--decode-hdr (anvil-wl--field h "Cc")) "\n"
                      (if anvil-wl-search-body
                          (ignore-errors (anvil-wl--decode-body path)) "")))
         (hay (downcase hay)))
    (cl-every (lambda (term) (string-match-p (regexp-quote (downcase term)) hay)) terms)))

(defun anvil-wl--tool-search (query &optional max_results folder)
  "Search a Maildir folder (subject/from/to/cc and optionally body) as JSON.
CJK works because headers/body are decoded before matching.

MCP Parameters:
  query - Space-separated terms; a message must contain ALL of them.
  max_results - Optional integer cap (default `anvil-wl-max-results').
  folder - Optional folder (default `anvil-wl-inbox-folder')."
  (let* ((fld (if (and folder (stringp folder) (not (string-empty-p (string-trim folder))))
                  folder anvil-wl-inbox-folder))
         (maxn (anvil-wl--coerce-int max_results anvil-wl-max-results))
         (anvil-wl-inbox-folder fld)
         (terms (split-string (or query "") "[ \t\n]+" t))
         (entries (anvil-wl--sorted-entries fld))
         (hits (if terms
                   (seq-filter (lambda (e) (anvil-wl--match-entry-p e terms)) entries)
                 entries))
         (picked (seq-take hits maxn)))
    (json-encode
     `((query . ,(or query ""))
       (folder . ,fld)
       (count . ,(length picked))
       (matched . ,(length hits))
       (messages . ,(vconcat (mapcar #'anvil-wl--summary picked)))))))

(defun anvil-wl--find-by-id (message_id &optional folder)
  "Return the ENTRY (PATH BASE FLAGS) whose base matches MESSAGE-ID."
  (let ((target (anvil-wl--base (string-trim (format "%s" message_id)))))
    (seq-find (lambda (e) (equal (nth 1 e) target))
              (anvil-wl--list-files (or folder anvil-wl-inbox-folder)))))

(defun anvil-wl--tool-read-mail (message_id &optional folder)
  "Read one message (decoded headers + plain-text body) as JSON.

MCP Parameters:
  message_id - The Maildir id returned by wl-list-mails/wl-search.
  folder - Optional folder (default `anvil-wl-inbox-folder')."
  (let* ((fld (if (and folder (stringp folder) (not (string-empty-p (string-trim folder))))
                  folder anvil-wl-inbox-folder))
         (anvil-wl-inbox-folder fld)
         (entry (anvil-wl--find-by-id message_id fld)))
    (unless entry
      (error "anvil-wl: no message %S in folder %s" message_id fld))
    (let* ((path (nth 0 entry))
           (h (anvil-wl--slurp path t)))
      (json-encode
       (append
        (anvil-wl--summary entry)
        `((message_id_header . ,(or (anvil-wl--field h "Message-ID") ""))
          (in_reply_to . ,(or (anvil-wl--field h "In-Reply-To") ""))
          (references . ,(or (anvil-wl--field h "References") ""))
          (body_plain . ,(anvil-wl--decode-body path))))))))

;;;; --- tools: compose -------------------------------------------------------

(defvar anvil-wl--counter 0)

(defun anvil-wl--unique-name ()
  (format "%d.%d_%d.%s"
          (truncate (float-time)) (emacs-pid) (cl-incf anvil-wl--counter)
          (if (fboundp 'system-name) (or (system-name) "localhost") "localhost")))

(defun anvil-wl--from-domain (from)
  (if (string-match "@\\([[:alnum:].-]+\\)" (or from "")) (match-string 1 from)
    "anvil.local"))

(defun anvil-wl--gen-message-id (from)
  (format "<anvil-wl-%s-%d@%s>"
          (format-time-string "%Y%m%d%H%M%S") (random 1000000)
          (anvil-wl--from-domain from)))

(defun anvil-wl--angle (id)
  (let ((s (string-trim (format "%s" (or id "")))))
    (cond ((string-empty-p s) "")
          ((string-prefix-p "<" s) s)
          (t (concat "<" s ">")))))

(defun anvil-wl--enc-subject (subj)
  (if (and (fboundp 'eword-encode-string) (string-match-p "[^[:ascii:]]" (or subj "")))
      (or (ignore-errors (eword-encode-string subj)) subj)
    (or subj "")))

(defun anvil-wl--tool-compose-draft (to &optional cc subject body in_reply_to)
  "Compose a message and save it as a draft.  NEVER sends.

MCP Parameters:
  to - Recipient address(es).
  cc - Optional Cc address(es).
  subject - Optional subject (RFC2047-encoded if it contains non-ASCII).
  body - Optional plain-text (UTF-8) body.
  in_reply_to - Optional Message-ID being replied to (sets threading headers)."
  (let* ((from (if (and anvil-wl-from (not (string-empty-p anvil-wl-from)))
                   anvil-wl-from "me@localhost"))
         (mid (anvil-wl--gen-message-id from))
         (irt (anvil-wl--angle in_reply_to))
         (drafts (anvil-wl--folder-dir anvil-wl-drafts-folder))
         (name (anvil-wl--unique-name))
         (tmp (expand-file-name (concat "tmp/" name) drafts))
         (draft-id (concat name (anvil-wl--info-suffix "D")))
         (cur (expand-file-name (concat "cur/" draft-id) drafts))
         (msg (concat
               "Date: " (format-time-string "%a, %d %b %Y %H:%M:%S %z") "\n"
               "From: " from "\n"
               "To: " (or to "") "\n"
               (if (and cc (not (string-empty-p cc))) (concat "Cc: " cc "\n") "")
               "Subject: " (anvil-wl--enc-subject subject) "\n"
               "Message-ID: " mid "\n"
               (if (string-empty-p irt) ""
                 (concat "In-Reply-To: " irt "\n" "References: " irt "\n"))
               "MIME-Version: 1.0\n"
               "Content-Type: text/plain; charset=utf-8\n"
               "Content-Transfer-Encoding: 8bit\n"
               "\n" (or body ""))))
    (dolist (sub '("tmp" "new" "cur"))
      (make-directory (expand-file-name sub drafts) t))
    (let ((coding-system-for-write 'utf-8))
      (write-region msg nil tmp nil 'silent))
    (rename-file tmp cur t)
    (json-encode
     `((draft_id . ,draft-id)
       (message_id . ,mid)
       (from . ,from)
       (to . ,(or to ""))
       (cc . ,(or cc ""))
       (subject . ,(or subject ""))
       (in_reply_to . ,(or in_reply_to ""))
       (folder . ,anvil-wl-drafts-folder)
       (saved . t)
       (sent . :json-false)))))

(defun anvil-wl--save-draft-message (message &optional folder)
  "Save RFC822 MESSAGE under FOLDER's cur/ and return (DRAFT-ID PATH)."
  (let* ((drafts (anvil-wl--folder-dir (or folder anvil-wl-drafts-folder)))
         (name (anvil-wl--unique-name))
         (tmp (expand-file-name (concat "tmp/" name) drafts))
         (draft-id (concat name (anvil-wl--info-suffix "D")))
         (cur (expand-file-name (concat "cur/" draft-id) drafts)))
    (dolist (sub '("tmp" "new" "cur"))
      (make-directory (expand-file-name sub drafts) t))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region message nil tmp nil 'silent))
    (rename-file tmp cur t)
    (list draft-id cur)))

(defun anvil-wl--draft-buffer-rfc822 (&optional body)
  "Return current Wanderlust draft buffer as an RFC822 message string.
BODY, when non-empty, is placed at the top of the RFC822 body."
  (let ((headers '())
        (rest '())
        (last-was-header nil))
    (dolist (line (split-string (buffer-string) "\n"))
      (cond
       ((and (not (string-empty-p (or mail-header-separator "")))
             (string= line mail-header-separator))
        (setq last-was-header nil))
       ((string-match-p "\\`[[:alnum:]-]+:" line)
        (push line headers)
        (setq last-was-header t))
       ((and last-was-header
             (string-match-p "\\`[ \t]" line))
        (push line headers))
       ((not (string-empty-p (string-trim line)))
        (push line rest)
        (setq last-was-header nil))))
    (let ((body-parts (delq nil
                            (list (and body
                                       (not (string-empty-p body))
                                       (string-trim-right body))
                                  (and rest
                                       (string-join (nreverse rest) "\n"))))))
      (setq headers (nreverse headers))
      (unless (seq-some (lambda (line) (string-match-p "\\`Date:" line)) headers)
        (setq headers
              (append headers
                      (list (concat "Date: "
                                    (let ((system-time-locale "C"))
                                      (format-time-string "%a, %d %b %Y %H:%M:%S %z")))))))
      (unless (seq-some (lambda (line) (string-match-p "\\`Message-ID:" line)) headers)
        (setq headers (append headers (list (concat "Message-ID: " (anvil-wl--gen-message-id anvil-wl-from))))))
      (unless (seq-some (lambda (line) (string-match-p "\\`MIME-Version:" line)) headers)
        (setq headers (append headers (list "MIME-Version: 1.0"))))
      (unless (seq-some (lambda (line) (string-match-p "\\`Content-Type:" line)) headers)
        (setq headers (append headers (list "Content-Type: text/plain; charset=utf-8"))))
      (unless (seq-some (lambda (line) (string-match-p "\\`Content-Transfer-Encoding:" line)) headers)
        (setq headers (append headers (list "Content-Transfer-Encoding: 8bit"))))
      (concat (string-join headers "\n")
              "\n\n"
              (string-join body-parts "\n")))))

(defun anvil-wl--insert-draft-body-at-top (body)
  "Insert BODY at the top of the current Wanderlust draft body."
  (when (and body (not (string-empty-p body)))
    (goto-char (point-min))
    (cond
     ((re-search-forward
       (concat "^" (regexp-quote mail-header-separator) "[ \t]*$") nil t)
      (forward-line 1))
     ((search-forward "\n\n" nil t))
     (t (goto-char (point-max)) (insert "\n\n")))
    (insert body)
    (unless (string-suffix-p "\n" body)
      (insert "\n"))))

(defun anvil-wl--yank-reply-citation (path)
  "Insert a Wanderlust citation for the message at PATH into current draft."
  (let* ((headers (anvil-wl--slurp path t))
         (date (anvil-wl--field headers "Date"))
         (from (anvil-wl--decode-hdr (anvil-wl--field headers "From")))
         (body (anvil-wl--decode-body path))
         (citation-buffer (generate-new-buffer " *anvil-wl-reply-citation*")))
    (unwind-protect
        (progn
          (with-current-buffer citation-buffer
            (when (and date (not (string-empty-p date)))
              (insert "Date: " date "\n"))
            (when (and from (not (string-empty-p from)))
              (insert "From: " from "\n"))
            (insert "\n")
            (insert body))
          (goto-char (point-max))
          (let ((wl-mail-reply-buffer citation-buffer)
                (wl-draft-add-references nil)
                (wl-draft-add-in-reply-to nil))
            (wl-draft-yank-from-mail-reply-buffer nil)))
      (when (buffer-live-p citation-buffer)
        (kill-buffer citation-buffer)))))

(defun anvil-wl--tool-reply-draft (message_id &optional folder body with_arg without_citation)
  "Create a Wanderlust reply draft for MESSAGE-ID.  NEVER sends.

MCP Parameters:
  message_id - The Maildir id returned by wl-list-mails/wl-search.
  folder - Optional source folder (default `anvil-wl-inbox-folder').
  body - Optional plain-text body inserted at the top of the reply body.
  with_arg - Optional truthy value; use Wanderlust's alternate reply rules.
  without_citation - Optional truthy value; omit quoted source body."
  (let* ((fld (if (and folder (stringp folder) (not (string-empty-p (string-trim folder))))
                  folder anvil-wl-inbox-folder))
         (entry (anvil-wl--find-by-id message_id fld)))
    (unless entry
      (error "anvil-wl: no message %S in folder %s" message_id fld))
    (require 'wl-draft)
    (let* ((path (nth 0 entry))
           (source (generate-new-buffer " *anvil-wl-reply-source*"))
           (draft-buffer nil))
      (catch 'anvil-wl-reply-result
        (unwind-protect
            (progn
              (with-current-buffer source
                (let ((coding-system-for-read 'utf-8))
                  (insert-file-contents path))
                (goto-char (point-min))
                (while (looking-at-p "[ \t\r]*\n")
                  (delete-region (point) (line-beginning-position 2))))
              (make-directory (expand-file-name "draft" anvil-wl-maildir-root) t)
              (make-directory (expand-file-name "queue" anvil-wl-maildir-root) t)
              (let ((this-command 'wl-summary-reply)
                    (wl-draft-use-frame nil)
                    (wl-draft-reply-buffer-style 'keep)
                    (wl-draft-buffer-style 'keep)
                    (wl-auto-flush-queue nil)
                    (elmo-localdir-folder-path anvil-wl-maildir-root)
                    (wl-from anvil-wl-from)
                    (wl-draft-folder "+draft")
                    (wl-queue-folder "+queue"))
                (wl-draft-reply source (anvil-wl--truthy with_arg) nil nil)
                (setq draft-buffer (current-buffer)))
              (with-current-buffer draft-buffer
                (unless (anvil-wl--truthy without_citation)
                  (anvil-wl--yank-reply-citation path))
                (let* ((message (anvil-wl--draft-buffer-rfc822 body))
                       (saved (anvil-wl--save-draft-message message))
                       (draft-id (nth 0 saved))
                       (draft-path (nth 1 saved)))
                  (throw
                   'anvil-wl-reply-result
                   (json-encode
                    `((draft_id . ,draft-id)
                      (message_id . ,(or (anvil-wl--header-value draft-path "Message-ID") ""))
                      (from . ,(or (anvil-wl--header-value draft-path "From") ""))
                      (to . ,(or (anvil-wl--header-value draft-path "To") ""))
                      (cc . ,(or (anvil-wl--header-value draft-path "Cc") ""))
                      (subject . ,(or (anvil-wl--decode-hdr
                                       (anvil-wl--header-value draft-path "Subject")) ""))
                      (in_reply_to . ,(or (anvil-wl--header-value draft-path "In-Reply-To") ""))
                      (references . ,(or (anvil-wl--header-value draft-path "References") ""))
                      (folder . ,anvil-wl-drafts-folder)
                      (source_folder . ,fld)
                      (source_message_id . ,message_id)
                      (saved . t)
                      (sent . :json-false)))))))
          (when (buffer-live-p source)
            (kill-buffer source))
          (when (buffer-live-p draft-buffer)
            (kill-buffer draft-buffer)))))))

;;;; --- tools: send (gated, irreversible) -----------------------------------

(defun anvil-wl--header-value (path header)
  "Return the unfolded value of HEADER from the RFC822 file at PATH, or nil."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (goto-char (point-min))
    (let ((end (save-excursion
                 (if (re-search-forward "^$" nil t) (point) (point-max)))))
      (goto-char (point-min))
      (when (re-search-forward
             (format "^%s:[ \t]*\\(.*\\(?:\n[ \t].*\\)*\\)" (regexp-quote header))
             end t)
        (string-trim (replace-regexp-in-string "\n[ \t]+" " " (match-string 1)))))))

(defun anvil-wl--extract-emails (s)
  "Return all e-mail addresses in string S, lower-cased, in order."
  (let ((emails '()) (start 0))
    (while (string-match
            "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]\\{2,\\}" (or s "") start)
      (push (downcase (match-string 0 s)) emails)
      (setq start (match-end 0)))
    (nreverse emails)))

(defun anvil-wl--parse-recipients (path)
  "Return the de-duplicated recipient addresses (To/Cc/Bcc) of the file at PATH."
  (let ((emails '()))
    (dolist (h '("To" "Cc" "Bcc"))
      (let ((v (anvil-wl--header-value path h)))
        (when v (setq emails (append emails (anvil-wl--extract-emails v))))))
    (delete-dups emails)))

(defun anvil-wl--recipient-allowed-p (email)
  "Return non-nil when EMAIL matches some `anvil-wl-send-allowlist' regexp."
  (seq-some (lambda (re) (string-match-p re email)) anvil-wl-send-allowlist))

(defun anvil-wl--audit-send (recipients subject path)
  "Append an audit line for a send of SUBJECT to RECIPIENTS (draft PATH)."
  (when (and anvil-wl-send-log (not (string-empty-p anvil-wl-send-log)))
    (let ((line (format "%s\tsent\t%s\t%s\t%s\n"
                        (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                        (string-join recipients ",") (or subject "") path))
          (coding-system-for-write 'utf-8-unix))
      (write-region line nil (expand-file-name anvil-wl-send-log) 'append 'silent))))

(defun anvil-wl--resolve-draft (drafts id)
  "Return the absolute path of draft ID under DRAFTS (cur/ or new/), or nil.
Guards path injection: the resolved file must live inside DRAFTS."
  (let ((root (file-name-as-directory (expand-file-name drafts))))
    (catch 'found
      (dolist (sub '("cur" "new"))
        (let ((p (expand-file-name (concat sub "/" id) drafts)))
          (when (and (string-prefix-p root (expand-file-name p))
                     (file-readable-p p))
            (throw 'found p))))
      nil)))

(defun anvil-wl--smtp-password ()
  "Read the SMTP app password from `anvil-wl-smtp-password-file', else signal."
  (let ((f anvil-wl-smtp-password-file))
    (unless (and f (not (string-empty-p f)) (file-readable-p (expand-file-name f)))
      (error "anvil-wl: set `anvil-wl-smtp-password-file' to a readable app-password file"))
    (with-temp-buffer
      (let ((coding-system-for-read (or (and (coding-system-p 'binary) 'binary)
                                        'no-conversion)))
        (insert-file-contents-literally (expand-file-name f)))
      (replace-regexp-in-string "[ \r\n]" "" (buffer-string)))))

(defun anvil-wl-send-with-tunnel (path recipients)
  "Default `anvil-wl-send-function': send the RFC822 file at PATH to RECIPIENTS.
Uses the pure-Elisp SMTP-over-openssl-tunnel client (`anvil-wl-smtp').  NOTE:
this live transport is intentionally NOT exercised by the test suite (tests
stub `anvil-wl-send-function'); validate it in your own mail environment
before relying on actual delivery."
  (require 'anvil-wl-smtp)
  (let* ((from (or (car (anvil-wl--extract-emails (or anvil-wl-from "")))
                   anvil-wl-smtp-user))
         (msg (with-temp-buffer
                (let ((coding-system-for-read 'utf-8))
                  (insert-file-contents path))
                (buffer-string))))
    (anvil-wl-smtp-send anvil-wl-smtp-host anvil-wl-smtp-port
                        anvil-wl-smtp-user (anvil-wl--smtp-password)
                        from recipients msg)))

(defun anvil-wl--tool-send (draft_id &optional confirm)
  "Send a previously composed draft, subject to safety gates.  Returns JSON.

Sending requires ALL of: `anvil-wl-allow-send' non-nil, every recipient
matching `anvil-wl-send-allowlist', and CONFIRM true.  When CONFIRM is not
true the draft is previewed (recipients + subject) without sending.

MCP Parameters:
  draft_id - The draft_id returned by wl-compose-draft (a name under the
             drafts folder).
  confirm - Boolean; must be true to actually send."
  (let* ((drafts (anvil-wl--folder-dir anvil-wl-drafts-folder))
         (id (string-trim (format "%s" draft_id)))
         (path (anvil-wl--resolve-draft drafts id)))
    (unless path
      (error "anvil-wl: draft %S not found under %s" draft_id drafts))
    (let* ((recipients (anvil-wl--parse-recipients path))
           (subject (anvil-wl--decode-hdr (anvil-wl--header-value path "Subject")))
           (disallowed (seq-remove #'anvil-wl--recipient-allowed-p recipients)))
      (cond
       ((not (anvil-wl--truthy confirm))
        (json-encode `((sent . :json-false)
                       (reason . "confirm=true is required to send")
                       (recipients . ,(vconcat recipients))
                       (subject . ,(or subject "")))))
       ((null recipients)
        (json-encode `((sent . :json-false)
                       (reason . "draft has no recipients (To/Cc/Bcc)"))))
       ((not anvil-wl-allow-send)
        (json-encode `((sent . :json-false)
                       (reason . "sending is disabled; set `anvil-wl-allow-send' to t"))))
       (disallowed
        (json-encode `((sent . :json-false)
                       (reason . "recipient(s) not in `anvil-wl-send-allowlist'")
                       (disallowed . ,(vconcat disallowed)))))
       (t
        (funcall anvil-wl-send-function path recipients)
        (anvil-wl--audit-send recipients subject path)
        (json-encode `((sent . t)
                       (recipients . ,(vconcat recipients))
                       (subject . ,(or subject "")))))))))

;;;; --- module lifecycle -----------------------------------------------------

(defun anvil-wl-enable ()
  "Register the anvil-wl MCP tools."
  (anvil-server-register-tool
   #'anvil-wl--tool-search
   :id "wl-search" :intent '(mail read search) :layer 'workflow :read-only t
   :server-id anvil-wl--server-id
   :description
   "Search the locally-synced Maildir and return structured results.
Decodes RFC2047 headers and MIME bodies, so CJK terms work.

Parameters:
  query - Space-separated terms; a message must contain ALL of them
          (matched against decoded subject/from/to/cc and, by default, body).
  max_results - Optional integer cap (default 50).
  folder - Optional folder name (default INBOX).

Returns JSON: query, folder, count, matched, and a messages array of
{message_id, date, from, to, cc, subject, flags, folder, file}.")

  (anvil-server-register-tool
   #'anvil-wl--tool-list-mails
   :id "wl-list-mails" :intent '(mail read) :layer 'workflow :read-only t
   :server-id anvil-wl--server-id
   :description
   "List recent mail in a Maildir folder (newest first).

Parameters:
  folder - Optional folder name (default INBOX).
  max_results - Optional integer cap (default 50).

Returns the same JSON message shape as wl-search.")

  (anvil-server-register-tool
   #'anvil-wl--tool-read-mail
   :id "wl-read-mail" :intent '(mail read) :layer 'workflow :read-only t
   :server-id anvil-wl--server-id
   :description
   "Read one message in full (decoded headers + plain-text body).

Parameters:
  message_id - The Maildir id from wl-list-mails/wl-search.
  folder - Optional folder name (default INBOX).

Returns JSON: the summary fields plus message_id_header, in_reply_to,
references, and body_plain.")

  (anvil-server-register-tool
   #'anvil-wl--tool-compose-draft
   :id "wl-compose-draft" :intent '(mail compose draft) :layer 'workflow
   :server-id anvil-wl--server-id
   :description
   "Compose a message and save it as a draft.  NEVER sends.

Parameters:
  to - Recipient address(es).
  cc - Optional Cc address(es).
  subject - Optional subject (RFC2047-encoded when non-ASCII).
  body - Optional plain-text (UTF-8) body.
  in_reply_to - Optional Message-ID being replied to.

Returns JSON: draft_id, message_id, from, to, cc, subject, in_reply_to,
folder, saved=true, sent=false.")

  (anvil-server-register-tool
   #'anvil-wl--tool-reply-draft
   :id "wl-reply-draft" :intent '(mail reply compose draft) :layer 'workflow
   :server-id anvil-wl--server-id
   :description
   "Create a Wanderlust reply draft for a local Maildir message.  NEVER sends.
This uses Wanderlust's `wl-draft-reply' recipient and threading logic.

Parameters:
  message_id - The Maildir id from wl-list-mails/wl-search.
  folder - Optional source folder (default INBOX).
  body - Optional plain-text body inserted at the top of the reply body.
  with_arg - Optional truthy value; use Wanderlust's alternate reply rules.
  without_citation - Optional truthy value; omit quoted source body.

Returns JSON: draft_id, message_id, from, to, cc, subject, in_reply_to,
references, source_folder, source_message_id, saved=true, sent=false.")

  (anvil-server-register-tool
   #'anvil-wl--tool-send
   :id "wl-send" :intent '(mail compose send) :layer 'workflow
   :server-id anvil-wl--server-id
   :description
   "Send a previously composed draft, subject to safety gates.

Sending requires ALL of: `anvil-wl-allow-send' non-nil, every recipient
matching `anvil-wl-send-allowlist', and confirm=true.  With confirm
omitted/false the draft is previewed (recipients + subject) without sending.

Parameters:
  draft_id - The draft_id returned by wl-compose-draft.
  confirm - Boolean; must be true to actually send.

Returns JSON: sent (true/false); when false, a reason; when previewing,
the recipients and subject."))

(defun anvil-wl-disable ()
  "Unregister the anvil-wl MCP tools."
  (dolist (id '("wl-search" "wl-list-mails" "wl-read-mail"
                "wl-compose-draft" "wl-reply-draft" "wl-send"))
    (anvil-server-unregister-tool id anvil-wl--server-id)))

(provide 'anvil-wl)
;;; anvil-wl.el ends here

;;; anvil-memory-bridge.el --- Local JSON bridge for anvil-memory  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;;; Commentary:

;; Doc 41 Phase 1 — read-only localhost HTTP API in front of
;; anvil-memory.  This is intentionally separate from the MCP tools:
;; browsers, phones, scripts, and other AI clients can query the same
;; SQLite-backed memory index without speaking MCP-over-stdio.
;; Phase 2a adds guarded write endpoints.  Mutating requests always
;; require `Authorization: Bearer <token>'; read endpoints remain
;; anonymous by default for localhost compatibility unless
;; `anvil-memory-bridge-auth-required' is enabled.
;; Phase 2b records bridge-side memory_event rows for mutating
;; requests and exposes them as text/event-stream snapshots through
;; GET /memory/events?since=<id>.
;; Phase 2c adds an explicit read-replica mode: replicas can pull
;; primary memory events, apply supported DB-direct mutations locally,
;; and reject ordinary local writes to avoid multi-master behavior.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'anvil-memory)
(require 'anvil-worklog)


;;;; --- group + state ------------------------------------------------------

(defgroup anvil-memory-bridge nil
  "Local JSON HTTP bridge for anvil-memory."
  :group 'anvil-memory
  :prefix "anvil-memory-bridge-")

(defconst anvil-memory-bridge-version "0.1.0"
  "Protocol version returned by every bridge endpoint.")

(defcustom anvil-memory-bridge-bind "127.0.0.1"
  "Interface address used by `anvil-memory-bridge-start'."
  :type 'string
  :group 'anvil-memory-bridge)

(defcustom anvil-memory-bridge-port 8730
  "TCP port used by `anvil-memory-bridge-start'."
  :type 'integer
  :group 'anvil-memory-bridge)

(defcustom anvil-memory-bridge-auth-required nil
  "Non-nil requires Bearer auth for every non-health endpoint.
Mutating requests require Bearer auth regardless of this setting."
  :type 'boolean
  :group 'anvil-memory-bridge)

(defcustom anvil-memory-bridge-cors-allowed-origins
  '("https://claude.ai"
    "https://chatgpt.com"
    "https://gemini.google.com"
    "capacitor://localhost"
    "ionic://localhost")
  "Browser origins allowed to call the memory bridge.
Entries are exact origins.  A value of \"*\" allows any origin; values
ending in \"://*\" match all origins with the same scheme prefix, which
is useful for unpacked browser-extension origins."
  :type '(repeat string)
  :group 'anvil-memory-bridge)

(defcustom anvil-memory-bridge-token-file
  (expand-file-name "anvil/memory-bridge.token" user-emacs-directory)
  "File storing the Phase 2a Bearer token."
  :type 'file
  :group 'anvil-memory-bridge)

(defcustom anvil-memory-bridge-events-limit 100
  "Default maximum number of events returned by /memory/events."
  :type 'integer
  :group 'anvil-memory-bridge)

(defcustom anvil-memory-bridge-replica-mode nil
  "Non-nil makes this bridge a read-replica of a primary bridge.
When enabled, ordinary local mutating endpoints are rejected.  Use
`/memory/replica/pull' or `anvil-memory-bridge-replica-pull' to apply
primary events to the local DB."
  :type 'boolean
  :group 'anvil-memory-bridge)

(defcustom anvil-memory-bridge-primary-url nil
  "Primary bridge base URL used by read-replica pull.
Example: http://127.0.0.1:8730.  The URL is used only when
`anvil-memory-bridge-replica-pull' is called without explicit events."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'anvil-memory-bridge)

(defvar anvil-memory-bridge--proc nil
  "Server process for the read-only memory bridge.")

(defvar anvil-memory-bridge--started-at nil
  "Unix epoch at which the current bridge process started.")

(defvar anvil-memory-bridge--token nil
  "Cached Bearer token for write/auth endpoints.")

(defvar anvil-memory-bridge--replica-last-event-id 0
  "Last primary memory_event id applied by this replica.")

(defvar anvil-memory-bridge--current-headers nil
  "Request headers dynamically visible while building one response.")


;;;; --- JSON / HTTP helpers ------------------------------------------------

(defun anvil-memory-bridge--running-p ()
  "Return non-nil when the bridge server process is live."
  (and anvil-memory-bridge--proc
       (process-live-p anvil-memory-bridge--proc)))

(defun anvil-memory-bridge--json-key (keyword)
  "Convert plist KEYWORD into a JSON object key symbol."
  (intern (substring (symbol-name keyword) 1)))

(defun anvil-memory-bridge--json-value (value)
  "Coerce VALUE into a shape `json-encode' serializes predictably."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((keywordp value) (symbol-name value))
   ((symbolp value) (symbol-name value))
   ((vectorp value)
    (vconcat (mapcar #'anvil-memory-bridge--json-value
                     (append value nil))))
   ((and (listp value) (consp (car value)))
    (mapcar (lambda (cell)
              (cons (car cell)
                    (anvil-memory-bridge--json-value (cdr cell))))
            value))
   ((and (listp value) (keywordp (car value)))
    (anvil-memory-bridge--plist->alist value))
   ((listp value)
    (mapcar #'anvil-memory-bridge--json-value value))
   (t value)))

(defun anvil-memory-bridge--plist->alist (plist)
  "Convert PLIST into a JSON-encodable alist."
  (let (out)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (when (keywordp key)
          (push (cons (anvil-memory-bridge--json-key key)
                      (anvil-memory-bridge--json-value value))
                out))))
    (nreverse out)))

(defun anvil-memory-bridge--json (value)
  "Encode VALUE as compact JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode (anvil-memory-bridge--json-value value))))

(defun anvil-memory-bridge--http-response (status content-type body)
  "Return a complete HTTP response with STATUS, CONTENT-TYPE, and BODY."
  (let* ((body (or body ""))
         (bytes (string-bytes body))
         (cors (anvil-memory-bridge--cors-headers
                anvil-memory-bridge--current-headers))
         (reason (pcase status
                   (200 "OK")
                   (204 "No Content")
                   (400 "Bad Request")
                   (401 "Unauthorized")
                   (404 "Not Found")
                   (405 "Method Not Allowed")
                   (409 "Conflict")
                   (500 "Internal Server Error")
                   (_ "OK"))))
    (concat (format "HTTP/1.1 %d %s\r\n" status reason)
            (format "Content-Type: %s\r\n" content-type)
            (format "Content-Length: %d\r\n" bytes)
            (format "Anvil-Memory-Bridge-Version: %s\r\n"
                    anvil-memory-bridge-version)
            cors
            "Connection: close\r\n"
            "Cache-Control: no-store\r\n"
            "\r\n"
            body)))

(defun anvil-memory-bridge--cors-origin-allowed-p (origin)
  "Return non-nil when ORIGIN is allowed by bridge CORS settings."
  (and origin
       (cl-some
        (lambda (allowed)
          (cond
           ((string= allowed "*") t)
           ((string-suffix-p "://*" allowed)
            (string-prefix-p (substring allowed 0 -1) origin))
           (t (string= allowed origin))))
        anvil-memory-bridge-cors-allowed-origins)))

(defun anvil-memory-bridge--cors-headers (headers)
  "Return CORS response headers for request HEADERS."
  (let ((origin (anvil-memory-bridge--header headers "origin")))
    (if (anvil-memory-bridge--cors-origin-allowed-p origin)
        (concat
         (format "Access-Control-Allow-Origin: %s\r\n" origin)
         "Vary: Origin\r\n"
         "Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS\r\n"
         "Access-Control-Allow-Headers: Authorization, Content-Type\r\n"
         "Access-Control-Max-Age: 600\r\n")
      "")))

(defun anvil-memory-bridge--json-response (status value)
  "Return JSON HTTP response for VALUE."
  (anvil-memory-bridge--http-response
   status "application/json; charset=utf-8"
   (anvil-memory-bridge--json value)))

(defun anvil-memory-bridge--error-response (status message)
  "Return JSON error response with STATUS and MESSAGE."
  (anvil-memory-bridge--json-response
   status (list :error message :status status)))

(defun anvil-memory-bridge--sse-response (events)
  "Return text/event-stream response for bridge EVENTS."
  (anvil-memory-bridge--http-response
   200 "text/event-stream; charset=utf-8"
   (mapconcat #'identity events "")))

(defun anvil-memory-bridge--parse-request-line (line)
  "Parse HTTP request LINE into (METHOD . TARGET), or nil."
  (when (and line (string-match
                   "\\`\\([A-Z]+\\) \\([^ ]+\\) HTTP/[0-9.]+\\'" line))
    (cons (match-string 1 line) (match-string 2 line))))

(defun anvil-memory-bridge--decode (value)
  "Decode a query or path VALUE."
  (url-unhex-string (replace-regexp-in-string "+" " " (or value ""))))

(defun anvil-memory-bridge--parse-target (target)
  "Return (:path PATH :query ALIST) parsed from request TARGET."
  (let* ((parts (split-string target "\\?" t))
         (path (car parts))
         (query (mapconcat #'identity (cdr parts) "?"))
         params)
    (when (and query (not (string-empty-p query)))
      (dolist (pair (split-string query "&" t))
        (let* ((kv (split-string pair "="))
               (key (anvil-memory-bridge--decode (car kv)))
               (val (anvil-memory-bridge--decode
                     (mapconcat #'identity (cdr kv) "="))))
          (push (cons key val) params))))
    (list :path path :query (nreverse params))))

(defun anvil-memory-bridge--param (params key)
  "Return first PARAMS value for KEY."
  (cdr (assoc key params)))

(defun anvil-memory-bridge--int-param (params key default)
  "Return integer PARAMS KEY or DEFAULT."
  (let ((raw (anvil-memory-bridge--param params key)))
    (if (and raw (string-match-p "\\`[0-9]+\\'" raw))
        (string-to-number raw)
      default)))

(defun anvil-memory-bridge--bool-param (params key)
  "Return non-nil when PARAMS KEY is truthy."
  (let ((raw (anvil-memory-bridge--param params key)))
    (and raw
         (not (member (downcase raw)
                      '("" "0" "false" "nil" "no" "off"))))))

(defun anvil-memory-bridge--type-param (params)
  "Return optional memory type from PARAMS."
  (let ((raw (anvil-memory-bridge--param params "type")))
    (and raw (not (string-empty-p raw)) (intern raw))))

(defun anvil-memory-bridge--string-param (params key)
  "Return optional non-empty string PARAMS value for KEY."
  (let ((raw (anvil-memory-bridge--param params key)))
    (and raw (not (string-empty-p raw)) raw)))

(defun anvil-memory-bridge--header (headers key)
  "Return case-insensitive HEADERS value for KEY."
  (cdr (assoc (downcase key) headers)))

(defun anvil-memory-bridge--generate-token ()
  "Generate a local bridge token string."
  (secure-hash
   'sha256
   (format "%s:%s:%s:%s"
           (float-time) (random) (user-uid) (system-name))))

(defun anvil-memory-bridge--token ()
  "Return the bridge auth token, creating it if needed."
  (or anvil-memory-bridge--token
      (setq anvil-memory-bridge--token
            (if (file-readable-p anvil-memory-bridge-token-file)
                (string-trim
                 (with-temp-buffer
                   (insert-file-contents-literally
                    anvil-memory-bridge-token-file)
                   (buffer-string)))
              (let ((token (anvil-memory-bridge--generate-token)))
                (make-directory
                 (file-name-directory anvil-memory-bridge-token-file) t)
                (with-temp-file anvil-memory-bridge-token-file
                  (insert token "\n"))
                token)))))

(defun anvil-memory-bridge-rotate-token ()
  "Rotate and return the Phase 2a bridge Bearer token."
  (interactive)
  (setq anvil-memory-bridge--token
        (anvil-memory-bridge--generate-token))
  (make-directory (file-name-directory anvil-memory-bridge-token-file) t)
  (with-temp-file anvil-memory-bridge-token-file
    (insert anvil-memory-bridge--token "\n"))
  (when (called-interactively-p 'interactive)
    (message "anvil-memory-bridge: token rotated"))
  anvil-memory-bridge--token)

(defun anvil-memory-bridge--authorized-p (headers)
  "Return non-nil when HEADERS contain the configured Bearer token."
  (let* ((auth (anvil-memory-bridge--header headers "authorization"))
         (prefix "Bearer "))
    (and auth
         (string-prefix-p prefix auth)
         (string= (substring auth (length prefix))
                  (anvil-memory-bridge--token)))))

(defun anvil-memory-bridge--health-path-p (path)
  "Return non-nil when PATH is an anonymous health endpoint."
  (member path '("/memory/health" "/worklog/health")))

(defun anvil-memory-bridge--mutating-method-p (method)
  "Return non-nil when METHOD mutates bridge state."
  (member method '("POST" "PUT" "DELETE")))

(defun anvil-memory-bridge--auth-required-p (method path)
  "Return non-nil when METHOD/PATH require Bearer auth."
  (and (not (anvil-memory-bridge--health-path-p path))
       (or anvil-memory-bridge-auth-required
           (anvil-memory-bridge--mutating-method-p method))))

(defun anvil-memory-bridge--json-body (body)
  "Parse BODY as a JSON object alist."
  (if (or (null body) (string-empty-p (string-trim body)))
      nil
    (json-parse-string body :object-type 'alist :array-type 'list)))

(defun anvil-memory-bridge--body-get (body key)
  "Return BODY alist value for string KEY."
  (or (cdr (assoc key body))
      (cdr (assoc (intern key) body))))

(defun anvil-memory-bridge--body-string (body key)
  "Return non-empty string value KEY from BODY."
  (let ((v (anvil-memory-bridge--body-get body key)))
    (cond
     ((and (stringp v) (not (string-empty-p v))) v)
     ((null v) nil)
     (t (format "%s" v)))))

(defun anvil-memory-bridge--body-symbol (body key)
  "Return symbol value KEY from BODY."
  (let ((v (anvil-memory-bridge--body-get body key)))
    (cond
     ((symbolp v) v)
     ((and (stringp v) (not (string-empty-p v))) (intern v))
     (t nil))))

(defun anvil-memory-bridge--body-roots (body)
  "Return optional roots list from BODY."
  (let ((roots (anvil-memory-bridge--body-get body "roots")))
    (cond
     ((null roots) nil)
     ((stringp roots) (split-string roots ":" t))
     ((listp roots) roots)
     ((vectorp roots) (append roots nil))
     (t nil))))

(defun anvil-memory-bridge--memory-key-from-path (path)
  "Return decoded memory key from DELETE PATH."
  (and (string-prefix-p "/memory/" path)
       (not (string-prefix-p "/memory/get/" path))
       (let ((key (anvil-memory-bridge--decode
                   (substring path (length "/memory/")))))
         (and (not (string-empty-p key)) key))))

(defun anvil-memory-bridge--delete-memory (key)
  "Delete memory KEY from DB and backing .md file when present."
  (let* ((entry (anvil-memory-get key))
         (file (plist-get entry :file)))
    (unless entry
      (user-error "memory entry not found"))
    (when (and file
               (not (string-prefix-p "anvil-memory:db:" file))
               (file-exists-p file))
      (delete-file file))
    (let ((db (anvil-memory--db)))
      (sqlite-execute db "DELETE FROM memory_meta WHERE file = ?1"
                      (list file))
      (sqlite-execute db "DELETE FROM memory_body_fts WHERE file = ?1"
                      (list file)))
    (list :deleted t :file file :name (plist-get entry :name))))


;;;; --- memory event stream -----------------------------------------------

(defun anvil-memory-bridge--ensure-event-schema ()
  "Ensure the bridge memory_event table exists in the memory DB."
  (let ((db (anvil-memory--db)))
    (sqlite-execute
     db
     "CREATE TABLE IF NOT EXISTS memory_event (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        ts INTEGER NOT NULL,
        op TEXT NOT NULL,
        file TEXT,
        name TEXT,
        type TEXT,
        payload TEXT
      )")
    (sqlite-execute
     db
     "CREATE INDEX IF NOT EXISTS memory_event_ts_idx ON memory_event(ts)")
    db))

(defun anvil-memory-bridge--append-event (op &optional payload)
  "Append a bridge memory event OP with optional PAYLOAD plist.
Returns the stored event plist."
  (let* ((db (anvil-memory-bridge--ensure-event-schema))
         (payload (or payload nil))
         (file (plist-get payload :file))
         (name (plist-get payload :name))
         (type (plist-get payload :type))
         (ts (truncate (float-time)))
         (payload-json (anvil-memory-bridge--json payload)))
    (sqlite-execute
     db
     "INSERT INTO memory_event(ts, op, file, name, type, payload)
      VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
     (list ts op file name
           (cond ((keywordp type) (symbol-name type))
                 ((symbolp type) (symbol-name type))
                 ((stringp type) type)
                 (t nil))
           payload-json))
    (let ((row (car (sqlite-select
                     db
                     "SELECT id, ts, op, file, name, type, payload
                        FROM memory_event
                       WHERE rowid = last_insert_rowid()"))))
      (anvil-memory-bridge--event-row row))))

(defun anvil-memory-bridge--event-row (row)
  "Convert memory_event ROW into a plist."
  (let ((payload (nth 6 row)))
    (list :id (nth 0 row)
          :ts (nth 1 row)
          :op (nth 2 row)
          :file (nth 3 row)
          :name (nth 4 row)
          :type (nth 5 row)
          :payload (and payload
                        (not (string-empty-p payload))
                        (json-parse-string payload
                                           :object-type 'alist
                                           :array-type 'list)))))

(defun anvil-memory-bridge--events-since (since limit)
  "Return memory events newer than SINCE, capped by LIMIT."
  (let* ((db (anvil-memory-bridge--ensure-event-schema))
         (limit (max 1 (min 1000 (or limit
                                      anvil-memory-bridge-events-limit))))
         (since (max 0 (or since 0))))
    (mapcar
     #'anvil-memory-bridge--event-row
     (sqlite-select
      db
      "SELECT id, ts, op, file, name, type, payload
         FROM memory_event
        WHERE id > ?1
        ORDER BY id ASC
        LIMIT ?2"
      (list since limit)))))

(defun anvil-memory-bridge--event-count ()
  "Return the number of bridge memory events."
  (let* ((db (anvil-memory-bridge--ensure-event-schema))
         (rows (sqlite-select db "SELECT COUNT(*) FROM memory_event")))
    (or (caar rows) 0)))

(defun anvil-memory-bridge--sse-event (event)
  "Encode EVENT plist as one SSE event block."
  (format "id: %s\nevent: memory.%s\ndata: %s\n\n"
          (plist-get event :id)
          (or (plist-get event :op) "event")
          (anvil-memory-bridge--json event)))


;;;; --- read replicas ------------------------------------------------------

(defun anvil-memory-bridge--replica-status ()
  "Return the Phase 2c read-replica status payload."
  (list :mode (if anvil-memory-bridge-replica-mode "replica" "primary")
        :replica-mode (and anvil-memory-bridge-replica-mode t)
        :primary-url (or anvil-memory-bridge-primary-url "")
        :last-event-id anvil-memory-bridge--replica-last-event-id
        :local-writes-allowed (not anvil-memory-bridge-replica-mode)
        :version anvil-memory-bridge-version))

(defun anvil-memory-bridge--event-id (event)
  "Return numeric id from decoded EVENT."
  (let ((id (or (plist-get event :id)
                (cdr (assq 'id event))
                (cdr (assoc "id" event)))))
    (cond ((integerp id) id)
          ((and (stringp id) (string-match-p "\\`[0-9]+\\'" id))
           (string-to-number id))
          (t 0))))

(defun anvil-memory-bridge--event-op (event)
  "Return operation string from decoded EVENT."
  (let ((op (or (plist-get event :op)
                (cdr (assq 'op event))
                (cdr (assoc "op" event)))))
    (cond ((symbolp op) (symbol-name op))
          ((stringp op) op)
          (t ""))))

(defun anvil-memory-bridge--event-payload (event)
  "Return decoded EVENT payload as an alist/plist."
  (or (plist-get event :payload)
      (cdr (assq 'payload event))
      (cdr (assoc "payload" event))))

(defun anvil-memory-bridge--payload-get (payload key)
  "Return KEY from event PAYLOAD, accepting plist or JSON alist."
  (or (and (listp payload)
           (plist-get payload (intern (concat ":" key))))
      (and (listp payload)
           (cdr (assoc key payload)))
      (and (listp payload)
           (cdr (assq (intern key) payload)))))

(defun anvil-memory-bridge--payload-symbol (payload key)
  "Return symbol value KEY from event PAYLOAD."
  (let ((value (anvil-memory-bridge--payload-get payload key)))
    (cond ((symbolp value) value)
          ((and (stringp value) (not (string-empty-p value)))
           (intern value))
          (t nil))))

(defun anvil-memory-bridge--payload-tags (payload)
  "Return tags list from event PAYLOAD."
  (let ((tags (anvil-memory-bridge--payload-get payload "tags")))
    (cond ((null tags) nil)
          ((stringp tags) (split-string tags "," t "[ \t\n\r]+"))
          ((vectorp tags) (append tags nil))
          ((listp tags) tags)
          (t nil))))

(defun anvil-memory-bridge--replica-delete-db-direct (name)
  "Delete DB-direct memory NAME when present."
  (let* ((db (anvil-memory--db))
         (basename (file-name-sans-extension
                    (file-name-nondirectory name)))
         (file (if (string-prefix-p "anvil-memory:db:" basename)
                   basename
                 (concat anvil-memory--synthetic-prefix basename))))
    (sqlite-execute db "DELETE FROM memory_meta WHERE file = ?1"
                    (list file))
    (sqlite-execute db "DELETE FROM memory_body_fts WHERE file = ?1"
                    (list file))))

(defun anvil-memory-bridge--replica-apply-save (payload)
  "Apply one primary save event PAYLOAD to the local DB."
  (let* ((name (or (anvil-memory-bridge--payload-get payload "name")
                   (anvil-memory-bridge--payload-get payload "file")))
         (type (anvil-memory-bridge--payload-symbol payload "type"))
         (body (anvil-memory-bridge--payload-get payload "body"))
         (description (anvil-memory-bridge--payload-get payload
                                                        "description"))
         (created (anvil-memory-bridge--payload-get payload "created"))
         (ttl (anvil-memory-bridge--payload-symbol payload "ttl-policy"))
         (tags (anvil-memory-bridge--payload-tags payload)))
    (unless (and name type (stringp body))
      (user-error "replica save event missing name/type/body"))
    (anvil-memory-bridge--replica-delete-db-direct name)
    (anvil-memory-add name type body
                      :description description
                      :created created
                      :tags tags
                      :ttl-policy ttl)))

(defun anvil-memory-bridge--replica-apply-delete (payload)
  "Apply one primary delete event PAYLOAD to the local DB."
  (let ((name (or (anvil-memory-bridge--payload-get payload "name")
                  (anvil-memory-bridge--payload-get payload "file"))))
    (when name
      (anvil-memory-bridge--replica-delete-db-direct name))
    (list :deleted (and name t) :name name)))

(defun anvil-memory-bridge--replica-apply-event (event)
  "Apply one primary bridge EVENT to this local replica."
  (let* ((op (anvil-memory-bridge--event-op event))
         (payload (anvil-memory-bridge--event-payload event))
         (id (anvil-memory-bridge--event-id event))
         result)
    (setq result
          (pcase op
            ("save" (anvil-memory-bridge--replica-apply-save payload))
            ("delete" (anvil-memory-bridge--replica-apply-delete payload))
            (_ (list :skipped t :op op))))
    (setq anvil-memory-bridge--replica-last-event-id
          (max anvil-memory-bridge--replica-last-event-id id))
    (list :id id :op op :result result)))

(defun anvil-memory-bridge--parse-sse-events (text)
  "Parse SSE TEXT and return decoded memory event JSON objects."
  (let (events)
    (dolist (block (split-string (or text "") "\n\n" t))
      (let (data-lines)
        (dolist (line (split-string block "\n"))
          (when (string-prefix-p "data: " line)
            (push (substring line 6) data-lines)))
        (when data-lines
          (push (json-parse-string
                 (mapconcat #'identity (nreverse data-lines) "\n")
                 :object-type 'alist
                 :array-type 'list)
                events))))
    (nreverse events)))

(defun anvil-memory-bridge--fetch-primary-events (since)
  "Fetch primary SSE events newer than SINCE."
  (unless (and anvil-memory-bridge-primary-url
               (not (string-empty-p anvil-memory-bridge-primary-url)))
    (user-error "replica pull requires anvil-memory-bridge-primary-url"))
  (require 'url)
  (let* ((url (format "%s/memory/events?since=%s&limit=%s"
                      (string-remove-suffix "/"
                                            anvil-memory-bridge-primary-url)
                      since
                      anvil-memory-bridge-events-limit))
         (buffer (url-retrieve-synchronously url t t 30)))
    (unless buffer
      (user-error "replica pull failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (search-forward "\r\n\r\n" nil t)
              (anvil-memory-bridge--parse-sse-events
               (buffer-substring-no-properties (point) (point-max)))
            nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun anvil-memory-bridge-replica-pull (&optional events since)
  "Pull and apply primary memory EVENTS to this read-replica.
EVENTS may be nil, a decoded event list, or an SSE text string.  When
EVENTS is nil, fetch from `anvil-memory-bridge-primary-url'.  SINCE
defaults to `anvil-memory-bridge--replica-last-event-id'."
  (let* ((since* (or since anvil-memory-bridge--replica-last-event-id))
         (events* (cond
                   ((null events)
                    (anvil-memory-bridge--fetch-primary-events since*))
                   ((stringp events)
                    (anvil-memory-bridge--parse-sse-events events))
                   ((vectorp events) (append events nil))
                   ((listp events) events)
                   (t (user-error "events must be nil, SSE text, or list"))))
         applied)
    (dolist (event events*)
      (when (> (anvil-memory-bridge--event-id event) since*)
        (push (anvil-memory-bridge--replica-apply-event event) applied)))
    (setq applied (nreverse applied))
    (list :replica-mode (and anvil-memory-bridge-replica-mode t)
          :primary-url (or anvil-memory-bridge-primary-url "")
          :since since*
          :applied (length applied)
          :last-event-id anvil-memory-bridge--replica-last-event-id
          :events (apply #'vector applied))))


;;;; --- endpoint dispatch --------------------------------------------------

(defun anvil-memory-bridge--health ()
  "Return the bridge health payload."
  (list :db (anvil-memory-effective-db-path)
        :rows (length (anvil-memory-list))
        :events (anvil-memory-bridge--event-count)
        :uptime-sec (if anvil-memory-bridge--started-at
                        (- (truncate (float-time))
                           anvil-memory-bridge--started-at)
                      0)
        :version anvil-memory-bridge-version))

(defun anvil-memory-bridge--dispatch-get (path params)
  "Dispatch read-only GET request PATH with query PARAMS."
  (cond
   ((equal path "/memory/health")
    (anvil-memory-bridge--json-response 200 (anvil-memory-bridge--health)))
   ((equal path "/memory/effective-db-path")
    (anvil-memory-bridge--json-response
     200 (list :db (anvil-memory-effective-db-path))))
   ((equal path "/memory/events")
    (let* ((since (anvil-memory-bridge--int-param params "since" 0))
           (limit (anvil-memory-bridge--int-param
                   params "limit" anvil-memory-bridge-events-limit))
           (events (anvil-memory-bridge--events-since since limit)))
      (anvil-memory-bridge--sse-response
       (append (list (format ": anvil-memory-bridge events since %s\n\n"
                             since))
               (mapcar #'anvil-memory-bridge--sse-event events)))))
   ((equal path "/memory/replica/status")
    (anvil-memory-bridge--json-response
     200 (anvil-memory-bridge--replica-status)))
   ((equal path "/memory/search")
    (let ((query (or (anvil-memory-bridge--param params "q") ""))
          (type (anvil-memory-bridge--type-param params))
          (limit (anvil-memory-bridge--int-param params "limit" 20)))
      (anvil-memory-bridge--json-response
       200 (apply #'vector
                  (anvil-memory-search query :type type :limit limit)))))
   ((equal path "/memory/list")
    (let ((type (anvil-memory-bridge--type-param params))
          (with-decay (anvil-memory-bridge--bool-param params "with_decay"))
          (sort-raw (anvil-memory-bridge--param params "sort")))
      (anvil-memory-bridge--json-response
       200
       (apply #'vector
              (anvil-memory-list
               type
               :with-decay with-decay
               :sort (and sort-raw (not (string-empty-p sort-raw))
                          (intern sort-raw)))))))
   ((string-prefix-p "/memory/get/" path)
    (let* ((key (anvil-memory-bridge--decode
                 (substring path (length "/memory/get/"))))
           (entry (and (not (string-empty-p key))
                       (anvil-memory-get key))))
      (if entry
          (anvil-memory-bridge--json-response 200 entry)
        (anvil-memory-bridge--error-response 404 "memory entry not found"))))
   ((equal path "/worklog/health")
    (anvil-memory-bridge--json-response
     200 (list :db (anvil-worklog-effective-db-path)
               :rows (length (anvil-worklog-list :limit 100000))
               :version anvil-memory-bridge-version)))
   ((equal path "/worklog/effective-db-path")
    (anvil-memory-bridge--json-response
     200 (list :db (anvil-worklog-effective-db-path))))
   ((equal path "/worklog/search")
    (let ((query (or (anvil-memory-bridge--param params "q") ""))
          (limit (anvil-memory-bridge--int-param params "limit" 20)))
      (anvil-memory-bridge--json-response
       200
       (apply #'vector
              (anvil-worklog-search
               query
               :limit limit
               :machine (anvil-memory-bridge--string-param params "machine")
               :since (anvil-memory-bridge--string-param params "since")
               :until (anvil-memory-bridge--string-param params "until")
               :year (anvil-memory-bridge--int-param params "year" nil))))))
   ((equal path "/worklog/list")
    (let ((limit (anvil-memory-bridge--int-param params "limit" 20)))
      (anvil-memory-bridge--json-response
       200
       (apply #'vector
              (anvil-worklog-list
               :limit limit
               :machine (anvil-memory-bridge--string-param params "machine")
               :since (anvil-memory-bridge--string-param params "since")
               :until (anvil-memory-bridge--string-param params "until")
               :year (anvil-memory-bridge--int-param params "year" nil))))))
   ((equal path "/worklog/get")
    (let* ((digest (anvil-memory-bridge--string-param params "digest"))
           (file (anvil-memory-bridge--string-param params "file"))
           (start-line (anvil-memory-bridge--int-param
                        params "start_line" nil))
           (entry (if digest
                      (anvil-worklog-get-by-digest digest)
                    (and file start-line
                         (anvil-worklog-get file start-line)))))
      (if entry
          (anvil-memory-bridge--json-response 200 entry)
        (anvil-memory-bridge--error-response 404 "worklog entry not found"))))
   (t
    (anvil-memory-bridge--error-response
     404 (format "unknown endpoint: %s" path)))))

(defun anvil-memory-bridge--dispatch-write (method path body)
  "Dispatch mutating METHOD request PATH with JSON BODY alist."
  (if (and anvil-memory-bridge-replica-mode
           (not (and (equal method "POST")
                     (equal path "/memory/replica/pull"))))
      (anvil-memory-bridge--error-response
       409 "replica mode rejects local writes; write to primary")
    (pcase method
    ("POST"
     (cond
      ((equal path "/memory/replica/pull")
       (let* ((events (anvil-memory-bridge--body-get body "events"))
              (sse (anvil-memory-bridge--body-string body "sse"))
              (since (anvil-memory-bridge--body-get body "since"))
              (pulled (anvil-memory-bridge-replica-pull
                       (or events sse)
                       (and (integerp since) since))))
         (anvil-memory-bridge--json-response 200 pulled)))
      ((equal path "/memory/save")
       (let* ((name (or (anvil-memory-bridge--body-string body "name")
                        (anvil-memory-bridge--body-string body "file")))
              (type (anvil-memory-bridge--body-symbol body "type"))
              (body-text (anvil-memory-bridge--body-string body "body"))
              (ttl (anvil-memory-bridge--body-symbol body "ttl_policy"))
              (description
               (anvil-memory-bridge--body-string body "description"))
              (tags (anvil-memory-bridge--body-get body "tags")))
         (unless name
           (user-error "memory/save requires name or file"))
         (unless type
           (user-error "memory/save requires type"))
         (unless body-text
           (user-error "memory/save requires body"))
         (let* ((row (anvil-memory-add
                      (file-name-sans-extension (file-name-nondirectory name))
                      type body-text
                      :description description
                      :tags tags
                      :ttl-policy ttl))
                (event (anvil-memory-bridge--append-event
                        "save"
                        (append row
                                (list :body body-text
                                      :tags tags
                                      :ttl-policy ttl)))))
           (anvil-memory-bridge--json-response
            200 (append row (list :event-id (plist-get event :id)))))))
      ((equal path "/memory/scan")
       (let* ((roots (anvil-memory-bridge--body-roots body))
              (n (anvil-memory-scan roots))
              (event (anvil-memory-bridge--append-event
                      "scan" (list :roots roots :count n))))
         (anvil-memory-bridge--json-response
         200 (list :scanned n :event-id (plist-get event :id)))))
      ((equal path "/memory/prune")
       (let* ((roots (anvil-memory-bridge--body-roots body))
              (n (anvil-memory-prune roots))
              (event (anvil-memory-bridge--append-event
                      "prune" (list :roots roots :count n))))
         (anvil-memory-bridge--json-response
          200 (list :pruned n :event-id (plist-get event :id)))))
      (t
       (anvil-memory-bridge--error-response
        404 (format "unknown endpoint: %s" path)))))
    ("PUT"
     (cond
      ((equal path "/memory/access")
       (let* ((key (or (anvil-memory-bridge--body-string body "file")
                       (anvil-memory-bridge--body-string body "name")))
              (entry (and key
                          (anvil-memory-get key :bump-access t))))
         (let* ((payload
                 (if entry
                     (list :file (plist-get entry :file)
                           :name (plist-get entry :name)
                           :found t
                           :access-count
                           (plist-get (anvil-memory-get
                                       (plist-get entry :file))
                                      :access-count))
                   (list :file key :found nil :access-count nil)))
                (event (anvil-memory-bridge--append-event
                        "access" payload)))
           (anvil-memory-bridge--json-response
            200 (append payload (list :event-id (plist-get event :id)))))))
      (t
       (anvil-memory-bridge--error-response
        404 (format "unknown endpoint: %s" path)))))
    ("DELETE"
     (if-let ((key (anvil-memory-bridge--memory-key-from-path path)))
         (let* ((payload (anvil-memory-bridge--delete-memory key))
                (event (anvil-memory-bridge--append-event
                        "delete" payload)))
           (anvil-memory-bridge--json-response
            200 (append payload (list :event-id (plist-get event :id)))))
       (anvil-memory-bridge--error-response
        404 (format "unknown endpoint: %s" path))))
    (_
     (anvil-memory-bridge--error-response
      405 "unsupported mutating method")))))

(defun anvil-memory-bridge--dispatch (method target &optional headers body)
  "Dispatch HTTP METHOD and TARGET into a complete response string."
  (condition-case err
      (let* ((parsed (anvil-memory-bridge--parse-target target))
             (path (plist-get parsed :path))
             (params (plist-get parsed :query))
             (anvil-memory-bridge--current-headers headers))
        (cond
         ((and (anvil-memory-bridge--auth-required-p method path)
               (not (anvil-memory-bridge--authorized-p headers)))
          (anvil-memory-bridge--error-response
           401 "missing or invalid bearer token"))
         ((equal method "GET")
          (anvil-memory-bridge--dispatch-get path params))
         ((equal method "OPTIONS")
          (anvil-memory-bridge--http-response 204 "text/plain" ""))
         ((anvil-memory-bridge--mutating-method-p method)
          (anvil-memory-bridge--dispatch-write
           method path (anvil-memory-bridge--json-body body)))
         (t
          (anvil-memory-bridge--error-response
           405 "unsupported method"))))
    (error
     (anvil-memory-bridge--error-response
      500 (error-message-string err)))))

(defun anvil-memory-bridge--parse-headers (lines)
  "Parse HTTP header LINES into a lowercase-key alist."
  (let (headers)
    (dolist (line lines)
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
        (push (cons (downcase (match-string 1 line))
                    (string-trim (match-string 2 line)))
              headers)))
    (nreverse headers)))

(defun anvil-memory-bridge--handle-buffer (buf)
  "Parse accumulated request BUF and return a response, or nil if incomplete."
  (let ((split (string-match "\r\n\r\n" buf)))
    (when split
      (let* ((head (substring buf 0 split))
             (lines (split-string head "\r\n" t))
             (parsed (anvil-memory-bridge--parse-request-line (car lines)))
             (headers (anvil-memory-bridge--parse-headers (cdr lines)))
             (content-length
              (string-to-number
               (or (anvil-memory-bridge--header
                    headers "content-length")
                   "0")))
             (body-start (+ split 4)))
        (if parsed
            (when (<= (+ body-start content-length) (length buf))
              (anvil-memory-bridge--dispatch
               (car parsed)
               (cdr parsed)
               headers
               (substring buf body-start (+ body-start content-length))))
          (anvil-memory-bridge--error-response 400 "bad request"))))))

(defun anvil-memory-bridge--filter (proc data)
  "Per-connection filter that responds once a request is complete."
  (let ((buf (concat (or (process-get proc :request-buffer) "") data)))
    (process-put proc :request-buffer buf)
    (let ((response (anvil-memory-bridge--handle-buffer buf)))
      (when response
        (ignore-errors (process-send-string proc response))
        (ignore-errors (delete-process proc))))))

(defun anvil-memory-bridge--sentinel (_proc _event)
  "Accepted connection sentinel.  The filter owns teardown."
  nil)


;;;; --- lifecycle ----------------------------------------------------------

;;;###autoload
(cl-defun anvil-memory-bridge-start (&key host port)
  "Start the read-only memory bridge and return (:host :port :process).
HOST defaults to `anvil-memory-bridge-bind'.  PORT defaults to
`anvil-memory-bridge-port'; pass t or 0 to request an ephemeral port."
  (interactive)
  (when (anvil-memory-bridge--running-p)
    (user-error
     "anvil-memory-bridge: already running on %s:%s"
     anvil-memory-bridge-bind anvil-memory-bridge-port))
  (let* ((h (or host anvil-memory-bridge-bind))
         (p (cond ((eq port t) t)
                  ((and (integerp port) (zerop port)) t)
                  (port port)
                  (t anvil-memory-bridge-port)))
         (proc (make-network-process
                :name "anvil-memory-bridge"
                :server t
                :host h
                :service p
                :family 'ipv4
                :coding 'binary
                :noquery t
                :filter #'anvil-memory-bridge--filter
                :sentinel #'anvil-memory-bridge--sentinel))
         (assigned (process-contact proc :service)))
    (setq anvil-memory-bridge--proc proc
          anvil-memory-bridge-bind h
          anvil-memory-bridge-port (if (integerp assigned) assigned p)
          anvil-memory-bridge--started-at (truncate (float-time)))
    (list :host h
          :port anvil-memory-bridge-port
          :process proc)))

;;;###autoload
(defun anvil-memory-bridge-stop ()
  "Stop the running memory bridge, if any.
Returns the previous port, or nil when the bridge was already idle."
  (interactive)
  (let ((was (and (anvil-memory-bridge--running-p)
                  anvil-memory-bridge-port)))
    (when (anvil-memory-bridge--running-p)
      (ignore-errors (delete-process anvil-memory-bridge--proc)))
    (setq anvil-memory-bridge--proc nil
          anvil-memory-bridge--started-at nil)
    was))

;;;###autoload
(defun anvil-memory-bridge-status ()
  "Return and display the current bridge status."
  (interactive)
  (let ((status (list :running (and (anvil-memory-bridge--running-p) t)
                      :host anvil-memory-bridge-bind
                      :port anvil-memory-bridge-port
                      :version anvil-memory-bridge-version)))
    (when (called-interactively-p 'interactive)
      (message "anvil-memory-bridge: %s"
               (if (plist-get status :running)
                   (format "running on %s:%s"
                           (plist-get status :host)
                           (plist-get status :port))
                 "stopped")))
    status))

(defun anvil-memory-bridge-enable ()
  "Load the bridge module without starting the HTTP listener."
  t)

(defun anvil-memory-bridge-disable ()
  "Stop the bridge listener when the module is unloaded."
  (anvil-memory-bridge-stop))

(provide 'anvil-memory-bridge)

;;; anvil-memory-bridge.el ends here

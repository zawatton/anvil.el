;;; anvil-org-watch.el --- Org TODO state-change signal channel -*- lexical-binding: t; -*-

;;; Commentary:

;; Per-Emacs-session, in-memory broker that emits one NDJSON line per
;; TODO state transition into a per-subscriber transport file under
;; `anvil-org-watch-dir'.
;;
;; Subscribers register a UUID watch-set via the `org-watch-register'
;; MCP tool and tail the returned transport file (e.g. with
;; `tail -F -n +1 <path>') — no polling from the client side.  The
;; hook fires for every writer path: this MCP server's own tools,
;; interactive `C-c C-t', `org-agenda', and programmatic `org-todo'.
;; The handler short-circuits when the buffer is not in
;; `anvil-org-allowed-files', when the heading carries no `:ID:'
;; property, or when no subscriber is watching that id, so filtering
;; is server-side and unwatched transitions cost one hash lookup.
;;
;; Each event line is a JSON object with six fields:
;;   uuid, file, headline, previous_state, new_state, timestamp (UTC).
;;
;; Intended use: a coordinating agent (or any external process) hands
;; work items to humans or other agents as org TODOs and needs to
;; react when their state changes, without polling the org files.
;;
;; Enable via `anvil-optional-modules':
;;   (add-to-list 'anvil-optional-modules 'org-watch)
;; or call `anvil-org-watch-enable' directly after `anvil-org'.

;;; Code:

(require 'json)
(require 'url-util)
(require 'org)
(require 'org-id)
(require 'anvil-org)
(require 'anvil-server)

(defcustom anvil-org-watch-dir
  (expand-file-name "anvil-org-watch/" temporary-file-directory)
  "Directory holding per-subscriber NDJSON transport files.
Created lazily with mode 0700 on first use."
  :type 'directory
  :group 'anvil-org)

(defvar anvil-org-watch--subscribers (make-hash-table :test 'equal)
  "Subscriber table for the org-state-change signal channel.
Maps subscriber-id (string) → plist with
  :watch-set      hash-table of uuid (string) → t
  :transport-path absolute path to per-subscriber NDJSON file")

(defun anvil-org-watch--ensure-dir ()
  "Create `anvil-org-watch-dir' with mode 0700 if absent."
  (unless (file-directory-p anvil-org-watch-dir)
    (make-directory anvil-org-watch-dir t)
    (set-file-modes anvil-org-watch-dir #o700)))

(defun anvil-org-watch--transport-path (subscriber-id)
  "Return the transport file path for SUBSCRIBER-ID."
  (expand-file-name
   (concat (url-hexify-string subscriber-id) ".ndjson")
   anvil-org-watch-dir))

(defun anvil-org-watch--file-monitored-p (path)
  "Return non-nil if PATH is currently open by any process.
When lsof(1) is unavailable the file is conservatively treated as
monitored, so stale subscribers are never reaped by mistake."
  (and (file-exists-p path)
       (if (executable-find "lsof")
           (= 0 (call-process "lsof" nil nil nil "-t" path))
         t)))

(defun anvil-org-watch--reap-stale ()
  "Drop subscribers whose transport file has no active monitor.
Also deletes their transport files."
  (let ((stale '()))
    (maphash
     (lambda (sid entry)
       (let ((path (plist-get entry :transport-path)))
         (unless (anvil-org-watch--file-monitored-p path)
           (push sid stale))))
     anvil-org-watch--subscribers)
    (dolist (sid stale)
      (let* ((entry (gethash sid anvil-org-watch--subscribers))
             (path (and entry (plist-get entry :transport-path))))
        (when (and path (file-exists-p path))
          (ignore-errors (delete-file path)))
        (remhash sid anvil-org-watch--subscribers)))))

(defun anvil-org-watch--build-set (watch-uuids)
  "Build a hash-set from WATCH-UUIDS (vector / list / nil)."
  (let ((set (make-hash-table :test 'equal)))
    (cond
     ((null watch-uuids) nil)
     ((vectorp watch-uuids)
      (mapc (lambda (u) (puthash u t set)) (append watch-uuids nil)))
     ((listp watch-uuids)
      (mapc (lambda (u) (puthash u t set)) watch-uuids))
     (t
      (anvil-org--tool-validation-error
       "Invalid watch_uuids format (expected array): %S" watch-uuids)))
    (maphash
     (lambda (u _v)
       (unless (stringp u)
         (anvil-org--tool-validation-error
          "watch_uuids entries must be strings, got: %S" u)))
     set)
    set))

(defun anvil-org-watch--iso8601-utc (&optional time)
  "Return ISO-8601 UTC timestamp for TIME (or current time)."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (or time (current-time)) t))

(defun anvil-org-watch--append-line (path line)
  "Append LINE (with trailing newline) to PATH atomically.
PATH's directory must exist."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat line "\n") nil path 'append 'silent)))

(defun anvil-org-watch--emit (payload-alist uuid)
  "Append one JSON line per matching subscriber for the event on UUID.
PAYLOAD-ALIST is the event payload."
  (let ((line (json-encode payload-alist)))
    (maphash
     (lambda (_sid entry)
       (let ((set (plist-get entry :watch-set))
             (path (plist-get entry :transport-path)))
         (when (and set path (gethash uuid set))
           (condition-case _err
               (anvil-org-watch--append-line path line)
             (error nil)))))
     anvil-org-watch--subscribers)))

(defun anvil-org-watch--allowed-buffer-p ()
  "Non-nil when the current buffer visits a file in the allowed list."
  (when-let* ((path (buffer-file-name)))
    (anvil-org--find-allowed-file path)))

(defun anvil-org-watch--state-change-handler ()
  "Hook function for `org-after-todo-state-change-hook'.
Emits one NDJSON line per matching subscriber to its transport file.
Short-circuits when the buffer is not in `anvil-org-allowed-files',
when the heading carries no `:ID:' property, or when no subscriber
is watching that id."
  (condition-case _err
      (when (anvil-org-watch--allowed-buffer-p)
        (when-let* ((id (org-entry-get nil "ID")))
          (let* ((headline (or (org-get-heading t t t t) ""))
                 (previous (or (bound-and-true-p org-last-state) ""))
                 (new (or (bound-and-true-p org-state) ""))
                 (payload
                  `((uuid . ,id)
                    (file . ,(buffer-file-name))
                    (headline . ,headline)
                    (previous_state . ,(or previous ""))
                    (new_state . ,(or new ""))
                    (timestamp . ,(anvil-org-watch--iso8601-utc)))))
            (anvil-org-watch--emit payload id))))
    (error nil)))

(defun anvil-org-watch--tool-register (subscriber_id watch_uuids)
  "Register SUBSCRIBER_ID with WATCH_UUIDS watch-set.
Idempotent: replaces any existing entry and truncates the transport file.

MCP Parameters:
  subscriber_id - Caller-generated UUID identifying this subscriber
  watch_uuids - Array of heading UUIDs to watch (may be empty)"
  (anvil-server-with-error-handling
    (unless (and (stringp subscriber_id) (length> subscriber_id 0))
      (anvil-org--tool-validation-error
       "subscriber_id must be a non-empty string"))
    (anvil-org-watch--ensure-dir)
    (anvil-org-watch--reap-stale)
    (let* ((set (anvil-org-watch--build-set watch_uuids))
           (path (anvil-org-watch--transport-path subscriber_id)))
      (when (file-exists-p path)
        (delete-file path))
      (anvil-org-watch--append-line path "")
      ;; `write-region' with empty string + append leaves the file at
      ;; exactly one newline; truncate it back to zero bytes.
      (write-region "" nil path nil 'silent)
      (puthash subscriber_id
               (list :watch-set set
                     :transport-path path)
               anvil-org-watch--subscribers)
      (json-encode
       `((ok . t)
         (subscriber_id . ,subscriber_id)
         (watch_count . ,(if set (hash-table-count set) 0))
         (transport_path . ,path))))))

(defun anvil-org-watch--tool-unregister (subscriber_id)
  "Unregister SUBSCRIBER_ID and delete its transport file.
Idempotent: a second call returns ok without error.

MCP Parameters:
  subscriber_id - Caller-generated UUID identifying this subscriber"
  (anvil-server-with-error-handling
    (unless (and (stringp subscriber_id) (length> subscriber_id 0))
      (anvil-org--tool-validation-error
       "subscriber_id must be a non-empty string"))
    (let* ((entry (gethash subscriber_id anvil-org-watch--subscribers))
           (path (or (and entry (plist-get entry :transport-path))
                     (anvil-org-watch--transport-path subscriber_id))))
      (when (and path (file-exists-p path))
        (ignore-errors (delete-file path)))
      (remhash subscriber_id anvil-org-watch--subscribers)
      (json-encode `((ok . t))))))

;;;###autoload
(defun anvil-org-watch-enable ()
  "Install the org-state-change signal channel.
Idempotent: hooks/tool registrations are added at most once."
  (anvil-server-register-tool
   #'anvil-org-watch--tool-register
   :id "org-watch-register"
   :intent '(org-watch)
   :layer 'workflow
   :description
   "Register a subscriber on the org-state-change signal channel.

The channel emits one NDJSON line per TODO state transition on any
heading whose `:ID:' property matches the subscriber's watch-set, for
any writer path (this MCP server, interactive `C-c C-t', `org-agenda',
or programmatic `org-todo').  Filtering is server-side: nothing is
written to the transport file for UUIDs the subscriber is not
watching.

Idempotent: re-registering the same `subscriber_id' replaces the
watch-set and truncates the transport file (so a resuming subscriber
starts from a clean cursor and never replays historical events).

Parameters:
  subscriber_id - Caller-generated UUID identifying this subscriber
                  (string, required, non-empty).  Every call from the
                  same logical client must reuse the same id.
  watch_uuids - Array of heading UUIDs to watch (required; may be the
                empty array).  Each entry must be a string.

Returns JSON object:
  ok - Always true on success (boolean)
  subscriber_id - The id that was registered (string)
  watch_count - Number of UUIDs in the watch-set (integer)
  transport_path - Absolute path to the per-subscriber NDJSON file
                   the caller should tail (e.g. with
                   `tail -F -n +1 <path>')"
   :read-only nil
   :schema
   '((type . "object")
     (properties
      . (("subscriber_id"
          . ((type . "string")
             (description
              . "Caller-generated UUID identifying this subscriber (non-empty).  Every call from the same logical client must reuse the same id.")))
         ("watch_uuids"
          . ((type . "array")
             (items . ((type . "string")))
             (description
              . "Array of heading UUIDs to watch (may be the empty array).  Each entry must be a string.")))))
     (required . ["subscriber_id" "watch_uuids"]))
   :server-id anvil-org--server-id)
  (anvil-server-register-tool
   #'anvil-org-watch--tool-unregister
   :id "org-watch-unregister"
   :intent '(org-watch)
   :layer 'workflow
   :description
   "Unregister a subscriber from the org-state-change signal channel
and delete its transport file.

Idempotent: calling on an unknown `subscriber_id' returns ok without
error.  Subscribers whose transport file has no active monitor are
reaped lazily on the next registration.

Parameters:
  subscriber_id - The id passed to `org-watch-register' (string,
                  required, non-empty).

Returns JSON object:
  ok - Always true on success (boolean)"
   :read-only nil
   :schema
   '((type . "object")
     (properties
      . (("subscriber_id"
          . ((type . "string")
             (description
              . "The id passed to `org-watch-register' (non-empty).")))))
     (required . ["subscriber_id"]))
   :server-id anvil-org--server-id)
  (add-hook 'org-after-todo-state-change-hook
            #'anvil-org-watch--state-change-handler))

(defun anvil-org-watch-disable ()
  "Tear down the org-state-change signal channel."
  (remove-hook 'org-after-todo-state-change-hook
               #'anvil-org-watch--state-change-handler)
  (ignore-errors
    (anvil-server-unregister-tool "org-watch-register"
                                  anvil-org--server-id))
  (ignore-errors
    (anvil-server-unregister-tool "org-watch-unregister"
                                  anvil-org--server-id)))

(provide 'anvil-org-watch)
;;; anvil-org-watch.el ends here

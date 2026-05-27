;;; anvil-memory-bridge-test.el --- Tests for anvil-memory-bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT coverage for Doc 41 Phase 1 read API and Phase 2a guarded writes.

;;; Code:

(require 'ert)
(require 'json)
(require 'url)
(require 'anvil-memory)
(require 'anvil-memory-bridge)
(require 'anvil-worklog)

(defvar url-http-end-of-headers)


;;;; --- fixtures -----------------------------------------------------------

(defun anvil-memory-bridge-test--write (path content)
  "Write CONTENT to PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert content)))

(defmacro anvil-memory-bridge-test--with-env (&rest body)
  "Run BODY with a fresh memory DB and root."
  (declare (indent 0))
  `(let* ((root (make-temp-file "anvil-mbridge-" t))
          (anvil-memory-db-path (make-temp-file "anvil-mbridge-db-" nil ".db"))
          (anvil-worklog-db-path (make-temp-file "anvil-mbridge-wl-" nil ".db"))
          (anvil-memory-bridge-token-file
           (make-temp-file "anvil-mbridge-token-"))
          (anvil-memory-bridge--token nil)
          (anvil-memory--db nil)
          (anvil-worklog--db nil)
          (anvil-worklog--resolved-db-path nil)
          (anvil-memory-bridge-replica-mode nil)
          (anvil-memory-bridge-primary-url nil)
          (anvil-memory-bridge--replica-last-event-id 0)
          (anvil-memory-roots (list root))
          (anvil-memory-bridge--proc nil)
          (anvil-memory-bridge--started-at nil)
          (anvil-memory-bridge-bind "127.0.0.1")
          (anvil-memory-bridge-port 8730))
     (unwind-protect
         (progn
           (anvil-memory-enable)
           (anvil-worklog-enable)
           ,@body)
       (ignore-errors (anvil-memory-bridge-stop))
       (ignore-errors (anvil-memory-disable))
       (ignore-errors (anvil-worklog-disable))
       (ignore-errors (delete-file anvil-memory-db-path))
       (ignore-errors (delete-file anvil-worklog-db-path))
       (ignore-errors (delete-file anvil-memory-bridge-token-file))
       (ignore-errors (delete-directory root t)))))

(defun anvil-memory-bridge-test--seed (root)
  "Populate ROOT with bridge fixture rows."
  (anvil-memory-bridge-test--write
   (expand-file-name "feedback_bridge_rule.md" root)
   "---\ntype: feedback\nname: bridge rule\ndescription: API rule\n---\nneedle bridge body\n")
  (anvil-memory-bridge-test--write
   (expand-file-name "project_bridge_plan.md" root)
   "---\ntype: project\n---\nphase one plan\n"))

(defun anvil-memory-bridge-test--body (response)
  "Return RESPONSE body from a raw HTTP response string."
  (let ((pos (string-match "\r\n\r\n" response)))
    (and pos (substring response (+ pos 4)))))

(defun anvil-memory-bridge-test--json (response)
  "Decode JSON body from RESPONSE."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false nil))
    (json-read-from-string (anvil-memory-bridge-test--body response))))

(defun anvil-memory-bridge-test--auth ()
  "Return test Bearer auth headers."
  (setq anvil-memory-bridge--token "secret-token")
  '(("authorization" . "Bearer secret-token")))

(defun anvil-memory-bridge-test--json-body (alist)
  "Encode ALIST as compact JSON body."
  (let ((json-encoding-pretty-print nil))
    (json-encode alist)))

(defun anvil-memory-bridge-test--live-json (path)
  "Fetch PATH from the live bridge and decode JSON."
  (let* ((url (format "http://127.0.0.1:%s%s" anvil-memory-bridge-port path))
         (buf (url-retrieve-synchronously url t t 5)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (or (and (boundp 'url-http-end-of-headers)
                              url-http-end-of-headers)
                         (point-min)))
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-false nil))
            (json-read)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))


;;;; --- JSON coercion ------------------------------------------------------

(ert-deftest anvil-memory-bridge-test/plist-json-uses-bare-keys ()
  "Keyword plists are exposed as ordinary JSON object keys."
  (let ((json (anvil-memory-bridge--json
               (list :file "x" :type 'feedback :last-accessed nil))))
    (should (string-match-p "\"file\":\"x\"" json))
    (should (string-match-p "\"type\":\"feedback\"" json))
    (should (string-match-p "\"last-accessed\":null" json))))


;;;; --- dispatch -----------------------------------------------------------

(ert-deftest anvil-memory-bridge-test/health-endpoint-shape ()
  "GET /memory/health returns db, rows, uptime, and version."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-bridge-test--seed root)
    (anvil-memory-scan)
    (let* ((resp (anvil-memory-bridge--dispatch "GET" "/memory/health"))
           (body (anvil-memory-bridge-test--json resp)))
      (should (equal anvil-memory-db-path (cdr (assq 'db body))))
      (should (= 2 (cdr (assq 'rows body))))
      (should (cdr (assq 'version body))))))

(ert-deftest anvil-memory-bridge-test/search-endpoint-filters-type ()
  "GET /memory/search?q=...&type=... returns matching memory rows."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-bridge-test--seed root)
    (anvil-memory-scan)
    (let* ((resp (anvil-memory-bridge--dispatch
                  "GET" "/memory/search?q=needle&type=feedback&limit=5"))
           (rows (anvil-memory-bridge-test--json resp)))
      (should (= 1 (length rows)))
      (should (equal "feedback" (cdr (assq 'type (car rows)))))
      (should (string-match-p "feedback_bridge_rule"
                              (cdr (assq 'file (car rows))))))))

(ert-deftest anvil-memory-bridge-test/search-empty-result-is-array ()
  "GET /memory/search returns [] rather than null for no hits."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-bridge-test--seed root)
    (anvil-memory-scan)
    (let* ((resp (anvil-memory-bridge--dispatch
                  "GET" "/memory/search?q=absent-token"))
           (body (anvil-memory-bridge-test--body resp)))
      (should (string-match-p "\\[\\]" body)))))

(ert-deftest anvil-memory-bridge-test/list-endpoint-supports-decay-sort ()
  "GET /memory/list accepts type, with_decay, and sort."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-bridge-test--seed root)
    (anvil-memory-scan)
    (let* ((resp (anvil-memory-bridge--dispatch
                  "GET" "/memory/list?type=project&with_decay=true&sort=decay"))
           (rows (anvil-memory-bridge-test--json resp)))
      (should (= 1 (length rows)))
      (should (assq 'decay-score (car rows)))
      (should (equal "project" (cdr (assq 'type (car rows))))))))

(ert-deftest anvil-memory-bridge-test/get-endpoint-returns-body ()
  "GET /memory/get/<basename> resolves through `anvil-memory-get'."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-bridge-test--seed root)
    (anvil-memory-scan)
    (let* ((resp (anvil-memory-bridge--dispatch
                  "GET" "/memory/get/feedback_bridge_rule"))
           (row (anvil-memory-bridge-test--json resp)))
      (should (equal "bridge rule" (cdr (assq 'name row))))
      (should (string-match-p "needle bridge body"
                              (cdr (assq 'body row)))))))


;;;; --- worklog bridge endpoints ------------------------------------------

(ert-deftest anvil-memory-bridge-test/worklog-search-endpoint ()
  "GET /worklog/search exposes the worklog FTS index."
  (anvil-memory-bridge-test--with-env
    (anvil-worklog-add "Bridge worklog" "needle worklog body"
                       :date "2026-05-26"
                       :machine "linux-test"
                       :year 2026)
    (let* ((resp (anvil-memory-bridge--dispatch
                  "GET" "/worklog/search?q=needle&machine=linux-test"))
           (rows (anvil-memory-bridge-test--json resp)))
      (should (= 1 (length rows)))
      (should (equal "Bridge worklog" (cdr (assq 'title (car rows)))))
      (should (equal "linux-test" (cdr (assq 'machine (car rows))))))))

(ert-deftest anvil-memory-bridge-test/worklog-list-endpoint ()
  "GET /worklog/list supports date and machine filters."
  (anvil-memory-bridge-test--with-env
    (anvil-worklog-add "Older" "old body"
                       :date "2026-05-25"
                       :machine "linux-test"
                       :year 2026)
    (anvil-worklog-add "Newer" "new body"
                       :date "2026-05-26"
                       :machine "linux-test"
                       :year 2026)
    (let* ((resp (anvil-memory-bridge--dispatch
                  "GET" "/worklog/list?machine=linux-test&since=2026-05-26"))
           (rows (anvil-memory-bridge-test--json resp)))
      (should (= 1 (length rows)))
      (should (equal "Newer" (cdr (assq 'title (car rows))))))))

(ert-deftest anvil-memory-bridge-test/worklog-get-endpoint-by-digest ()
  "GET /worklog/get can fetch an entry by digest."
  (anvil-memory-bridge-test--with-env
    (let* ((id (anvil-worklog-add "Digest target" "digest body"
                                  :date "2026-05-26"
                                  :machine "linux-test"
                                  :year 2026))
           (digest (plist-get id :digest))
           (resp (anvil-memory-bridge--dispatch
                  "GET" (format "/worklog/get?digest=%s" digest)))
           (row (anvil-memory-bridge-test--json resp)))
      (should (equal "Digest target" (cdr (assq 'title row))))
      (should (equal digest (cdr (assq 'digest row)))))))

(ert-deftest anvil-memory-bridge-test/worklog-effective-db-path ()
  "GET /worklog/effective-db-path exposes the worklog DB resolver."
  (anvil-memory-bridge-test--with-env
    (let* ((resp (anvil-memory-bridge--dispatch
                  "GET" "/worklog/effective-db-path"))
           (body (anvil-memory-bridge-test--json resp)))
      (should (equal anvil-worklog-db-path (cdr (assq 'db body)))))))

(ert-deftest anvil-memory-bridge-test/not-found-is-json-404 ()
  "Unknown endpoints return a JSON error."
  (let* ((resp (anvil-memory-bridge--dispatch "GET" "/missing"))
         (body (anvil-memory-bridge-test--json resp)))
    (should (string-prefix-p "HTTP/1.1 404" resp))
    (should (cdr (assq 'error body)))))

(ert-deftest anvil-memory-bridge-test/cors-allows-configured-origin ()
  "Allowed browser origins receive CORS headers."
  (let ((resp (anvil-memory-bridge--dispatch
               "GET" "/memory/health"
               '(("origin" . "https://chatgpt.com")))))
    (should (string-match-p
             "Access-Control-Allow-Origin: https://chatgpt.com" resp))
    (should (string-match-p
             "Access-Control-Allow-Headers: Authorization, Content-Type"
             resp))))

(ert-deftest anvil-memory-bridge-test/cors-preflight-allows-token-header ()
  "OPTIONS preflight includes bearer-token-friendly CORS headers."
  (let ((resp (anvil-memory-bridge--dispatch
               "OPTIONS" "/memory/search"
               '(("origin" . "https://claude.ai")
                 ("access-control-request-headers" . "authorization")))))
    (should (string-prefix-p "HTTP/1.1 204" resp))
    (should (string-match-p
             "Access-Control-Allow-Origin: https://claude.ai" resp))
    (should (string-match-p
             "Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS"
             resp))))

(ert-deftest anvil-memory-bridge-test/cors-rejects-unconfigured-origin ()
  "Unconfigured origins receive no CORS allow-origin header."
  (let ((resp (anvil-memory-bridge--dispatch
               "GET" "/memory/health"
               '(("origin" . "https://example.invalid")))))
    (should-not (string-match-p "Access-Control-Allow-Origin" resp))))

(ert-deftest anvil-memory-bridge-test/mutating-endpoints-require-auth ()
  "Phase 2a write endpoints require Bearer auth."
  (let ((resp (anvil-memory-bridge--dispatch
               "POST" "/memory/save" nil "{}")))
    (should (string-prefix-p "HTTP/1.1 401" resp))))

(ert-deftest anvil-memory-bridge-test/save-endpoint-adds-db-memory ()
  "POST /memory/save writes through `anvil-memory-add'."
  (anvil-memory-bridge-test--with-env
    (let* ((resp (anvil-memory-bridge--dispatch
                  "POST" "/memory/save"
                  (anvil-memory-bridge-test--auth)
                  (anvil-memory-bridge-test--json-body
                   '((file . "feedback_bridge_write")
                     (type . "feedback")
                     (description . "Bridge write")
                     (body . "bridge write body")))))
           (row (anvil-memory-bridge-test--json resp))
           (hits (anvil-memory-search "bridge write body")))
      (should (string-prefix-p "HTTP/1.1 200" resp))
      (should (equal "feedback_bridge_write" (cdr (assq 'name row))))
      (should (integerp (cdr (assq 'event-id row))))
      (should (= 1 (length hits))))))

(ert-deftest anvil-memory-bridge-test/events-endpoint-streams-mutations ()
  "GET /memory/events returns Phase 2b SSE rows after bridge writes."
  (anvil-memory-bridge-test--with-env
    (let* ((save (anvil-memory-bridge--dispatch
                  "POST" "/memory/save"
                  (anvil-memory-bridge-test--auth)
                  (anvil-memory-bridge-test--json-body
                   '((file . "feedback_bridge_event")
                     (type . "feedback")
                     (body . "event body")))))
           (save-body (anvil-memory-bridge-test--json save))
           (event-id (cdr (assq 'event-id save-body)))
           (events (anvil-memory-bridge--dispatch
                    "GET" "/memory/events?since=0&limit=10"))
           (stream (anvil-memory-bridge-test--body events))
           (after (anvil-memory-bridge--dispatch
                   "GET" (format "/memory/events?since=%s" event-id)))
           (after-stream (anvil-memory-bridge-test--body after)))
      (should (string-prefix-p "HTTP/1.1 200" events))
      (should (string-match-p "Content-Type: text/event-stream" events))
      (should (string-match-p (format "id: %s" event-id) stream))
      (should (string-match-p "event: memory.save" stream))
      (should (string-match-p "feedback_bridge_event" stream))
      (should (string-match-p "event body" stream))
      (should-not (string-match-p "event: memory.save" after-stream)))))

(ert-deftest anvil-memory-bridge-test/replica-status-endpoint ()
  "GET /memory/replica/status reports Phase 2c replica state."
  (anvil-memory-bridge-test--with-env
    (let ((anvil-memory-bridge-replica-mode t)
          (anvil-memory-bridge-primary-url "http://primary.example")
          (anvil-memory-bridge--replica-last-event-id 7))
      (let* ((resp (anvil-memory-bridge--dispatch
                    "GET" "/memory/replica/status"))
             (body (anvil-memory-bridge-test--json resp)))
        (should (equal "replica" (cdr (assq 'mode body))))
        (should (eq t (cdr (assq 'replica-mode body))))
        (should (equal "http://primary.example"
                       (cdr (assq 'primary-url body))))
        (should (= 7 (cdr (assq 'last-event-id body))))
        (should-not (cdr (assq 'local-writes-allowed body)))))))

(ert-deftest anvil-memory-bridge-test/replica-mode-rejects-local-writes ()
  "Replica mode forbids local writes other than replica pull."
  (anvil-memory-bridge-test--with-env
    (let ((anvil-memory-bridge-replica-mode t))
      (let ((resp (anvil-memory-bridge--dispatch
                   "POST" "/memory/save"
                   (anvil-memory-bridge-test--auth)
                   (anvil-memory-bridge-test--json-body
                    '((file . "feedback_replica_write")
                      (type . "feedback")
                      (body . "should not write"))))))
        (should (string-prefix-p "HTTP/1.1 409" resp))
        (should-not (anvil-memory-get "feedback_replica_write"))))))

(ert-deftest anvil-memory-bridge-test/replica-pull-applies-save-event ()
  "POST /memory/replica/pull applies primary save events locally."
  (anvil-memory-bridge-test--with-env
    (let* ((anvil-memory-bridge-replica-mode t)
           (event '((id . 1)
                    (op . "save")
                    (payload . ((name . "feedback_replica_saved")
                                (type . "feedback")
                                (description . "Replica saved")
                                (body . "replicated body")
                                (tags . ["doc41" "replica"])))))
           (resp (anvil-memory-bridge--dispatch
                  "POST" "/memory/replica/pull"
                  (anvil-memory-bridge-test--auth)
                  (anvil-memory-bridge-test--json-body
                   `((events . [,event])))))
           (body (anvil-memory-bridge-test--json resp))
           (entry (anvil-memory-get "feedback_replica_saved")))
      (should (string-prefix-p "HTTP/1.1 200" resp))
      (should (= 1 (cdr (assq 'applied body))))
      (should (= 1 (cdr (assq 'last-event-id body))))
      (should entry)
      (should (string-match-p "replicated body"
                              (plist-get entry :body))))))

(ert-deftest anvil-memory-bridge-test/replica-pull-applies-delete-event ()
  "POST /memory/replica/pull applies primary delete events locally."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-add "feedback_replica_deleted" 'feedback "delete me")
    (let* ((anvil-memory-bridge-replica-mode t)
           (event '((id . 2)
                    (op . "delete")
                    (payload . ((name . "feedback_replica_deleted")))))
           (resp (anvil-memory-bridge--dispatch
                  "POST" "/memory/replica/pull"
                  (anvil-memory-bridge-test--auth)
                  (anvil-memory-bridge-test--json-body
                   `((events . [,event])))))
           (body (anvil-memory-bridge-test--json resp)))
      (should (string-prefix-p "HTTP/1.1 200" resp))
      (should (= 1 (cdr (assq 'applied body))))
      (should (= 2 (cdr (assq 'last-event-id body))))
      (should-not (anvil-memory-get "feedback_replica_deleted")))))

(ert-deftest anvil-memory-bridge-test/access-endpoint-bumps-count ()
  "PUT /memory/access bumps access_count by basename."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-add "feedback_bridge_access" 'feedback "body")
    (let* ((resp (anvil-memory-bridge--dispatch
                  "PUT" "/memory/access"
                  (anvil-memory-bridge-test--auth)
                  (anvil-memory-bridge-test--json-body
                   '((file . "feedback_bridge_access")))))
           (body (anvil-memory-bridge-test--json resp)))
      (should (string-prefix-p "HTTP/1.1 200" resp))
      (should (eq t (cdr (assq 'found body))))
      (should (integerp (cdr (assq 'event-id body))))
      (should (= 1 (cdr (assq 'access-count body)))))))

(ert-deftest anvil-memory-bridge-test/scan-and-prune-endpoints ()
  "POST /memory/scan and /memory/prune write the index."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-bridge-test--seed root)
    (let* ((scan (anvil-memory-bridge--dispatch
                  "POST" "/memory/scan"
                  (anvil-memory-bridge-test--auth)
                  (anvil-memory-bridge-test--json-body
                   `((roots . [,root])))))
           (scan-body (anvil-memory-bridge-test--json scan)))
      (should (= 2 (cdr (assq 'scanned scan-body)))))
    (delete-file (expand-file-name "feedback_bridge_rule.md" root))
    (let* ((prune (anvil-memory-bridge--dispatch
                   "POST" "/memory/prune"
                   (anvil-memory-bridge-test--auth)
                   (anvil-memory-bridge-test--json-body
                    `((roots . [,root])))))
           (prune-body (anvil-memory-bridge-test--json prune)))
      (should (= 1 (cdr (assq 'pruned prune-body)))))))

(ert-deftest anvil-memory-bridge-test/delete-endpoint-removes-memory ()
  "DELETE /memory/<name> removes DB-direct rows."
  (anvil-memory-bridge-test--with-env
    (anvil-memory-add "feedback_bridge_delete" 'feedback "delete body")
    (let ((resp (anvil-memory-bridge--dispatch
                 "DELETE" "/memory/feedback_bridge_delete"
                 (anvil-memory-bridge-test--auth))))
      (should (string-prefix-p "HTTP/1.1 200" resp))
      (should (integerp (cdr (assq 'event-id
                                   (anvil-memory-bridge-test--json resp)))))
      (should-not (anvil-memory-get "feedback_bridge_delete"))
      (should-not (anvil-memory-search "delete body")))))


;;;; --- live loopback ------------------------------------------------------

(ert-deftest anvil-memory-bridge-test/live-loopback-search ()
  "The network listener serves JSON over loopback."
  (skip-unless (or (getenv "ANVIL_MEMORY_BRIDGE_LIVE")
                   (getenv "ANVIL_ALLOW_LIVE")))
  (anvil-memory-bridge-test--with-env
    (anvil-memory-bridge-test--seed root)
    (anvil-memory-scan)
    (anvil-memory-bridge-start :port t)
    (let ((rows (anvil-memory-bridge-test--live-json
                 "/memory/search?q=needle&type=feedback")))
      (should (= 1 (length rows)))
      (should (equal "feedback" (cdr (assq 'type (car rows))))))))

(ert-deftest anvil-memory-bridge-test/start-stop-status ()
  "`anvil-memory-bridge-status' reflects listener state."
  (skip-unless (or (getenv "ANVIL_MEMORY_BRIDGE_LIVE")
                   (getenv "ANVIL_ALLOW_LIVE")))
  (anvil-memory-bridge-test--with-env
    (should-not (plist-get (anvil-memory-bridge-status) :running))
    (anvil-memory-bridge-start :port t)
    (should (plist-get (anvil-memory-bridge-status) :running))
    (should (integerp (anvil-memory-bridge-stop)))
    (should-not (plist-get (anvil-memory-bridge-status) :running))))

(provide 'anvil-memory-bridge-test)

;;; anvil-memory-bridge-test.el ends here

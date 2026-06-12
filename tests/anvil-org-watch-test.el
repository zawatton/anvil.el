;;; anvil-org-watch-test.el --- Org signal-channel ERTs -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil)
(require 'anvil-org)
(require 'anvil-server)
(require 'anvil-server-ert)
(require 'anvil-org-watch)

;;; --- Signal-channel fixture -----------------------------------------

(defvar anvil-org-test--watch-fixture nil
  "Plist holding the active signal-channel test fixture.
Keys: :file (temp .org file path), :watch-dir (temp watch dir),
:uuid-a, :uuid-b, :headline-a, :headline-b,
:saved-allowed, :saved-watch-dir, :saved-server-id,
:saved-subscribers, :saved-hook.")

(defun anvil-org-test--watch-make-org-file ()
  "Create a temp .org file with two TODO headings and return its path."
  (let* ((path (make-temp-file "anvil-org-watch-" nil ".org"))
         (uuid-a "11111111-1111-1111-1111-111111111111")
         (uuid-b "22222222-2222-2222-2222-222222222222"))
    (with-temp-file path
      (insert "* TODO Heading A\n"
              ":PROPERTIES:\n"
              ":ID:       " uuid-a "\n"
              ":END:\n"
              "\n"
              "* TODO Heading B\n"
              ":PROPERTIES:\n"
              ":ID:       " uuid-b "\n"
              ":END:\n"))
    path))

(defun anvil-org-test--watch-setup ()
  "Install the signal-channel test fixture."
  (let* ((path (anvil-org-test--watch-make-org-file))
         (watch-dir (make-temp-file "anvil-org-watch-dir-" t)))
    (setq anvil-org-test--watch-fixture
          (list :file path
                :watch-dir watch-dir
                :uuid-a "11111111-1111-1111-1111-111111111111"
                :uuid-b "22222222-2222-2222-2222-222222222222"
                :headline-a "Heading A"
                :headline-b "Heading B"
                :saved-allowed anvil-org-allowed-files
                :saved-watch-dir anvil-org-watch-dir
                :saved-server-id anvil-server-ert-server-id
                :saved-subscribers (copy-hash-table
                                    anvil-org-watch--subscribers)
                :saved-hook (copy-sequence
                             org-after-todo-state-change-hook)))
    (setq anvil-org-allowed-files (list path))
    (setq anvil-org-watch-dir (file-name-as-directory watch-dir))
    (setq anvil-server-ert-server-id "emacs-eval")
    (setq anvil-org-watch--subscribers (make-hash-table :test 'equal))
    ;; Register the org + org-watch tools and install the hook
    ;; (re-entrant: registration is ref-counted, add-hook idempotent).
    (anvil-org-enable)
    (anvil-org-watch-enable)))

(defun anvil-org-test--watch-teardown ()
  "Tear down the signal-channel test fixture.
Restores the global hook and subscribers table snapshots so test
runs are isolated from each other."
  (let* ((fx anvil-org-test--watch-fixture)
         (path (plist-get fx :file))
         (watch-dir (plist-get fx :watch-dir)))
    (when (and path (file-exists-p path))
      (delete-file path))
    (when (and watch-dir (file-directory-p watch-dir))
      (delete-directory watch-dir t))
    (setq anvil-org-allowed-files (plist-get fx :saved-allowed))
    (setq anvil-org-watch-dir (plist-get fx :saved-watch-dir))
    (setq anvil-server-ert-server-id (plist-get fx :saved-server-id))
    (setq anvil-org-watch--subscribers
          (or (plist-get fx :saved-subscribers)
              (make-hash-table :test 'equal)))
    (anvil-org-watch-disable)
    (anvil-org-disable)
    (setq org-after-todo-state-change-hook (plist-get fx :saved-hook))
    (setq anvil-org-test--watch-fixture nil)))

(defmacro anvil-org-test--with-watch-fixture (&rest body)
  "Run BODY with the signal-channel fixture installed.
Stubs `anvil-org-watch--file-monitored-p' to treat every existing
transport file as monitored: batch test runs have no tailing process,
and without the stub a second registration would reap the first
subscriber."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (anvil-org-test--watch-setup)
         (cl-letf (((symbol-function 'anvil-org-watch--file-monitored-p)
                    #'file-exists-p))
           ,@body))
     (anvil-org-test--watch-teardown)))

(defun anvil-org-test--read-ndjson (path)
  "Return the list of decoded JSON objects from NDJSON file at PATH."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (let ((lines (split-string (buffer-string) "\n" t))
            (out '()))
        (dolist (line lines)
          (unless (string-empty-p (string-trim line))
            (push (json-read-from-string line) out)))
        (nreverse out)))))

(defun anvil-org-test--toggle-todo-via-org-todo (uuid new-state)
  "Open the fixture file and call `org-todo' on UUID to set NEW-STATE."
  (let* ((fx anvil-org-test--watch-fixture)
         (file (plist-get fx :file)))
    (with-current-buffer (find-file-noselect file)
      (revert-buffer t t)
      (goto-char (point-min))
      ;; org-id-goto needs a populated org-id DB; locate the property
      ;; directly so batch runs need no org-id-locations state.
      (goto-char (org-find-property "ID" uuid))
      (org-back-to-heading t)
      (let ((org-todo-log-states nil))
        (org-todo new-state))
      (save-buffer)
      (kill-buffer (current-buffer)))))


;;; --- Live-server signal-channel tests -------------------------------

(ert-deftest anvil-org-test-watch-register-creates-empty-transport ()
  "Register creates the NDJSON transport file as an empty file."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((fx anvil-org-test--watch-fixture)
             (sub "sub-empty-xform")
             (raw
              (anvil-server-ert-call-tool
               "org-watch-register"
               `(("subscriber_id" . ,sub)
                 ("watch_uuids" . ,(vector (plist-get fx :uuid-a))))))
             (decoded (json-read-from-string raw))
             (path (cdr (assq 'transport_path decoded))))
        (should (eq t (cdr (assq 'ok decoded))))
        (should (equal sub (cdr (assq 'subscriber_id decoded))))
        (should (equal 1 (cdr (assq 'watch_count decoded))))
        (should (stringp path))
        (should (file-exists-p path))
        (should (= 0 (file-attribute-size (file-attributes path))))))))

(ert-deftest anvil-org-test-watch-register-truncates-existing-file ()
  "Re-registering truncates the transport file."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((fx anvil-org-test--watch-fixture)
             (sub "sub-trunc"))
        (let* ((raw
                (anvil-server-ert-call-tool
                 "org-watch-register"
                 `(("subscriber_id" . ,sub)
                   ("watch_uuids" . ,(vector (plist-get fx :uuid-a))))))
               (path (cdr (assq 'transport_path
                                (json-read-from-string raw)))))
          (with-temp-buffer
            (insert "{\"stale\":true}\n")
            (write-region (point-min) (point-max) path))
          (should (> (file-attribute-size (file-attributes path)) 0))
          (let ((raw2
                 (anvil-server-ert-call-tool
                  "org-watch-register"
                  `(("subscriber_id" . ,sub)
                    ("watch_uuids" . ,(vector (plist-get fx :uuid-a)))))))
            (let ((path2 (cdr (assq 'transport_path
                                    (json-read-from-string raw2)))))
              (should (equal path path2))
              (should (= 0 (file-attribute-size
                            (file-attributes path2)))))))))))

(ert-deftest anvil-org-test-watch-hook-fires-from-mcp-update ()
  "MCP `org-update-todo-state' fires the hook and emits one event."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((fx anvil-org-test--watch-fixture)
             (sub "sub-mcp")
             (raw
              (anvil-server-ert-call-tool
               "org-watch-register"
               `(("subscriber_id" . ,sub)
                 ("watch_uuids" . ,(vector (plist-get fx :uuid-a))))))
             (path (cdr (assq 'transport_path
                              (json-read-from-string raw)))))
        (anvil-server-ert-call-tool
         "org-update-todo-state"
         `(("uri" . ,(concat "org-id://" (plist-get fx :uuid-a)))
           ("current_state" . "TODO")
           ("new_state" . "DONE")))
        (let* ((events (anvil-org-test--read-ndjson path)))
          (should (= 1 (length events)))
          (let ((e (car events)))
            (should (equal (plist-get fx :uuid-a)
                           (cdr (assq 'uuid e))))
            (should (equal (plist-get fx :file)
                           (cdr (assq 'file e))))
            (should (equal (plist-get fx :headline-a)
                           (cdr (assq 'headline e))))
            (should (equal "TODO" (cdr (assq 'previous_state e))))
            (should (equal "DONE" (cdr (assq 'new_state e))))
            (should (stringp (cdr (assq 'timestamp e))))
            (should (string-match-p
                     "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'"
                     (cdr (assq 'timestamp e))))))))))

(ert-deftest anvil-org-test-watch-hook-fires-from-direct-org-todo ()
  "Direct `org-todo' in a buffer (interactive path) also fires the hook."
  (anvil-org-test--with-watch-fixture
    (let* ((fx anvil-org-test--watch-fixture)
           (sub "sub-direct"))
      (anvil-server-ert-with-server :tools t :resources t
        (let* ((raw
                (anvil-server-ert-call-tool
                 "org-watch-register"
                 `(("subscriber_id" . ,sub)
                   ("watch_uuids" . ,(vector (plist-get fx :uuid-a))))))
               (path (cdr (assq 'transport_path
                                (json-read-from-string raw)))))
          (anvil-org-test--toggle-todo-via-org-todo
           (plist-get fx :uuid-a) "DONE")
          (let ((events (anvil-org-test--read-ndjson path)))
            (should (= 1 (length events)))
            (should (equal "DONE"
                           (cdr (assq 'new_state (car events)))))))))))

(ert-deftest anvil-org-test-watch-set-filtering ()
  "An event for a non-watched UUID does not leak to the subscriber."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((fx anvil-org-test--watch-fixture)
             (sub "sub-filter")
             (raw
              (anvil-server-ert-call-tool
               "org-watch-register"
               `(("subscriber_id" . ,sub)
                 ("watch_uuids" . ,(vector (plist-get fx :uuid-a))))))
             (path (cdr (assq 'transport_path
                              (json-read-from-string raw)))))
        (anvil-server-ert-call-tool
         "org-update-todo-state"
         `(("uri" . ,(concat "org-id://" (plist-get fx :uuid-b)))
           ("current_state" . "TODO")
           ("new_state" . "DONE")))
        (should (= 0 (length (anvil-org-test--read-ndjson path))))))))

(ert-deftest anvil-org-test-watch-multi-subscriber-broadcast ()
  "Two subscribers with overlapping watch-sets each receive one line."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((fx anvil-org-test--watch-fixture)
             (sub-1 "sub-1")
             (sub-2 "sub-2")
             (uuid-a (plist-get fx :uuid-a))
             (uuid-b (plist-get fx :uuid-b))
             (p1
              (cdr (assq 'transport_path
                         (json-read-from-string
                          (anvil-server-ert-call-tool
                           "org-watch-register"
                           `(("subscriber_id" . ,sub-1)
                             ("watch_uuids" . ,(vector uuid-a uuid-b))))))))
             (p2
              (cdr (assq 'transport_path
                         (json-read-from-string
                          (anvil-server-ert-call-tool
                           "org-watch-register"
                           `(("subscriber_id" . ,sub-2)
                             ("watch_uuids" . ,(vector uuid-a)))))))))
        (anvil-server-ert-call-tool
         "org-update-todo-state"
         `(("uri" . ,(concat "org-id://" uuid-a))
           ("current_state" . "TODO")
           ("new_state" . "DONE")))
        (anvil-server-ert-call-tool
         "org-update-todo-state"
         `(("uri" . ,(concat "org-id://" uuid-b))
           ("current_state" . "TODO")
           ("new_state" . "DONE")))
        (should (= 2 (length (anvil-org-test--read-ndjson p1))))
        (let ((ev (anvil-org-test--read-ndjson p2)))
          (should (= 1 (length ev)))
          (should (equal uuid-a (cdr (assq 'uuid (car ev))))))))))

(ert-deftest anvil-org-test-watch-unregister-removes-state ()
  "Unregister deletes the transport file and the broker entry."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((fx anvil-org-test--watch-fixture)
             (sub "sub-unreg")
             (raw
              (anvil-server-ert-call-tool
               "org-watch-register"
               `(("subscriber_id" . ,sub)
                 ("watch_uuids" . ,(vector (plist-get fx :uuid-a))))))
             (path (cdr (assq 'transport_path
                              (json-read-from-string raw)))))
        (should (file-exists-p path))
        (should (gethash sub anvil-org-watch--subscribers))
        (let* ((raw2
                (anvil-server-ert-call-tool
                 "org-watch-unregister"
                 `(("subscriber_id" . ,sub))))
               (decoded (json-read-from-string raw2)))
          (should (eq t (cdr (assq 'ok decoded)))))
        (should-not (file-exists-p path))
        (should-not (gethash sub anvil-org-watch--subscribers))
        (let ((raw3
               (anvil-server-ert-call-tool
                "org-watch-unregister"
                `(("subscriber_id" . ,sub)))))
          (should (eq t (cdr (assq 'ok (json-read-from-string raw3))))))))))

(ert-deftest anvil-org-test-watch-reregister-after-unregister ()
  "Register after unregister works cleanly and starts from an empty file."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((fx anvil-org-test--watch-fixture)
             (sub "sub-rereg")
             (uuid-a (plist-get fx :uuid-a))
             (raw1
              (anvil-server-ert-call-tool
               "org-watch-register"
               `(("subscriber_id" . ,sub)
                 ("watch_uuids" . ,(vector uuid-a)))))
             (path1 (cdr (assq 'transport_path
                               (json-read-from-string raw1)))))
        (anvil-server-ert-call-tool
         "org-update-todo-state"
         `(("uri" . ,(concat "org-id://" uuid-a))
           ("current_state" . "TODO")
           ("new_state" . "DONE")))
        (should (= 1 (length (anvil-org-test--read-ndjson path1))))
        (anvil-server-ert-call-tool
         "org-watch-unregister"
         `(("subscriber_id" . ,sub)))
        (let* ((raw2
                (anvil-server-ert-call-tool
                 "org-watch-register"
                 `(("subscriber_id" . ,sub)
                   ("watch_uuids" . ,(vector uuid-a)))))
               (path2 (cdr (assq 'transport_path
                                 (json-read-from-string raw2)))))
          (should (file-exists-p path2))
          (should (= 0 (length (anvil-org-test--read-ndjson path2)))))))))

(ert-deftest anvil-org-test-watch-handler-tolerates-no-id ()
  "Handler does not error when the heading has no `:ID:'."
  (anvil-org-test--with-watch-fixture
    (let* ((fx anvil-org-test--watch-fixture)
           (file (plist-get fx :file)))
      (with-temp-buffer
        (insert "\n* TODO No-ID heading\n")
        (write-region (point-min) (point-max) file 'append))
      (with-current-buffer (find-file-noselect file)
        (revert-buffer t t)
        (goto-char (point-max))
        (re-search-backward "No-ID heading")
        (should-not (org-entry-get nil "ID"))
        (let ((org-state "DONE")
              (org-last-state "TODO"))
          (anvil-org-watch--state-change-handler))
        (kill-buffer (current-buffer))))))

(ert-deftest anvil-org-test-watch-handler-tolerates-disallowed-buffer ()
  "Handler does not error when the buffer is not in the allowed list."
  (anvil-org-test--with-watch-fixture
    (let ((stranger (make-temp-file "anvil-org-stranger-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file stranger
              (insert "* TODO Stranger\n"
                      ":PROPERTIES:\n"
                      ":ID:       99999999-9999-9999-9999-999999999999\n"
                      ":END:\n"))
            (with-current-buffer (find-file-noselect stranger)
              (revert-buffer t t)
              (goto-char (point-min))
              (let ((org-state "DONE")
                    (org-last-state "TODO"))
                (anvil-org-watch--state-change-handler))
              (kill-buffer (current-buffer))))
        (when (file-exists-p stranger)
          (delete-file stranger))))))

(ert-deftest anvil-org-test-watch-tools-listed ()
  "Both new tools appear in tools/list from a fresh server."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((request
              (anvil-server-create-tools-list-request))
             (response
              (anvil-server-process-jsonrpc-parsed
               request anvil-server-ert-server-id))
             (result (alist-get 'result response))
             (tools (alist-get 'tools result))
             (names (mapcar (lambda (t-alist)
                              (alist-get 'name t-alist))
                            (append tools nil))))
        (should (member "org-watch-register" names))
        (should (member "org-watch-unregister" names))))))

(ert-deftest anvil-org-test-watch-register-input-schema-shape ()
  "Advertised inputSchema for `org-watch-register' types `watch_uuids'
as an array of strings (not a string), with non-empty descriptions on
both required parameters.  Guards the MCP-path bug where the schema
auto-generator would type every parameter as string and the live MCP
client would reject array arguments before dispatch."
  (anvil-org-test--with-watch-fixture
    (anvil-server-ert-with-server :tools t :resources t
      (let* ((request
              (anvil-server-create-tools-list-request))
             (response
              (anvil-server-process-jsonrpc-parsed
               request anvil-server-ert-server-id))
             (result (alist-get 'result response))
             (tools (append (alist-get 'tools result) nil))
             (entry
              (seq-find
               (lambda (t-alist)
                 (equal (alist-get 'name t-alist) "org-watch-register"))
               tools))
             (schema (alist-get 'inputSchema entry))
             (properties (alist-get 'properties schema))
             (required (alist-get 'required schema))
             (sub (alist-get 'subscriber_id properties))
             (wu (alist-get 'watch_uuids properties))
             (wu-items (alist-get 'items wu)))
        (should entry)
        (should (equal (alist-get 'type schema) "object"))
        (should (equal (alist-get 'type sub) "string"))
        (should (stringp (alist-get 'description sub)))
        (should (> (length (alist-get 'description sub)) 0))
        (should (equal (alist-get 'type wu) "array"))
        (should (equal (alist-get 'type wu-items) "string"))
        (should (stringp (alist-get 'description wu)))
        (should (> (length (alist-get 'description wu)) 0))
        (let ((req-list (append required nil)))
          (should (member "subscriber_id" req-list))
          (should (member "watch_uuids" req-list)))))))


;;; --- End-to-end integration (fixture-bypass) ------------------------

(ert-deftest anvil-org-test-watch-end-to-end-emit-via-live-hook ()
  "Register via MCP tools-call → real `org-todo' on a temp .org file
added to `anvil-org-allowed-files' → tail the transport file → assert
exactly one NDJSON line with all six fields and matching uuid.

Owns its setup top-to-bottom (no shared fixture) so a fixture
regression cannot mask it.  Confirms the live global hook actually
delivers the emit when a real writer changes a TODO state."
  (let* ((saved-allowed anvil-org-allowed-files)
         (saved-watch-dir anvil-org-watch-dir)
         (saved-subscribers (copy-hash-table anvil-org-watch--subscribers))
         (saved-hook (copy-sequence org-after-todo-state-change-hook))
         (saved-server-id anvil-server-ert-server-id)
         (uuid "33333333-3333-3333-3333-333333333333")
         (path (make-temp-file "anvil-org-e2e-" nil ".org"))
         (watch-dir (make-temp-file "anvil-org-e2e-watch-" t)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "* TODO E2E Heading\n"
                    ":PROPERTIES:\n"
                    ":ID:       " uuid "\n"
                    ":END:\n"))
          (setq anvil-org-allowed-files (list path))
          (setq anvil-org-watch-dir (file-name-as-directory watch-dir))
          (setq anvil-server-ert-server-id "emacs-eval")
          (setq anvil-org-watch--subscribers (make-hash-table :test 'equal))
          (anvil-org-enable)
          (anvil-org-watch-enable)
          (should (memq 'anvil-org-watch--state-change-handler
                        org-after-todo-state-change-hook))
          (anvil-server-ert-with-server :tools t :resources t
            (let* ((sub "sub-e2e")
                   (raw
                    (anvil-server-ert-call-tool
                     "org-watch-register"
                     `(("subscriber_id" . ,sub)
                       ("watch_uuids" . ,(vector uuid)))))
                   (decoded (json-read-from-string raw))
                   (transport (cdr (assq 'transport_path decoded))))
              (should (eq t (cdr (assq 'ok decoded))))
              (should (file-exists-p transport))
              (with-current-buffer (find-file-noselect path)
                (revert-buffer t t)
                (goto-char (point-min))
                (goto-char (org-find-property "ID" uuid))
                (org-back-to-heading t)
                (let ((org-todo-log-states nil))
                  (org-todo "DONE"))
                (save-buffer)
                (kill-buffer (current-buffer)))
              (let ((events (anvil-org-test--read-ndjson transport)))
                (should (= 1 (length events)))
                (let ((e (car events)))
                  (should (equal uuid (cdr (assq 'uuid e))))
                  (should (equal path (cdr (assq 'file e))))
                  (should (equal "E2E Heading"
                                 (cdr (assq 'headline e))))
                  (should (equal "TODO" (cdr (assq 'previous_state e))))
                  (should (equal "DONE" (cdr (assq 'new_state e))))
                  (should (stringp (cdr (assq 'timestamp e)))))))))
      (when (file-exists-p path) (delete-file path))
      (when (file-directory-p watch-dir) (delete-directory watch-dir t))
      (anvil-org-watch-disable)
      (anvil-org-disable)
      (setq anvil-org-allowed-files saved-allowed)
      (setq anvil-org-watch-dir saved-watch-dir)
      (setq anvil-server-ert-server-id saved-server-id)
      (setq anvil-org-watch--subscribers saved-subscribers)
      (setq org-after-todo-state-change-hook saved-hook))))

(provide 'anvil-org-watch-test)
;;; anvil-org-watch-test.el ends here

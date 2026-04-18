;;; anvil-browser-test.el --- Tests for anvil-browser -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `anvil-browser' that stub out
;; `anvil-browser--run-batch' so no real Chrome / agent-browser
;; process is launched.  One live smoke test at the bottom runs the
;; CLI against https://example.com and is skipped unless
;; `agent-browser' is discoverable on PATH.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-browser)
(require 'anvil-state)

;;;; --- fixtures -----------------------------------------------------------

(defvar anvil-browser-test--last-commands nil
  "Record of the last COMMANDS list passed to the stubbed runner.")

(defvar anvil-browser-test--last-session nil
  "Record of the last SESSION value passed to the stubbed runner.")

(defun anvil-browser-test--stub-result-for (commands)
  "Produce a fake `agent-browser batch --json' result list for COMMANDS.
Each entry mirrors the real CLI output shape — `:command',
`:success', `:error', `:result'.  Snapshot commands include a
deterministic `:snapshot' string so tests can assert on it."
  (mapcar
   (lambda (cmd)
     (let ((verb (car cmd)))
       (list :command cmd
             :success t
             :error nil
             :result
             (cond
              ((string= verb "snapshot")
               (list :snapshot
                     (format "- text \"stubbed %s\" [ref=e1]"
                             (mapconcat #'identity cmd " "))))
              ((string= verb "open")
               (list :url (cadr cmd) :title "stubbed"))
              (t (list :ok t))))))
   commands))

(defmacro anvil-browser-test--with-stub (&rest body)
  "Run BODY with `anvil-browser--run-batch' replaced by a stub.
Also gives each test a fresh `anvil-state' DB so cache entries
do not leak between runs.  The stub records arguments in
`anvil-browser-test--last-commands' and returns the fixture from
`anvil-browser-test--stub-result-for'."
  (declare (indent 0))
  `(let ((anvil-browser-test--last-commands nil)
         (anvil-browser-test--last-session nil)
         (anvil-state-db-path (make-temp-file "anvil-browser-st-" nil ".db"))
         (anvil-state--db nil)
         (anvil-browser--metrics (list :fetches 0 :cache-hits 0
                                       :errors 0 :log nil)))
     (unwind-protect
         (progn
           (anvil-state-enable)
           (cl-letf (((symbol-function 'anvil-browser--run-batch)
                      (lambda (commands &optional session)
                        (setq anvil-browser-test--last-commands commands)
                        (setq anvil-browser-test--last-session session)
                        (anvil-browser-test--stub-result-for commands)))
                     ((symbol-function 'anvil-browser--cli-path)
                      (lambda () "/stub/agent-browser")))
             ,@body))
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

;;;; --- browser-fetch ------------------------------------------------------

(ert-deftest anvil-browser-test-fetch-returns-snapshot ()
  "`browser-fetch' returns the snapshot string from the last batch entry."
  (anvil-browser-test--with-stub
    (let ((snap (anvil-browser--tool-fetch "https://example.com")))
      (should (stringp snap))
      (should (string-match-p "stubbed snapshot -i -c" snap)))))

(ert-deftest anvil-browser-test-fetch-cache-hit ()
  "Second fetch of the same URL within TTL hits the cache, no new batch."
  (anvil-browser-test--with-stub
    (anvil-browser--tool-fetch "https://example.com")
    (setq anvil-browser-test--last-commands nil)
    (let ((snap (anvil-browser--tool-fetch "https://example.com")))
      (should (stringp snap))
      (should (null anvil-browser-test--last-commands))
      (should (= 1 (plist-get anvil-browser--metrics :cache-hits))))))

(ert-deftest anvil-browser-test-fetch-selector-forwarded ()
  "A non-empty selector becomes `-s SEL' on the snapshot command."
  (anvil-browser-test--with-stub
    (anvil-browser--tool-fetch "https://example.com" "main article")
    (let ((snapshot-cmd
           (cl-find-if (lambda (c) (string= (car c) "snapshot"))
                       anvil-browser-test--last-commands)))
      (should snapshot-cmd)
      (should (member "-s" snapshot-cmd))
      (should (member "main article" snapshot-cmd)))))

(ert-deftest anvil-browser-test-fetch-session-override ()
  "Passing SESSION forwards it to the runner verbatim."
  (anvil-browser-test--with-stub
    (anvil-browser--tool-fetch "https://example.com" nil "work")
    (should (equal "work" anvil-browser-test--last-session))))

;;;; --- browser-interact ---------------------------------------------------

(ert-deftest anvil-browser-test-interact-wraps-actions ()
  "`browser-interact' prepends `open' and appends a final `snapshot'."
  (anvil-browser-test--with-stub
    (let* ((actions "[[\"click\",\"@e1\"],[\"fill\",\"@e2\",\"hi\"]]")
           (snap (anvil-browser--tool-interact
                  "https://example.com" actions)))
      (should (stringp snap))
      (let ((verbs (mapcar #'car anvil-browser-test--last-commands)))
        (should (equal '("open" "click" "fill" "snapshot") verbs))))))

(ert-deftest anvil-browser-test-interact-invalid-json ()
  "Malformed ACTIONS surfaces a clear user-error, not a generic crash."
  (anvil-browser-test--with-stub
    (should-error
     (anvil-browser--tool-interact "https://example.com" "not-json")
     :type 'anvil-server-tool-error)))

;;;; --- browser-capture ----------------------------------------------------

(ert-deftest anvil-browser-test-capture-writes-org-file ()
  "`browser-capture' writes the rendered template to `capture-dir'."
  (let* ((tmpdir (make-temp-file "anvil-browser-cap-" t))
         (anvil-browser-capture-dir tmpdir))
    (unwind-protect
        (anvil-browser-test--with-stub
          (let* ((path (anvil-browser--tool-capture
                        "https://example.com" "Hello" "ai research")))
            (should (file-exists-p path))
            (let ((content (with-temp-buffer
                             (let ((coding-system-for-read 'utf-8))
                               (insert-file-contents path))
                             (buffer-string))))
              (should (string-match-p "#\\+TITLE: Hello" content))
              (should (string-match-p "#\\+PROPERTY: URL https://example.com"
                                      content))
              (should (string-match-p ":ai:research:" content))
              (should (string-match-p "stubbed snapshot" content)))))
      (ignore-errors (delete-directory tmpdir t)))))

;;;; --- browser-screenshot -------------------------------------------------

(ert-deftest anvil-browser-test-screenshot-returns-png-path ()
  "`browser-screenshot' returns a .png temp path and forwards the region."
  (anvil-browser-test--with-stub
    (let ((path (anvil-browser--tool-screenshot
                 "https://example.com" ".main")))
      (unwind-protect
          (progn
            (should (stringp path))
            (should (string-suffix-p ".png" path))
            (let ((shot-cmd (cl-find-if
                             (lambda (c) (string= (car c) "screenshot"))
                             anvil-browser-test--last-commands)))
              (should shot-cmd)
              (should (member ".main" shot-cmd))
              (should (member path shot-cmd))))
        (when (and (stringp path) (file-exists-p path))
          (delete-file path))))))

;;;; --- helpers ------------------------------------------------------------

(ert-deftest anvil-browser-test-normalize-tags-space ()
  "Space/comma-separated tags become an org :a:b: string."
  (should (equal ":ai:research:"
                 (anvil-browser--normalize-tags "ai research")))
  (should (equal ":ai:research:"
                 (anvil-browser--normalize-tags "ai,research")))
  (should (equal "" (anvil-browser--normalize-tags nil)))
  (should (equal "" (anvil-browser--normalize-tags ""))))

(ert-deftest anvil-browser-test-subst-template-all-keys ()
  "Placeholders are substituted; unknown ones remain verbatim."
  (should (equal "A|2026|U|:t:|C|%OTHER%"
                 (anvil-browser--subst-template
                  "%TITLE%|%DATE%|%URL%|%TAGS%|%CONTENT%|%OTHER%"
                  (list :title "A" :date "2026" :url "U"
                        :tags ":t:" :content "C")))))

;;;; --- live smoke test ----------------------------------------------------

(ert-deftest anvil-browser-test-live-fetch-example-com ()
  "Hit the real CLI against example.com if agent-browser is installed."
  (skip-unless (and (executable-find "agent-browser")
                    (not (getenv "ANVIL_SKIP_LIVE"))))
  (let ((anvil-state-db-path (make-temp-file "anvil-browser-live-" nil ".db"))
        (anvil-state--db nil)
        (anvil-browser--metrics (list :fetches 0 :cache-hits 0
                                      :errors 0 :log nil))
        (anvil-browser-cache-ttl-sec 0)
        (anvil-browser-timeout-sec 90))
    (unwind-protect
        (progn
          (anvil-state-enable)
          (let ((snap (anvil-browser--tool-fetch "https://example.com")))
            (should (stringp snap))
            (should (string-match-p "Example Domain" snap))))
      (anvil-state-disable)
      (ignore-errors (delete-file anvil-state-db-path)))))

(provide 'anvil-browser-test)
;;; anvil-browser-test.el ends here

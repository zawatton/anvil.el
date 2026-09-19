;;; anvil-session-capture-test.el --- Tests for anvil-session-capture -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 63 Phase 2.  `anvil-session-capture-classify' is pure, so
;; nearly everything here runs without a database or a hook.
;;
;; The tests assert on the fields that later decide what survives a
;; snapshot budget — :category and :priority — not just on :kind.  A
;; classifier that files every row under priority 3 would still emit
;; the right :kind, and the snapshot would then drop the errors first.

;;; Code:

(require 'ert)
(require 'anvil-session-capture)
(require 'anvil-session-store)

(defun anvil-session-capture-test--payload (&rest kv)
  "Build a hook payload alist from KV, a plist of string keys."
  (let (out)
    (while kv
      (push (cons (car kv) (cadr kv)) out)
      (setq kv (cddr kv)))
    (nreverse out)))

;;;; --- file operations -----------------------------------------------------

(ert-deftest anvil-session-capture-test-edit-carries-the-path ()
  "An Edit yields a P1 file row whose `data' is the path, not the tool."
  (let ((ev (anvil-session-capture-classify
             (anvil-session-capture-test--payload
              "hook_event_name" "PostToolUse"
              "tool_name" "Edit"
              "tool_input" '(("file_path" . "/repo/anvil-worker.el"))))))
    (should (equal (plist-get ev :kind) "file_edit"))
    (should (eq (plist-get ev :category) 'file))
    (should (= (plist-get ev :priority) 1))
    ;; The whole point of Phase 2: the path reaches the store.
    (should (equal (plist-get ev :data) "/repo/anvil-worker.el"))))

(ert-deftest anvil-session-capture-test-rule-file-read-outranks-plain-read ()
  "Reading CLAUDE.md is a P1 rule row; reading source is a P3 file row."
  (let ((rule (anvil-session-capture-classify
               (anvil-session-capture-test--payload
                "tool_name" "Read"
                "tool_input" '(("file_path" . "/repo/CLAUDE.md")))))
        (plain (anvil-session-capture-classify
                (anvil-session-capture-test--payload
                 "tool_name" "Read"
                 "tool_input" '(("file_path" . "/repo/anvil.el"))))))
    (should (eq (plist-get rule :category) 'rule))
    (should (= (plist-get rule :priority) 1))
    (should (eq (plist-get plain :category) 'file))
    (should (= (plist-get plain :priority) 3))
    (should (< (plist-get rule :priority) (plist-get plain :priority)))))

;;;; --- bash / git ----------------------------------------------------------

(ert-deftest anvil-session-capture-test-state-changing-git-is-p1 ()
  "`git commit' is a P1 git row; `git status' is not."
  (let ((commit (anvil-session-capture-classify
                 (anvil-session-capture-test--payload
                  "tool_name" "Bash"
                  "tool_input" '(("command" . "git commit -m 'fix'")))))
        (status (anvil-session-capture-classify
                 (anvil-session-capture-test--payload
                  "tool_name" "Bash"
                  "tool_input" '(("command" . "git status --porcelain"))))))
    (should (eq (plist-get commit :category) 'git))
    (should (= (plist-get commit :priority) 1))
    ;; Read-only git is deliberately demoted to ordinary tool noise so
    ;; it cannot bury the commits in a snapshot section.
    (should (eq (plist-get status :category) 'tool-use))
    (should (= (plist-get status :priority) 3))))

(ert-deftest anvil-session-capture-test-mentioning-git-is-not-running-git ()
  "A command that merely contains a git invocation is not a git op.

Found on live data the day the hook was wired: an `echo' of a JSON
payload whose text included \"git commit -m wired\" was recorded as
a P1 git row. The old matcher scanned the whole command string, so
anything quoting a git command — an echo, a grep, a heredoc — was
filed as one. P1 survives the snapshot budget, so a false positive
there displaces a real commit."
  (dolist (cmd '("echo '{\"command\":\"git commit -m wired\"}' | ./hook"
                 "grep -rn 'git push' scripts/"
                 "cat <<EOF\ngit rebase -i main\nEOF"))
    (let ((ev (anvil-session-capture-classify
               (anvil-session-capture-test--payload
                "tool_name" "Bash" "tool_input" `(("command" . ,cmd))))))
      (should (eq (plist-get ev :category) 'tool-use))))
  ;; ...while a real invocation in any segment still counts, including
  ;; the pre-subcommand flag forms. `--no-pager' is the case that broke
  ;; the first fix: a rule of "a flag may take the next token" swallowed
  ;; the subcommand, so the value-taking flags are enumerated instead.
  (dolist (cmd '("git commit -m x"
                 "  cd /repo && git push origin main"
                 "GIT_AUTHOR_NAME=x git commit -m y"
                 "/usr/bin/git -C /repo checkout develop"
                 "git -c user.name=x commit -m z"
                 "git --no-pager merge develop"
                 "sudo git reset --hard"))
    (let ((ev (anvil-session-capture-classify
               (anvil-session-capture-test--payload
                "tool_name" "Bash" "tool_input" `(("command" . ,cmd))))))
      (should (eq (plist-get ev :category) 'git))
      (should (= (plist-get ev :priority) 1)))))

(ert-deftest anvil-session-capture-test-git-word-must-be-the-command ()
  "`git' must be a whole word followed by a real subcommand.

The input is chosen so the word boundary is the only thing that
decides the answer: \"legit push\" contains the letters `git'
immediately followed by a space and the subcommand `push', so an
unanchored pattern classifies this echo as a git push.  An input
like \"digital content\" would pass either way and would not test
the anchor at all."
  (let ((ev (anvil-session-capture-classify
             (anvil-session-capture-test--payload
              "tool_name" "Bash"
              "tool_input" '(("command" . "echo 'legit push to prod'"))))))
    (should (eq (plist-get ev :category) 'tool-use))
    (should-not (eq (plist-get ev :category) 'git))))

;;;; --- failures ------------------------------------------------------------

(ert-deftest anvil-session-capture-test-failure-becomes-p1-error ()
  "A tool_response carrying an error produces a P1 error row.

This must win over the tool's own classification: a failed Edit is
an error first and a file edit second."
  (let ((ev (anvil-session-capture-classify
             (anvil-session-capture-test--payload
              "tool_name" "Edit"
              "tool_input" '(("file_path" . "/repo/a.el"))
              "tool_response" '(("is_error" . t)
                                ("stderr" . "String not found in file"))))))
    (should (eq (plist-get ev :category) 'error))
    (should (= (plist-get ev :priority) 1))
    (should (equal (plist-get ev :data) "/repo/a.el"))
    (should (string-match-p "String not found" (plist-get ev :summary)))))

(ert-deftest anvil-session-capture-test-json-false-is-not-an-error ()
  "`is_error: false' decodes to :json-false and must not raise an error row.

Elisp truthiness is the trap here: `:json-false' is non-nil, so a
naive check files every successful tool call as a failure."
  (let ((ev (anvil-session-capture-classify
             (anvil-session-capture-test--payload
              "tool_name" "Edit"
              "tool_input" '(("file_path" . "/repo/a.el"))
              "tool_response" '(("is_error" . :json-false))))))
    (should (eq (plist-get ev :category) 'file))
    (should (equal (plist-get ev :kind) "file_edit"))))

;;;; --- noise / shape -------------------------------------------------------

(ert-deftest anvil-session-capture-test-missing-identifier-yields-nil ()
  "A tool whose input has no identifying field produces no row."
  (should (null (anvil-session-capture-classify
                 (anvil-session-capture-test--payload
                  "tool_name" "Edit" "tool_input" nil))))
  (should (null (anvil-session-capture-classify
                 (anvil-session-capture-test--payload
                  "hook_event_name" "UserPromptSubmit")))))

(ert-deftest anvil-session-capture-test-mcp-tools-are-grouped ()
  "An mcp__ tool lands in the mcp category rather than generic tool-use."
  (let ((ev (anvil-session-capture-classify
             (anvil-session-capture-test--payload
              "tool_name" "mcp__emacs-eval__file-edit"))))
    (should (eq (plist-get ev :category) 'mcp))
    (should (equal (plist-get ev :tool) "mcp__emacs-eval__file-edit"))))

;;;; --- end to end ----------------------------------------------------------

(ert-deftest anvil-session-capture-test-record-writes-searchable-row ()
  "A raw hook JSON string becomes a row retrievable by its file path."
  (skip-unless (anvil-session-store-available-p))
  (let* ((path (make-temp-file "anvil-session-capture-test-" nil ".db"))
         (anvil-session-store-db-path path)
         (anvil-session-store--db nil))
    (unwind-protect
        (progn
          (anvil-session-capture-record
           "s1"
           (concat "{\"hook_event_name\":\"PostToolUse\","
                   "\"tool_name\":\"Edit\","
                   "\"tool_input\":{\"file_path\":\"/repo/anvil-worker.el\"}}"))
          (let ((hits (anvil-session-store-search "anvil-worker"
                                                  :session-id "s1")))
            (should (= (length hits) 1))
            (should (equal (plist-get (car hits) :data)
                           "/repo/anvil-worker.el"))))
      (anvil-session-store-close)
      (dolist (suffix '("" "-wal" "-shm"))
        (let ((f (concat path suffix)))
          (when (file-exists-p f) (ignore-errors (delete-file f))))))))

(ert-deftest anvil-session-capture-test-record-never-signals ()
  "Malformed JSON is dropped quietly — a throwing hook breaks every call."
  (should (null (anvil-session-capture-record "s1" "{not json")))
  (should (null (anvil-session-capture-record "" "{}"))))

(provide 'anvil-session-capture-test)
;;; anvil-session-capture-test.el ends here

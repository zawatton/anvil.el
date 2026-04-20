;;; anvil-git-msg-test.el --- Tests for anvil-git-msg -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 15 Phase 1 — commit / PR message synthesis.
;; Each test builds a fresh synthetic git repo under a temp directory
;; so we never touch anvil's own history.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-git-msg)


;;;; --- fixtures ------------------------------------------------------------

(defun anvil-git-msg-test--sh (dir &rest args)
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %s failed in %s: %s"
               (mapconcat #'identity args " ") dir (buffer-string)))
      (string-trim (buffer-string)))))

(defun anvil-git-msg-test--configure (dir)
  (anvil-git-msg-test--sh dir "config" "user.email" "msg@anvil.test")
  (anvil-git-msg-test--sh dir "config" "user.name"  "Anvil Msg")
  (anvil-git-msg-test--sh dir "config" "commit.gpgsign" "false")
  (anvil-git-msg-test--sh dir "config" "core.autocrlf" "false"))

(defun anvil-git-msg-test--write (path content)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert content)))

(defmacro anvil-git-msg-test--with-repo (repo-var &rest body)
  "Create a fresh git repo bound to REPO-VAR and run BODY.
Makes one baseline commit so the repo has HEAD."
  (declare (indent 1))
  `(let* ((,repo-var (make-temp-file "anvil-git-msg-fxt-" t)))
     (unwind-protect
         (progn
           (anvil-git-msg-test--sh ,repo-var "init" "-q" "-b" "master")
           (anvil-git-msg-test--configure ,repo-var)
           (anvil-git-msg-test--write (expand-file-name "README.md" ,repo-var)
                                      "# fixture\n")
           (anvil-git-msg-test--sh ,repo-var "add" "-A")
           (anvil-git-msg-test--sh ,repo-var "commit" "-m" "init")
           (let ((default-directory (file-name-as-directory ,repo-var)))
             ,@body))
       (ignore-errors (delete-directory ,repo-var t)))))


;;;; --- type inference ------------------------------------------------------

(ert-deftest anvil-git-msg-test/type-test-only ()
  "Staging only test files yields type=test."
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "tests/anvil-foo-test.el" repo) ";; t1\n")
    (anvil-git-msg-test--sh repo "add" "tests/anvil-foo-test.el")
    (let ((draft (anvil-git-msg-commit :repo repo)))
      (should (equal "test" (plist-get draft :type))))))

(ert-deftest anvil-git-msg-test/type-docs-only ()
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "docs/design/99-foo.org" repo) "* foo\n")
    (anvil-git-msg-test--sh repo "add" "docs/design/99-foo.org")
    (let ((draft (anvil-git-msg-commit :repo repo)))
      (should (equal "docs" (plist-get draft :type))))))

(ert-deftest anvil-git-msg-test/type-feat-when-new-file ()
  "Adding a new .el file yields type=feat."
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-greeter.el" repo)
     ";;; anvil-greeter.el --- Hello -*- lexical-binding: t; -*-\n(provide 'anvil-greeter)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-greeter.el")
    (let ((draft (anvil-git-msg-commit :repo repo)))
      (should (equal "feat" (plist-get draft :type))))))

(ert-deftest anvil-git-msg-test/type-fix-for-existing-file-mod ()
  "Modifying only existing code files yields type=fix."
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-thing.el" repo) "(provide 'anvil-thing)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-thing.el")
    (anvil-git-msg-test--sh repo "commit" "-m" "add thing")
    ;; Now modify without adding new files
    (anvil-git-msg-test--write
     (expand-file-name "anvil-thing.el" repo)
     "(defun anvil-thing-fixed () nil)\n(provide 'anvil-thing)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-thing.el")
    (let ((draft (anvil-git-msg-commit :repo repo)))
      (should (equal "fix" (plist-get draft :type))))))


;;;; --- scope inference -----------------------------------------------------

(ert-deftest anvil-git-msg-test/scope-single-module-anvil-prefix ()
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-http.el" repo) "(provide 'anvil-http)\n")
    (anvil-git-msg-test--write
     (expand-file-name "tests/anvil-http-test.el" repo) ";; t\n")
    (anvil-git-msg-test--sh repo "add" "anvil-http.el" "tests/anvil-http-test.el")
    (let ((draft (anvil-git-msg-commit :repo repo)))
      (should (equal "http" (plist-get draft :scope))))))

(ert-deftest anvil-git-msg-test/scope-compound-name ()
  "anvil-orchestrator-routing.el yields scope=\"orchestrator-routing\"."
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-orchestrator-routing.el" repo)
     "(provide 'anvil-orchestrator-routing)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-orchestrator-routing.el")
    (let ((draft (anvil-git-msg-commit :repo repo)))
      (should (equal "orchestrator-routing" (plist-get draft :scope))))))

(ert-deftest anvil-git-msg-test/scope-common-prefix ()
  "Multiple files under the same common prefix collapse to that prefix."
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-orchestrator.el" repo)
     "(provide 'anvil-orchestrator)\n")
    (anvil-git-msg-test--write
     (expand-file-name "anvil-orchestrator-routing.el" repo)
     "(provide 'anvil-orchestrator-routing)\n")
    (anvil-git-msg-test--sh repo "add"
                            "anvil-orchestrator.el"
                            "anvil-orchestrator-routing.el")
    (let ((draft (anvil-git-msg-commit :repo repo)))
      (should (equal "orchestrator" (plist-get draft :scope))))))

(ert-deftest anvil-git-msg-test/scope-override-wins ()
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-http.el" repo) "(provide 'anvil-http)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-http.el")
    (let ((draft (anvil-git-msg-commit :repo repo :scope-override "custom")))
      (should (equal "custom" (plist-get draft :scope))))))


;;;; --- trailers + suggested-full ------------------------------------------

(ert-deftest anvil-git-msg-test/co-authored-by-is-present-by-default ()
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-x.el" repo) "(provide 'anvil-x)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-x.el")
    (let* ((draft (anvil-git-msg-commit :repo repo))
           (trailers (plist-get draft :trailers))
           (full (plist-get draft :suggested-full)))
      (should (assoc "Co-Authored-By" trailers))
      (should (string-match-p "Co-Authored-By: " full)))))

(ert-deftest anvil-git-msg-test/co-authored-by-skipped-when-nil ()
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-x.el" repo) "(provide 'anvil-x)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-x.el")
    (let* ((anvil-git-msg-co-authored-by nil)
           (draft (anvil-git-msg-commit :repo repo))
           (trailers (plist-get draft :trailers))
           (full (plist-get draft :suggested-full)))
      (should (null trailers))
      (should-not (string-match-p "Co-Authored-By" full)))))

(ert-deftest anvil-git-msg-test/suggested-full-has-subject-placeholder ()
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-x.el" repo) "(provide 'anvil-x)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-x.el")
    (let* ((draft (anvil-git-msg-commit :repo repo))
           (full (plist-get draft :suggested-full)))
      (should (string-match-p
               (regexp-quote anvil-git-msg-subject-placeholder) full)))))

(ert-deftest anvil-git-msg-test/commit-returns-nil-when-nothing-staged ()
  (anvil-git-msg-test--with-repo repo
    (should-not (anvil-git-msg-commit :repo repo))))


;;;; --- PR body ------------------------------------------------------------

(ert-deftest anvil-git-msg-test/pr-body-summarises-commits ()
  (anvil-git-msg-test--with-repo repo
    ;; Create branch work, add two commits.
    (anvil-git-msg-test--sh repo "checkout" "-b" "work")
    (anvil-git-msg-test--write (expand-file-name "a.el" repo) ";; a\n")
    (anvil-git-msg-test--sh repo "add" "a.el")
    (anvil-git-msg-test--sh repo "commit" "-m" "feat(a): add a")
    (anvil-git-msg-test--write (expand-file-name "b.el" repo) ";; b\n")
    (anvil-git-msg-test--sh repo "add" "b.el")
    (anvil-git-msg-test--sh repo "commit" "-m" "fix(b): fix b")
    (let* ((pr (anvil-git-msg-pr :base "master" :head "work" :repo repo))
           (commits (plist-get pr :commits))
           (body (plist-get pr :suggested-body)))
      (should (= 2 (length commits)))
      (should (string-match-p "2 commits" body))
      (should (string-match-p "feat(a): add a" body))
      (should (string-match-p "fix(b): fix b" body))
      (should (string-match-p "## Test Plan" body)))))

(ert-deftest anvil-git-msg-test/pr-body-nil-when-range-empty ()
  (anvil-git-msg-test--with-repo repo
    (should-not (anvil-git-msg-pr :base "master" :head "master" :repo repo))))


;;;; --- MCP wrappers -------------------------------------------------------

(ert-deftest anvil-git-msg-test/tool-commit-message-returns-draft ()
  (anvil-git-msg-test--with-repo repo
    (anvil-git-msg-test--write
     (expand-file-name "anvil-http.el" repo) "(provide 'anvil-http)\n")
    (anvil-git-msg-test--sh repo "add" "anvil-http.el")
    (let ((draft (anvil-git-msg--tool-commit-message nil repo)))
      (should (equal "feat" (plist-get draft :type)))
      (should (equal "http" (plist-get draft :scope))))))

(ert-deftest anvil-git-msg-test/tool-commit-message-reports-empty-stage ()
  (anvil-git-msg-test--with-repo repo
    (let ((result (anvil-git-msg--tool-commit-message nil repo)))
      (should (eq nil (plist-get result :staged)))
      (should (stringp (plist-get result :message))))))

(ert-deftest anvil-git-msg-test/tool-pr-body-reports-empty-range ()
  (anvil-git-msg-test--with-repo repo
    (let ((result (anvil-git-msg--tool-pr-body "master" "master" repo)))
      (should (null (plist-get result :commits)))
      (should (stringp (plist-get result :message))))))

(provide 'anvil-git-msg-test)

;;; anvil-git-msg-test.el ends here

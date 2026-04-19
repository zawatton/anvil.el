;;; anvil-bisect-test.el --- Tests for anvil-bisect -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 13 Phase 1.  Each test builds a small synthetic
;; git repository under a temp directory so the bisect loop runs in
;; isolation and does not depend on anvil's own history.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-bisect)


;;;; --- synthetic repo fixture ------------------------------------------

(defun anvil-bisect-test--sh (dir &rest args)
  "Run \"git ARGS\" in DIR; return trimmed stdout or signal."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %s failed in %s: %s"
               (mapconcat #'identity args " ") dir (buffer-string)))
      (string-trim (buffer-string)))))

(defun anvil-bisect-test--configure (dir)
  "Set a minimal git identity + init.defaultBranch in DIR so
`git commit' works in `-Q' batch without touching user config."
  (anvil-bisect-test--sh dir "config" "user.email" "bisect@anvil.test")
  (anvil-bisect-test--sh dir "config" "user.name" "Anvil Bisect")
  (anvil-bisect-test--sh dir "config" "commit.gpgsign" "false"))

(defconst anvil-bisect-test--good-source "\
;;; lib.el --- fixture -*- lexical-binding: t; -*-
;;; Code:
(defun fx-add (a b) (+ a b))
(provide 'lib)
;;; lib.el ends here
")

(defconst anvil-bisect-test--bad-source "\
;;; lib.el --- fixture -*- lexical-binding: t; -*-
;;; Code:
(defun fx-add (a b) (+ a b 1))    ; off-by-one: this is the breaking change
(provide 'lib)
;;; lib.el ends here
")

(defconst anvil-bisect-test--test-source "\
;;; lib-test.el --- fixture tests -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'lib)

(ert-deftest fx-add-is-plus ()
  (should (= 5 (fx-add 2 3))))

(provide 'lib-test)
;;; lib-test.el ends here
")

(defun anvil-bisect-test--write (path content)
  "Write CONTENT to PATH, creating parent dirs as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert content)))

(defun anvil-bisect-test--commit (repo msg)
  "Stage all and commit with message MSG in REPO.  Returns the new SHA."
  (anvil-bisect-test--sh repo "add" "-A")
  (anvil-bisect-test--sh repo "commit" "-m" msg "--allow-empty")
  (anvil-bisect-test--sh repo "rev-parse" "HEAD"))

(defun anvil-bisect-test--build-repo (n-good n-bad)
  "Build a synthetic repo with N-GOOD passing commits, then the breaking
commit, then N-BAD passing-infra / no-op commits after it.
Returns (REPO :good-ref SHA :bad-ref SHA :break-sha SHA)."
  (let* ((repo (make-temp-file "anvil-bisect-fxt-" t))
         (lib-path (expand-file-name "lib.el" repo))
         (test-path (expand-file-name "tests/lib-test.el" repo))
         (break-sha nil)
         (good-ref nil)
         (bad-ref nil))
    (anvil-bisect-test--sh repo "init" "-q" "-b" "main")
    (anvil-bisect-test--configure repo)
    ;; Initial commit — good implementation + test.
    (anvil-bisect-test--write lib-path anvil-bisect-test--good-source)
    (anvil-bisect-test--write test-path anvil-bisect-test--test-source)
    (setq good-ref (anvil-bisect-test--commit repo "initial good"))
    ;; Additional passing commits to widen the range.
    (dotimes (i n-good)
      (anvil-bisect-test--write
       (expand-file-name (format "pad-g%d.txt" i) repo)
       (format "padding %d\n" i))
      (setq good-ref
            (anvil-bisect-test--commit repo (format "pad-good %d" i))))
    ;; The regression commit.
    (anvil-bisect-test--write lib-path anvil-bisect-test--bad-source)
    (setq break-sha (anvil-bisect-test--commit repo "BAD: off-by-one in fx-add"))
    ;; Later passing-infra commits (should still be bad in bisect terms).
    (dotimes (i n-bad)
      (anvil-bisect-test--write
       (expand-file-name (format "pad-b%d.txt" i) repo)
       (format "post-break %d\n" i))
      (setq bad-ref
            (anvil-bisect-test--commit repo (format "pad-after-break %d" i))))
    ;; If n-bad is 0 we want bad-ref to be HEAD (the break itself).
    (unless bad-ref (setq bad-ref break-sha))
    (list repo :good-ref good-ref :bad-ref bad-ref :break-sha break-sha)))

(defun anvil-bisect-test--copy-entry (repo)
  "Copy the subprocess entry file into REPO's tests/ dir."
  (let ((src (locate-library "anvil-bisect-entry")))
    (unless src
      (error "cannot find anvil-bisect-entry.el on load-path"))
    (let ((dst (expand-file-name "tests/anvil-bisect-entry.el" repo)))
      (make-directory (file-name-directory dst) t)
      (copy-file src dst t))))

(defun anvil-bisect-test--with-fixture (n-good n-bad fn)
  "Build a repo, copy the entry, call FN with (repo plist)."
  (let* ((plist (anvil-bisect-test--build-repo n-good n-bad))
         (repo (car plist)))
    (unwind-protect
        (progn
          (anvil-bisect-test--copy-entry repo)
          (funcall fn repo (cdr plist)))
      ;; Reset per-run state and remove the fixture.
      (setq anvil-bisect--active-worktree nil
            anvil-bisect--cancelled nil)
      (when (file-directory-p repo)
        (delete-directory repo t)))))


;;;; --- smoke -----------------------------------------------------------

(ert-deftest anvil-bisect-test-feature-provided ()
  (should (featurep 'anvil-bisect)))

(ert-deftest anvil-bisect-test-enable-disable-callable ()
  (should (fboundp 'anvil-bisect-enable))
  (should (fboundp 'anvil-bisect-disable)))


;;;; --- helper unit tests ----------------------------------------------

(ert-deftest anvil-bisect-test-first-bad-sha-regex ()
  "anvil-bisect--first-bad-sha extracts the SHA from a real bisect log."
  (let ((log (concat
              "git bisect start\n"
              "# status: waiting for both good and bad commits\n"
              "# bad: [abc123def456] BAD: off-by-one\n"
              "# good: [fed321cba654] good baseline\n"
              "# first bad commit: [abc123def456] BAD: off-by-one\n")))
    (should (equal "abc123def456"
                   (anvil-bisect--first-bad-sha log)))))

(ert-deftest anvil-bisect-test-commit-metadata-shape ()
  "anvil-bisect--commit-metadata returns the four documented keys."
  (anvil-bisect-test--with-fixture
   0 0
   (lambda (repo _plist)
     (let* ((sha (anvil-bisect-test--sh repo "rev-parse" "HEAD"))
            (meta (anvil-bisect--commit-metadata repo sha)))
       (should (equal sha (plist-get meta :sha)))
       (should (stringp (plist-get meta :subject)))
       (should (stringp (plist-get meta :author)))
       (should (stringp (plist-get meta :date)))))))


;;;; --- worktree lifecycle ---------------------------------------------

(ert-deftest anvil-bisect-test-worktree-create-destroy ()
  "Creating a detached worktree at BAD leaves the main tree untouched;
destroying it removes both the directory and the git record."
  (anvil-bisect-test--with-fixture
   1 1
   (lambda (repo plist)
     (let* ((bad (plist-get plist :bad-ref))
            (wt (anvil-bisect--create-worktree repo bad)))
       (unwind-protect
           (progn
             (should (file-directory-p wt))
             (should (file-exists-p (expand-file-name "lib.el" wt)))
             (should (string-prefix-p
                      bad
                      (anvil-bisect-test--sh wt "rev-parse" "HEAD"))))
         (anvil-bisect--destroy-worktree repo wt))
       (should-not (file-directory-p wt))))))


;;;; --- end-to-end bisect ----------------------------------------------

(ert-deftest anvil-bisect-test-bisect-identifies-break ()
  "Bisect on a 3-commit range pinpoints the breaking commit's SHA."
  (anvil-bisect-test--with-fixture
   2 2
   (lambda (repo plist)
     (let ((default-directory (file-name-as-directory repo))
           (anvil-bisect-extra-load-path nil))
       (let ((r (anvil-bisect-test
                 'fx-add-is-plus
                 :good "HEAD~4" :bad "HEAD"
                 :test-file "tests/lib-test.el"
                 :timeout-total 120
                 :timeout-per-step 30)))
         (should (eq 'found (plist-get r :status)))
         (should (equal (plist-get plist :break-sha)
                        (plist-get r :breaking-sha)))
         (should (string-match-p "BAD:" (plist-get r :breaking-subject)))
         (should (> (plist-get r :steps) 0)))))))

(ert-deftest anvil-bisect-test-last-result-populated ()
  "After a bisect run, `anvil-bisect-last-result' returns the same plist."
  (anvil-bisect-test--with-fixture
   1 1
   (lambda (repo plist)
     (let ((default-directory (file-name-as-directory repo)))
       (let ((r (anvil-bisect-test
                 'fx-add-is-plus
                 :good "HEAD~2" :bad "HEAD"
                 :test-file "tests/lib-test.el"
                 :timeout-total 120
                 :timeout-per-step 30)))
         (ignore plist)
         (should (equal r (anvil-bisect-last-result))))))))


;;;; --- error paths ---------------------------------------------------

(ert-deftest anvil-bisect-test-requires-test-file ()
  "Omitting :test-file raises immediately, before any worktree is made."
  (should-error (anvil-bisect-test 'foo :good "HEAD~1" :bad "HEAD")))

(ert-deftest anvil-bisect-test-unknown-ref-errors ()
  "An unresolvable :good ref surfaces a git rev-parse error."
  (anvil-bisect-test--with-fixture
   0 0
   (lambda (repo _plist)
     (let ((default-directory (file-name-as-directory repo)))
       (should-error
        (anvil-bisect-test 'fx-add-is-plus
                           :good "no-such-ref"
                           :bad "HEAD"
                           :test-file "tests/lib-test.el"))))))

(ert-deftest anvil-bisect-test-missing-test-skips-not-crashes ()
  "If the named test does not exist on some commit, the step exits
with the git-bisect skip code (125) and the bisect does not crash."
  ;; We reuse the entry directly rather than driving a full bisect.
  (anvil-bisect-test--with-fixture
   0 0
   (lambda (repo _plist)
     (let* ((anvil-bisect-extra-load-path nil)
            (rc (anvil-bisect--run-step
                 repo 'this-test-does-not-exist
                 "tests/lib-test.el" 30)))
       (should (eq 'skip rc))))))


;;;; --- MCP tool surface ---------------------------------------------

(ert-deftest anvil-bisect-test-tool-test-returns-plist ()
  "The MCP tool wrapper returns the same plist shape as the public API."
  (anvil-bisect-test--with-fixture
   1 1
   (lambda (repo plist)
     (let ((default-directory (file-name-as-directory repo)))
       (let ((r (anvil-bisect--tool-test
                 "fx-add-is-plus" "tests/lib-test.el"
                 "HEAD~2" "HEAD" "120" "30")))
         (should (eq 'found (plist-get r :status)))
         (should (equal (plist-get plist :break-sha)
                        (plist-get r :breaking-sha))))))))

(ert-deftest anvil-bisect-test-tool-last-result-mirrors ()
  (anvil-bisect-test--with-fixture
   1 1
   (lambda (repo _plist)
     (let ((default-directory (file-name-as-directory repo)))
       (anvil-bisect-test 'fx-add-is-plus
                          :good "HEAD~2" :bad "HEAD"
                          :test-file "tests/lib-test.el"
                          :timeout-total 120 :timeout-per-step 30)
       (should (equal (anvil-bisect--tool-last-result)
                      (anvil-bisect-last-result)))))))



;;;; --- review-feedback hardening ------------------------------------

(ert-deftest anvil-bisect-test-first-bad-sha-fallback-picks-last ()
  "When `# first bad commit' is absent, the fallback returns the *last*
`# bad:' SHA — the introducing commit — not the first, which is the
initial bad ref."
  (let ((log (concat
              "git bisect start\n"
              "# bad: [aaaaaaaaaaaaaaaa] initial bad ref\n"
              "# good: [bbbbbbbbbbbbbbbb] initial good ref\n"
              "# bad: [cccccccccccccccc] mid-step 1\n"
              "# bad: [dddddddddddddddd] final bad\n")))
    (should (equal "dddddddddddddddd"
                   (anvil-bisect--first-bad-sha log)))))

(ert-deftest anvil-bisect-test-concurrent-bisect-refused ()
  "While `anvil-bisect--active-worktree' is set, a second bisect must
be refused rather than silently starting on top of the first."
  (let ((anvil-bisect--active-worktree "/tmp/fake-worktree"))
    (should-error
     (anvil-bisect-test 'foo :test-file "tests/x.el"))
    (setq anvil-bisect--active-worktree nil)))

(ert-deftest anvil-bisect-test-entry-path-prefers-source ()
  "`anvil-bisect--entry-path' resolves to the .el file, not a .elc,
so the subprocess always runs fresh source even if a stale byte-
compiled cache sits next to it."
  (let ((p (anvil-bisect--entry-path)))
    (should (string-suffix-p "anvil-bisect-entry.el" p))
    (should (not (string-suffix-p ".elc" p)))))


(provide 'anvil-bisect-test)
;;; anvil-bisect-test.el ends here

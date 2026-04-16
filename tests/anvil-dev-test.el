;;; anvil-dev-test.el --- Tests for anvil-dev -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises `anvil-self-sync-check' and its helpers.  The `git'
;; subprocess is stubbed via `cl-letf' so the tests do not depend
;; on the filesystem containing a real anvil worktree.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-dev)

(defun anvil-dev-test--call-process-stub-output (buf text)
  "Insert TEXT into BUF, respecting the call-process conventions for t / nil."
  (let ((dest (cond ((eq buf t) (current-buffer))
                    ((bufferp buf) buf)
                    ((stringp buf) (get-buffer buf))
                    (t nil))))
    (when dest
      (with-current-buffer dest (insert text)))))

(defmacro anvil-dev-test--with-git (replies &rest body)
  "Run BODY with `call-process' (of git) returning preset REPLIES.
REPLIES is a list of (ARGS-MATCHER . OUTPUT) pairs.  First hit wins.
Non-git `call-process' calls still signal exit-status 1."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'call-process)
              (lambda (prog &optional _infile buf _disp &rest args)
                (if (equal prog "git")
                    (let ((match (cl-some (lambda (pair)
                                            (and (funcall (car pair) args)
                                                 pair))
                                          ,replies)))
                      (anvil-dev-test--call-process-stub-output
                       buf (or (cdr match) ""))
                      (if match 0 1))
                  1))))
     ,@body))

(defun anvil-dev-test--args-has (substr)
  "Predicate: true when the CALL-PROCESS args contain SUBSTR."
  (lambda (args) (and (member substr args) t)))

(defun anvil-dev-test--make-dir ()
  (let ((d (make-temp-file "anvil-dev-" t)))
    d))

;;;; --- --git-at -----------------------------------------------------------

(ert-deftest anvil-dev-test-git-at-returns-trimmed-stdout ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (anvil-dev-test--with-git
            (list (cons (anvil-dev-test--args-has "HEAD")
                        "abcdef1234\n"))
          (should (equal "abcdef1234"
                         (anvil-dev--git-at d "rev-parse" "HEAD"))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-git-at-returns-nil-on-failure ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (anvil-dev-test--with-git '()
          (should (null (anvil-dev--git-at d "rev-parse" "HEAD"))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-git-at-rejects-non-directory ()
  (should (null (anvil-dev--git-at "/no/such/dir" "rev-parse" "HEAD")))
  (should (null (anvil-dev--git-at nil "rev-parse" "HEAD"))))

;;;; --- --short-sha --------------------------------------------------------

(ert-deftest anvil-dev-test-short-sha-truncates ()
  (should (equal "abcdef1" (anvil-dev--short-sha "abcdef1234567")))
  (should (equal "abc" (anvil-dev--short-sha "abc")))
  (should (null (anvil-dev--short-sha nil))))

;;;; --- self-sync-check integration ---------------------------------------

(ert-deftest anvil-dev-test-self-sync-check-reports-in-sync ()
  "When installed HEAD equals dev HEAD, :in-sync t and :warning nil."
  (let ((installed (anvil-dev-test--make-dir))
        (dev       (anvil-dev-test--make-dir))
        (anvil-dev-source-path nil))
    (unwind-protect
        (let ((anvil-dev-source-path dev))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed))))
            (anvil-dev-test--with-git
                (list
                 (cons (lambda (args) (member "--porcelain" args)) "")
                 (cons (anvil-dev-test--args-has "--abbrev-ref") "master")
                 (cons (anvil-dev-test--args-has "HEAD")
                       "1111111111111111111111111111111111111111"))
              (let ((res (anvil-self-sync-check)))
                (should (eq t   (plist-get res :in-sync)))
                (should (null   (plist-get res :warning)))
                (should (equal  "1111111111111111111111111111111111111111"
                                (plist-get res :installed-head)))
                (should (equal  "1111111111111111111111111111111111111111"
                                (plist-get res :dev-head)))
                (should (equal  0 (plist-get res :installed-dirty-count)))))))
      (delete-directory installed t)
      (delete-directory dev t))))

(ert-deftest anvil-dev-test-self-sync-check-flags-head-mismatch ()
  "Differing installed / dev HEADs set :in-sync nil and fill :warning."
  (let ((installed (anvil-dev-test--make-dir))
        (dev       (anvil-dev-test--make-dir)))
    (unwind-protect
        (let ((anvil-dev-source-path dev))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed)))
                    ;; Track which dir is asked so we can return
                    ;; different HEADs for installed vs dev.
                    ((symbol-function 'call-process)
                     (lambda (prog &optional _i buf _d &rest args)
                       (when (equal prog "git")
                         (let ((text
                                (cond
                                 ((member "--porcelain" args) "")
                                 ((member "--abbrev-ref" args) "master")
                                 ((equal default-directory
                                         (file-name-as-directory installed))
                                  "aaaaaaa1111111111111111111111111111111111")
                                 (t
                                  "bbbbbbb2222222222222222222222222222222222"))))
                           (anvil-dev-test--call-process-stub-output buf text)
                           0)))))
            (let ((res (anvil-self-sync-check)))
              (should (null (plist-get res :in-sync)))
              (should (stringp (plist-get res :warning)))
              (should (string-match-p "HEAD" (plist-get res :warning))))))
      (delete-directory installed t)
      (delete-directory dev t))))

(ert-deftest anvil-dev-test-self-sync-check-handles-missing-locate ()
  "When anvil-server is not on `load-path' the warning flags it."
  (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) nil)))
    (let* ((anvil-dev-source-path nil)
           (res (anvil-self-sync-check)))
      (should (null (plist-get res :installed-dir)))
      (should (string-match-p "not located" (plist-get res :warning))))))

(ert-deftest anvil-dev-test-self-sync-check-no-dev-path-is-in-sync ()
  "Without `anvil-dev-source-path' we compare nothing, so :in-sync t."
  (let ((installed (anvil-dev-test--make-dir)))
    (unwind-protect
        (let ((anvil-dev-source-path nil))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed))))
            (anvil-dev-test--with-git
                (list
                 (cons (lambda (args) (member "--porcelain" args)) "")
                 (cons (anvil-dev-test--args-has "--abbrev-ref") "master")
                 (cons (anvil-dev-test--args-has "HEAD") "ccccccc"))
              (let ((res (anvil-self-sync-check)))
                (should (eq t (plist-get res :in-sync)))
                (should (null (plist-get res :dev-head)))
                (should (null (plist-get res :warning)))))))
      (delete-directory installed t))))

;;; anvil-dev-test.el ends here

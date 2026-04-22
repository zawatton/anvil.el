;;; anvil-lint-test.el --- Tests for anvil-lint -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 16 Phase 1 — registry + 3 org scanners.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-lint)


;;;; --- helpers ------------------------------------------------------------

(defmacro anvil-lint-test--with-clean-registry (&rest body)
  "Run BODY with a saved-and-restored scanner registry."
  (declare (indent 0))
  `(let ((anvil-lint--scanners nil))
     ,@body))

(defmacro anvil-lint-test--with-tmp-tree (binding &rest body)
  "Bind BINDING to a fresh temp directory, run BODY, then delete the dir.
BINDING form: (DIR-VAR)."
  (declare (indent 1))
  (let ((dir-var (car binding)))
    `(let ((,dir-var (make-temp-file "anvil-lint-test-" t)))
       (unwind-protect
           (progn ,@body)
         (delete-directory ,dir-var t)))))

(defun anvil-lint-test--write (path content)
  "Write CONTENT to PATH, creating parent dirs as needed."
  (let ((dir (file-name-directory path)))
    (unless (file-directory-p dir) (make-directory dir t)))
  (with-temp-file path (insert content)))

(defun anvil-lint-test--findings-for (id findings)
  "Filter FINDINGS keeping only entries whose `:scanner' matches ID."
  (seq-filter (lambda (f) (eq (plist-get f :scanner) id)) findings))


;;;; --- registry tests -----------------------------------------------------

(ert-deftest anvil-lint-test-register-and-list-scanner ()
  "Newly registered scanner shows up in `anvil-lint-scanners' output."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-register-scanner
     :id 'demo/noop
     :family 'demo
     :description "demo scanner"
     :scan (lambda (_files) nil))
    (let ((entry (car (anvil-lint-scanners))))
      (should (eq 'demo/noop (plist-get entry :id)))
      (should (eq 'demo (plist-get entry :family)))
      (should (equal "demo scanner" (plist-get entry :description))))))

(ert-deftest anvil-lint-test-register-rejects-missing-fields ()
  "Registration validates required fields."
  (anvil-lint-test--with-clean-registry
    (should-error (anvil-lint-register-scanner :family 'x :description "y"
                                               :scan #'ignore))
    (should-error (anvil-lint-register-scanner :id 'a :description "y"
                                               :scan #'ignore))
    (should-error (anvil-lint-register-scanner :id 'a :family 'x
                                               :description ""
                                               :scan #'ignore))
    (should-error (anvil-lint-register-scanner :id 'a :family 'x
                                               :description "y"))))

(ert-deftest anvil-lint-test-register-replaces-existing-id ()
  "Re-registering an ID replaces the prior entry rather than duplicating."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-register-scanner
     :id 'dup :family 'demo :description "v1"
     :scan (lambda (_) nil))
    (anvil-lint-register-scanner
     :id 'dup :family 'demo :description "v2"
     :scan (lambda (_) nil))
    (let ((entries (anvil-lint-scanners)))
      (should (= 1 (length entries)))
      (should (equal "v2" (plist-get (car entries) :description))))))

(ert-deftest anvil-lint-test-unregister-scanner-removes ()
  (anvil-lint-test--with-clean-registry
    (anvil-lint-register-scanner
     :id 'x :family 'demo :description "doomed"
     :scan (lambda (_) nil))
    (should (anvil-lint-unregister-scanner 'x))
    (should-not (anvil-lint-unregister-scanner 'x))
    (should (null (anvil-lint-scanners)))))


;;;; --- conflict-markers scanner -------------------------------------------

(ert-deftest anvil-lint-test-conflict-markers-detects-classic-block ()
  "All three markers in a classic <<<<<<< / ======= / >>>>>>> block fire."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-test--with-tmp-tree (root)
      (let ((file (expand-file-name "x.org" root)))
        (anvil-lint-test--write
         file
         (concat
          "* heading\n"
          "<<<<<<< HEAD\n"
          "ours line\n"
          "=======\n"
          "theirs line\n"
          ">>>>>>> branch-foo\n"
          "* tail\n"))
        (anvil-lint--register-builtin-scanners)
        (let* ((all (anvil-lint :family 'lint-org :scope root))
               (hits (anvil-lint-test--findings-for
                      'lint-org/conflict-markers all)))
          (should (= 3 (length hits)))
          (should (equal '(2 4 6)
                         (mapcar (lambda (f) (plist-get f :line)) hits)))
          (should (cl-every (lambda (f) (eq 'error (plist-get f :severity)))
                            hits)))))))

(ert-deftest anvil-lint-test-conflict-markers-ignores-org-tables ()
  "Bare `=======' inside an org table or under a heading is NOT flagged
(scanner anchors on `=======$' alone, not table separators)."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-test--with-tmp-tree (root)
      (let ((file (expand-file-name "y.org" root)))
        (anvil-lint-test--write
         file
         (concat
          "* heading\n"
          "| col1 | col2 |\n"
          "|------+------|\n"
          "| a    | b    |\n"
          "* heading2\n"
          "Some prose with === inside.\n"))
        (anvil-lint--register-builtin-scanners)
        (let ((findings (anvil-lint :family 'lint-org :scope root)))
          (should (null findings)))))))


;;;; --- orphan-ids scanner -------------------------------------------------

(ert-deftest anvil-lint-test-orphan-ids-flags-unreferenced ()
  "An :ID: with no incoming [[id:...]] is reported as info severity."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-test--with-tmp-tree (root)
      (let ((file (expand-file-name "z.org" root)))
        (anvil-lint-test--write
         file
         (concat
          "* one\n"
          "  :PROPERTIES:\n"
          "  :ID:       abc-123\n"
          "  :END:\n"
          "* two\n"
          "  :PROPERTIES:\n"
          "  :ID:       def-456\n"
          "  :END:\n"
          "Link to [[id:abc-123][one]].\n"))
        (anvil-lint--register-builtin-scanners)
        (let* ((all (anvil-lint :family 'lint-org :scope root
                                :severity-min 'info))
               (hits (anvil-lint-test--findings-for
                      'lint-org/orphan-ids all)))
          (should (= 1 (length hits)))
          (should (string-match-p "def-456"
                                  (plist-get (car hits) :message)))
          (should (eq 'info (plist-get (car hits) :severity))))))))

(ert-deftest anvil-lint-test-orphan-ids-cross-file-link-counts ()
  "An [[id:...]] in one file satisfies the :ID: in another."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-test--with-tmp-tree (root)
      (anvil-lint-test--write
       (expand-file-name "a.org" root)
       (concat
        "* anchor\n"
        "  :PROPERTIES:\n"
        "  :ID:       link-target\n"
        "  :END:\n"))
      (anvil-lint-test--write
       (expand-file-name "b.org" root)
       "See [[id:link-target][the anchor]].\n")
      (anvil-lint--register-builtin-scanners)
      (let* ((all (anvil-lint :family 'lint-org :scope root
                              :severity-min 'info))
             (hits (anvil-lint-test--findings-for
                    'lint-org/orphan-ids all)))
        (should (null hits))))))


;;;; --- broken-scheduled scanner -------------------------------------------

(ert-deftest anvil-lint-test-broken-scheduled-flags-bad-repeater ()
  "Scheduled timestamps with garbled repeaters are reported as warning."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-test--with-tmp-tree (root)
      (anvil-lint-test--write
       (expand-file-name "todo.org" root)
       (concat
        "* TODO good\n"
        "  SCHEDULED: <2026-04-22 Wed +1w>\n"
        "* TODO bad\n"
        "  SCHEDULED: <2026-04-22 Wed +w1>\n"))
      (anvil-lint--register-builtin-scanners)
      (let* ((all (anvil-lint :family 'lint-org :scope root))
             (hits (anvil-lint-test--findings-for
                    'lint-org/broken-scheduled all)))
        (should (= 1 (length hits)))
        (should (eq 'warning (plist-get (car hits) :severity)))
        (should (string-match-p "+w1"
                                (plist-get (car hits) :message)))))))

(ert-deftest anvil-lint-test-broken-scheduled-accepts-time-and-repeater ()
  "Common shapes (date + weekday + time + repeater) are not flagged."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-test--with-tmp-tree (root)
      (anvil-lint-test--write
       (expand-file-name "ok.org" root)
       (concat
        "* TODO with-time\n"
        "  SCHEDULED: <2026-04-22 Wed 10:00 +1d>\n"
        "* TODO with-range\n"
        "  SCHEDULED: <2026-04-22 Wed 10:00-11:30 ++3d>\n"
        "* TODO catch-up\n"
        "  SCHEDULED: <2026-04-22 Wed .+1m>\n"))
      (anvil-lint--register-builtin-scanners)
      (let* ((all (anvil-lint :family 'lint-org :scope root))
             (hits (anvil-lint-test--findings-for
                    'lint-org/broken-scheduled all)))
        (should (null hits))))))


;;;; --- main entry filtering -----------------------------------------------

(ert-deftest anvil-lint-test-severity-min-filters-out-lower ()
  "`:severity-min' floor drops lower-rank findings."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-test--with-tmp-tree (root)
      (anvil-lint-test--write
       (expand-file-name "mix.org" root)
       (concat
        "* heading\n"
        "  :PROPERTIES:\n"
        "  :ID:       only-info\n"
        "  :END:\n"
        "<<<<<<< HEAD\n"
        "=======\n"
        ">>>>>>> branch\n"))
      (anvil-lint--register-builtin-scanners)
      (let* ((info-floor (anvil-lint :scope root :severity-min 'info))
             (warn-floor (anvil-lint :scope root :severity-min 'warning))
             (err-floor  (anvil-lint :scope root :severity-min 'error)))
        ;; info floor — orphan-id (info) + 3 conflict-marker (error) = 4
        (should (= 4 (length info-floor)))
        ;; warning floor drops the info finding.
        (should (= 3 (length warn-floor)))
        ;; error floor still keeps the conflict markers.
        (should (= 3 (length err-floor)))))))

(ert-deftest anvil-lint-test-family-filter-restricts-scanners ()
  "`:family' restricts dispatch to that family only."
  (anvil-lint-test--with-clean-registry
    (anvil-lint-register-scanner
     :id 'demo/finding
     :family 'demo
     :description "always returns one finding"
     :severity-default 'warning
     :scan (lambda (_files)
             (list (list :file "synthetic" :line 1
                         :message "demo" :hint "n/a"))))
    (anvil-lint--register-builtin-scanners)
    (anvil-lint-test--with-tmp-tree (root)
      (let ((demo-only (anvil-lint :family 'demo :scope root))
            (org-only (anvil-lint :family 'lint-org :scope root)))
        (should (= 1 (length demo-only)))
        (should (eq 'demo/finding (plist-get (car demo-only) :scanner)))
        (should (null org-only))))))


;;;; --- enable / disable ---------------------------------------------------

(ert-deftest anvil-lint-test-enable-registers-builtin-scanners ()
  "`anvil-lint-enable' brings up the three Phase 1 scanners.  The
inverse `disable' clears them again."
  (anvil-lint-test--with-clean-registry
    (anvil-lint--register-builtin-scanners)
    (let ((ids (mapcar (lambda (e) (plist-get e :id))
                       (anvil-lint-scanners))))
      (should (memq 'lint-org/conflict-markers ids))
      (should (memq 'lint-org/orphan-ids ids))
      (should (memq 'lint-org/broken-scheduled ids)))
    (anvil-lint--unregister-builtin-scanners)
    (should (null (anvil-lint-scanners)))))


(provide 'anvil-lint-test)

;;; anvil-lint-test.el ends here

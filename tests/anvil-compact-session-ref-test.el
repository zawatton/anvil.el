;;; anvil-compact-session-ref-test.el --- Doc 63 Phase 3 wiring -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers the one edit Doc 63 Phase 3 makes to a shipped module:
;; `anvil-compact-snapshot-format' gained an optional SESSION-ID and
;; appends a reference snapshot when opted in.
;;
;; The failure this guards against is not a missing section — it is a
;; restore preamble that throws.  That path runs right after /compact
;; has wiped the model's history, so an error there costs the whole
;; session's continuity.  Hence the degradation tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-compact)
(require 'anvil-session-store)

(defmacro anvil-compact-session-ref-test--with-store (&rest body)
  "Run BODY with a private session-store database."
  (declare (indent 0))
  `(let* ((path (make-temp-file "anvil-compact-ref-test-" nil ".db"))
          (anvil-session-store-db-path path)
          (anvil-session-store--db nil))
     (unwind-protect (progn ,@body)
       (anvil-session-store-close)
       (dolist (suffix '("" "-wal" "-shm"))
         (let ((f (concat path suffix)))
           (when (file-exists-p f) (ignore-errors (delete-file f))))))))

(defconst anvil-compact-session-ref-test--snap
  '(:captured-at "2026-09-19T00:00:00" :percent 47
    :task-summary "doc63 wiring" :branch "feat/doc63")
  "A minimal snapshot plist accepted by `anvil-compact-snapshot-format'.")

(ert-deftest anvil-compact-session-ref-test-off-by-default ()
  "With the opt-in off, no reference section is appended."
  (skip-unless (anvil-session-store-available-p))
  (anvil-compact-session-ref-test--with-store
    (anvil-session-store-put "s1" "file_edit" :category 'file :data "a.el")
    (let ((anvil-compact-append-session-ref nil))
      (let ((text (anvil-compact-snapshot-format
                   anvil-compact-session-ref-test--snap "s1")))
        (should (string-match-p "anvil-compact restore" text))
        (should-not (string-match-p "session_resume" text))))))

(ert-deftest anvil-compact-session-ref-test-appends-when-enabled ()
  "With the opt-in on, the reference snapshot is appended after the digest."
  (skip-unless (anvil-session-store-available-p))
  (anvil-compact-session-ref-test--with-store
    (anvil-session-store-put "s1" "error" :category 'error
                             :data "anvil-worker.el:441"
                             :summary "retry cap exceeded")
    (let ((anvil-compact-append-session-ref t))
      (let ((text (anvil-compact-snapshot-format
                   anvil-compact-session-ref-test--snap "s1")))
        (should (string-match-p "anvil-compact restore" text))
        (should (string-match-p "session_resume" text))
        (should (string-match-p "anvil-worker\\.el:441" text))
        ;; The digest must still come first — the reference block is an
        ;; appendix, not a replacement.
        (should (< (string-match "anvil-compact restore" text)
                   (string-match "session_resume" text)))))))

(ert-deftest anvil-compact-session-ref-test-no-session-id-is-inert ()
  "Enabled but called without a session id, nothing is appended."
  (skip-unless (anvil-session-store-available-p))
  (anvil-compact-session-ref-test--with-store
    (anvil-session-store-put "s1" "file_edit" :category 'file :data "a.el")
    (let ((anvil-compact-append-session-ref t))
      (should-not
       (string-match-p "session_resume"
                       (anvil-compact-snapshot-format
                        anvil-compact-session-ref-test--snap))))))

(ert-deftest anvil-compact-session-ref-test-survives-store-without-sqlite ()
  "A runtime with no SQLite yields the plain preamble, not an error.

This is the NeLisp-standalone case observed on 2026-09-19: the
sqlite-* surface exists but the FFI behind it is not wired, so
`anvil-session-store-available-p' is nil."
  (cl-letf (((symbol-function 'anvil-session-store-available-p)
             (lambda () nil)))
    (let ((anvil-compact-append-session-ref t))
      (let ((text (anvil-compact-snapshot-format
                   anvil-compact-session-ref-test--snap "s1")))
        (should (string-match-p "anvil-compact restore" text))
        (should-not (string-match-p "session_resume" text))))))

(ert-deftest anvil-compact-session-ref-test-survives-throwing-store ()
  "A store that signals must not take the restore preamble down with it."
  (cl-letf (((symbol-function 'anvil-session-store-available-p)
             (lambda () t))
            ((symbol-function 'anvil-session-store-snapshot-ref)
             (lambda (&rest _) (error "database is locked"))))
    (let ((anvil-compact-append-session-ref t))
      (let ((text (anvil-compact-snapshot-format
                   anvil-compact-session-ref-test--snap "s1")))
        (should (string-match-p "anvil-compact restore" text))
        (should-not (string-match-p "session_resume" text))))))

(ert-deftest anvil-compact-session-ref-test-old-arity-still-works ()
  "The one-argument call shipped everywhere keeps its old behaviour."
  (let ((text (anvil-compact-snapshot-format
               anvil-compact-session-ref-test--snap)))
    (should (string-match-p "doc63 wiring" text)))
  (should (null (anvil-compact-snapshot-format nil))))

(provide 'anvil-compact-session-ref-test)
;;; anvil-compact-session-ref-test.el ends here

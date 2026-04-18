;;; anvil-test-fixtures.el --- Shared ERT fixtures for anvil modules -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers used by multiple anvil-*-test.el suites to avoid
;; copy-pasting the same boilerplate: killing tracked subprocesses,
;; spinning up disposable git repos, building stub CLI argv for sh +
;; printf, inspecting the MCP tool registry, and wrapping work under
;; a fresh `anvil-state' database.
;;
;; This library lives under `tests/' — not the top-level package —
;; and is therefore only loaded by tests via `(require
;; 'anvil-test-fixtures)'.  It must not bring in production deps it
;; cannot satisfy (e.g. `anvil-state' is loaded lazily, and
;; `anvil-server' is optional for suites that don't register tools).
;;
;; Convention: every public helper is `anvil-test-fixtures-*' so
;; per-suite helpers (e.g. `anvil-orchestrator-test--wait-batch')
;; stay visibly different.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; --- process cleanup ----------------------------------------------------

(defun anvil-test-fixtures-kill-processes (table &optional timeout)
  "SIGKILL every live process in TABLE and wait for sentinels to settle.

TABLE is a hash-table mapping ID → process object (the same shape
used by `anvil-orchestrator--running' and `anvil-worker--active').
Each process has `anvil-cancel-reason' set to `cancelled' first so
finalize paths that inspect it don't queue an auto-retry; then a
`SIGKILL' is sent, and we wait up to TIMEOUT seconds (default 2)
for the process sentinel to clear TABLE.

Safe to call on an already-empty table."
  (maphash
   (lambda (_id proc)
     (when (process-live-p proc)
       (process-put proc 'anvil-cancel-reason 'cancelled)
       (ignore-errors (signal-process proc 'SIGKILL))))
   table)
  (let ((deadline (+ (float-time) (or timeout 2.0))))
    (while (and (> (hash-table-count table) 0)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

;;;; --- disposable git repos ----------------------------------------------

(defun anvil-test-fixtures-make-repo (&optional initial-branch)
  "Create a throwaway git repo with one commit and return its path.
INITIAL-BRANCH defaults to \"main\".  The repo has a single
`README' file committed under a deterministic test identity so
`git log' output is reproducible."
  (let ((dir (make-temp-file "anvil-test-repo-" t))
        (branch (or initial-branch "main")))
    (cl-labels ((git (&rest args)
                  (let ((code (apply #'call-process "git" nil nil nil
                                     "-C" dir args)))
                    (unless (eql code 0)
                      (error "anvil-test-fixtures: git %s -> exit %s"
                             (mapconcat #'identity args " ") code)))))
      (git "init" "-q" "-b" branch)
      (git "config" "user.email" "t@test")
      (git "config" "user.name"  "test")
      (git "config" "commit.gpgsign" "false")
      (write-region "hello\n" nil (expand-file-name "README" dir))
      (git "add" "README")
      (git "commit" "-q" "-m" "init"))
    dir))

(defun anvil-test-fixtures-destroy-repo (dir)
  "Recursively delete DIR if it exists; convenience wrapper."
  (when (and (stringp dir) (file-directory-p dir))
    (ignore-errors (delete-directory dir t))))

;;;; --- stub CLI argv ------------------------------------------------------

(defun anvil-test-fixtures-stub-cmd (stdout &optional exit-code)
  "Return a `(sh -c SCRIPT)' argv that prints STDOUT and exits.

STDOUT is written via `printf' so embedded newlines round-trip
cleanly.  EXIT-CODE defaults to 0.  The returned list is suitable
for a provider `:build-cmd' or any caller that hands argv to
`make-process'."
  (list "sh" "-c"
        (format "printf '%%s\\n' %s; exit %d"
                (shell-quote-argument (or stdout ""))
                (or exit-code 0))))

(defun anvil-test-fixtures-sleep-stub-cmd (sleep-sec stdout &optional exit-code)
  "Like `anvil-test-fixtures-stub-cmd' but sleeps SLEEP-SEC first.
Used by tests that need to exercise slow / timing-sensitive code
paths (concurrency caps, dep scheduling, timeouts)."
  (list "sh" "-c"
        (format "sleep %s; printf '%%s\\n' %s; exit %d"
                (number-to-string sleep-sec)
                (shell-quote-argument (or stdout ""))
                (or exit-code 0))))

;;;; --- MCP tool registry inspection --------------------------------------

(defun anvil-test-fixtures-registered-tool-ids (server-id)
  "Return the list of tool ids currently registered under SERVER-ID.
SERVER-ID matches what `anvil-server-register-tool' was given.
Returns nil when `anvil-server' is absent or the server-id has
no tools."
  (when (fboundp 'anvil-server--get-server-tools)
    (let ((tbl (anvil-server--get-server-tools server-id))
          ids)
      (when (hash-table-p tbl)
        (maphash (lambda (id _tool) (push id ids)) tbl))
      ids)))

;;;; --- anvil-state fresh DB macro ----------------------------------------

(defmacro anvil-test-fixtures-with-temp-state (&rest body)
  "Run BODY with a fresh `anvil-state' database and clean up on exit.

Binds `anvil-state-db-path' to a new temp file, clears
`anvil-state--db' (so the lazy open picks up the new path),
`anvil-state-enable's the store, then `anvil-state-disable's it
and removes the file afterwards.

Requires `anvil-state' to be loadable; this macro `require's it
at expansion time so callers don't need to."
  (declare (indent 0))
  `(progn
     (require 'anvil-state)
     (let ((anvil-state-db-path (make-temp-file "anvil-test-state-"
                                                nil ".db"))
           (anvil-state--db nil))
       (unwind-protect
           (progn
             (anvil-state-enable)
             ,@body)
         (ignore-errors (anvil-state-disable))
         (ignore-errors (delete-file anvil-state-db-path))))))

(provide 'anvil-test-fixtures)
;;; anvil-test-fixtures.el ends here

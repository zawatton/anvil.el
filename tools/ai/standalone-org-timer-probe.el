;;; standalone-org-timer-probe.el --- org and timer checks on standalone -*- lexical-binding: t; -*-

;; Load after tools/anvil-standalone-mcp.el with
;; ANVIL_STANDALONE_TEST_NO_SERVER=1.  Prints one `ORGTIMER NAME PASS|FAIL'
;; line per check.  Errors are printed with `prin1-to-string' because
;; `format' with %S can itself fail on some standalone builds.

;;; Code:

(defun standalone-org-timer-probe--report (name ok &optional detail)
  (princ (concat "ORGTIMER " name (if ok " PASS" " FAIL")
                 (if detail (concat " " detail) "")
                 "\n")))

(defun standalone-org-timer-probe--error-string (err)
  (let ((s (prin1-to-string err)))
    (if (> (length s) 200) (substring s 0 200) s)))

(defmacro standalone-org-timer-probe--check (name &rest body)
  "Run BODY; report NAME as PASS when it returns non-nil."
  (declare (indent 1))
  `(condition-case err
       (standalone-org-timer-probe--report ,name (progn ,@body))
     (error (standalone-org-timer-probe--report
             ,name nil (standalone-org-timer-probe--error-string err)))))

;;;; org

(standalone-org-timer-probe--check "require-org"
  (require 'org)
  (featurep 'org))

(standalone-org-timer-probe--check "org-buffer-roundtrip"
  (with-temp-buffer
    (insert "* H1\n** TODO H2\nbody\n")
    (goto-char (point-min))
    (and (re-search-forward "^\\*\\* " nil t)
         (= (point) 9)
         (equal (buffer-string) "* H1\n** TODO H2\nbody\n"))))

(standalone-org-timer-probe--check "require-anvil-file"
  (require 'anvil-file)
  (featurep 'anvil-file))

;;;; timers

(defvar standalone-org-timer-probe--hits nil)

(defun standalone-org-timer-probe--pump (secs)
  (let ((deadline (+ (float-time) secs)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05))))

(standalone-org-timer-probe--check "run-with-timer-deferred"
  (setq standalone-org-timer-probe--hits nil)
  (run-with-timer 0.2 nil (lambda () (push 'once standalone-org-timer-probe--hits)))
  ;; Not fired synchronously.
  (and (null standalone-org-timer-probe--hits)
       (progn (standalone-org-timer-probe--pump 0.6)
              (equal standalone-org-timer-probe--hits '(once)))))

(standalone-org-timer-probe--check "run-with-timer-repeat-cancel"
  (setq standalone-org-timer-probe--hits nil)
  (let ((tm (run-with-timer 0.05 0.1 (lambda () (push 'rep standalone-org-timer-probe--hits)))))
    (standalone-org-timer-probe--pump 0.5)
    (let ((n (length standalone-org-timer-probe--hits)))
      (cancel-timer tm)
      (standalone-org-timer-probe--pump 0.3)
      (and (timerp tm)
           (>= n 3)
           (= n (length standalone-org-timer-probe--hits))))))

(standalone-org-timer-probe--check "run-with-idle-timer"
  (setq standalone-org-timer-probe--hits nil)
  (let ((tm (run-with-idle-timer 0.1 nil (lambda () (push 'idle standalone-org-timer-probe--hits)))))
    (standalone-org-timer-probe--pump 0.5)
    (and (timerp tm)
         (equal standalone-org-timer-probe--hits '(idle)))))

;;;; modules whose enable step needed the timer functions

;; Keep their databases out of the real user directory.
(let ((scratch (file-name-as-directory
                (or (getenv "TMPDIR") temporary-file-directory))))
  (setq user-emacs-directory scratch)
  (defvar anvil-state-db-path nil)
  (defvar anvil-org-index-db-path nil)
  (setq anvil-state-db-path (expand-file-name "anvil-state.db" scratch)
        anvil-org-index-db-path (expand-file-name "anvil-org-index.db" scratch)))

(dolist (m '(anvil-browser anvil-claude-watchdog anvil-compact anvil-org-index
             anvil-session anvil-shell-filter anvil-state))
  (let ((name (symbol-name m)))
    (standalone-org-timer-probe--check (concat "enable-" name)
      (require m)
      (funcall (intern (concat name "-enable")))
      t)))

(provide 'standalone-org-timer-probe)
;;; standalone-org-timer-probe.el ends here

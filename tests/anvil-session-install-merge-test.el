;;; anvil-session-install-merge-test.el --- installer merge semantics -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression cover for a destructive bug in `anvil-hook-install-settings'.
;;
;; The planner used to build the new hook block with
;;
;;     (puthash claude-key desired new)
;;
;; which replaces the entire value for every key anvil binds.  On a
;; settings.json where another tool also hooks Stop — the author's
;; machine had a Discord notifier there — running the installer
;; silently deleted it.  Demonstrated on a copy of the real file on
;; 2026-09-19: two commands under Stop before, one after.
;;
;; The first test below is that scenario.  It fails on the old planner
;; and passes on the merging one, which is the only reason to trust the
;; rest of this file.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil-session)

(defconst anvil-session-install-merge-test--foreign
  "python3 /somewhere/notify.py stop"
  "A hook command belonging to a tool that is not anvil.")

(defun anvil-session-install-merge-test--settings-with-foreign ()
  "Return JSON text for a settings file whose Stop hook is foreign."
  (json-encode
   `(("hooks"
      . (("Stop"
          . [(("matcher" . "")
              ("hooks" . [(("type" . "command")
                           ("command" . ,anvil-session-install-merge-test--foreign))]))])))
     ("permissions" . (("allow" . ["Bash(ls:*)"]))))))

(defmacro anvil-session-install-merge-test--with-file (var text &rest body)
  "Bind VAR to a temp settings file seeded with TEXT for BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "anvil-session-merge-" nil ".json")))
     (unwind-protect
         (progn (with-temp-file ,var (insert ,text)) ,@body)
       (ignore-errors (delete-file ,var)))))

(defun anvil-session-install-merge-test--commands (path key)
  "Return every hook command string bound to KEY in the file at PATH."
  (let* ((json (with-temp-buffer
                 (insert-file-contents path)
                 (json-parse-buffer :object-type 'hash-table
                                    :array-type 'array
                                    :null-object :null
                                    :false-object :false)))
         (hooks (gethash "hooks" json))
         (val (and hooks (gethash key hooks)))
         (out nil))
    (dolist (outer (append (or val []) nil) (nreverse out))
      (dolist (inner (append (or (gethash "hooks" outer) []) nil))
        (push (gethash "command" inner) out)))))

;;;; --- the bug ------------------------------------------------------------

(ert-deftest anvil-session-install-merge-test-keeps-foreign-hook ()
  "Installing must not delete another tool's command on a shared key."
  (anvil-session-install-merge-test--with-file path
      (anvil-session-install-merge-test--settings-with-foreign)
    (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook")
    (let ((cmds (anvil-session-install-merge-test--commands path "Stop")))
      (should (member anvil-session-install-merge-test--foreign cmds))
      (should (cl-some (lambda (c) (string-prefix-p "/opt/anvil/anvil-hook" c))
                       cmds)))))

(ert-deftest anvil-session-install-merge-test-uninstall-keeps-foreign ()
  "Uninstalling removes anvil's command and leaves the foreign one."
  (anvil-session-install-merge-test--with-file path
      (anvil-session-install-merge-test--settings-with-foreign)
    (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook")
    (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook"
                                 :uninstall t)
    (let ((cmds (anvil-session-install-merge-test--commands path "Stop")))
      (should (equal cmds (list anvil-session-install-merge-test--foreign))))))

(ert-deftest anvil-session-install-merge-test-untouched-keys-survive ()
  "A hook key anvil does not bind is neither read nor rewritten."
  (anvil-session-install-merge-test--with-file path
      (json-encode
       '(("hooks" . (("Notification"
                      . [(("matcher" . "")
                          ("hooks" . [(("type" . "command")
                                       ("command" . "python3 notify.py"))]))])))))
    (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook")
    (should (equal (anvil-session-install-merge-test--commands
                    path "Notification")
                   '("python3 notify.py")))))

;;;; --- idempotency ---------------------------------------------------------

(ert-deftest anvil-session-install-merge-test-is-idempotent ()
  "Installing twice updates in place instead of appending a duplicate."
  (anvil-session-install-merge-test--with-file path
      (anvil-session-install-merge-test--settings-with-foreign)
    (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook")
    (let ((first (anvil-session-install-merge-test--commands path "Stop")))
      (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook")
      (should (equal first
                     (anvil-session-install-merge-test--commands
                      path "Stop"))))))

(ert-deftest anvil-session-install-merge-test-moved-script-updates-in-place ()
  "Reinstalling from a new path rewrites the old binding, not duplicates it.

A worktree or a reinstall under a different prefix changes the
script path.  Matching on the wrapper basename as well as the full
path is what keeps that from accumulating a second binding on
every move."
  (anvil-session-install-merge-test--with-file path "{}"
    (anvil-hook-install-settings :path path :script "/old/place/anvil-hook")
    (anvil-hook-install-settings :path path :script "/new/place/anvil-hook")
    (let ((cmds (anvil-session-install-merge-test--commands path "Stop")))
      (should (= (length cmds) 1))
      (should (string-prefix-p "/new/place/anvil-hook" (car cmds))))))

;;;; --- Doc 63 capture hook -------------------------------------------------

(ert-deftest anvil-session-install-merge-test-adds-capture-hook ()
  "The capture hook binds beside the Doc 17 hook, not over it.

Both wrappers share the PostToolUse key, so a matcher that treated
any anvil script as interchangeable would let one overwrite the
other."
  (anvil-session-install-merge-test--with-file path "{}"
    (let ((anvil-session-install-capture-hook t)
          (anvil-session-capture-hook-script "/opt/anvil/anvil-capture-hook"))
      (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook")
      (let ((cmds (anvil-session-install-merge-test--commands
                   path "PostToolUse")))
        (should (= (length cmds) 2))
        (should (cl-some (lambda (c)
                           (string-prefix-p "/opt/anvil/anvil-hook " c))
                         cmds))
        (should (member "/opt/anvil/anvil-capture-hook" cmds))))))

(ert-deftest anvil-session-install-merge-test-capture-hook-opt-out ()
  "With the capture hook disabled only the Doc 17 binding is written."
  (anvil-session-install-merge-test--with-file path "{}"
    (let ((anvil-session-install-capture-hook nil))
      (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook")
      (should (= (length (anvil-session-install-merge-test--commands
                          path "PostToolUse"))
                 1)))))

(ert-deftest anvil-session-install-merge-test-session-end-gets-prune ()
  "SessionEnd carries the Doc 17 hook and the capture hook's --prune."
  (anvil-session-install-merge-test--with-file path "{}"
    (let ((anvil-session-install-capture-hook t)
          (anvil-session-capture-hook-script "/opt/anvil/anvil-capture-hook"))
      (anvil-hook-install-settings :path path :script "/opt/anvil/anvil-hook")
      (should (member "/opt/anvil/anvil-capture-hook --prune"
                      (anvil-session-install-merge-test--commands
                       path "SessionEnd"))))))

;;;; --- dry-run -------------------------------------------------------------

(ert-deftest anvil-session-install-merge-test-dry-run-writes-nothing ()
  "A dry run reports a diff and leaves the file byte-identical."
  (anvil-session-install-merge-test--with-file path
      (anvil-session-install-merge-test--settings-with-foreign)
    (let ((before (with-temp-buffer (insert-file-contents path)
                                    (buffer-string)))
          (r (anvil-hook-install-settings :path path :dry-run t
                                          :script "/opt/anvil/anvil-hook")))
      (should-not (plist-get r :applied))
      (should (stringp (plist-get r :diff)))
      (should (equal before (with-temp-buffer (insert-file-contents path)
                                              (buffer-string)))))))

(provide 'anvil-session-install-merge-test)
;;; anvil-session-install-merge-test.el ends here

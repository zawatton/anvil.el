;;; anvil-org-symlink-paths-test.el --- ERT for symlink-alias path comparison -*- lexical-binding: t; -*-

;;; Commentary:

;; `anvil-org--fail-if-modified' and `anvil-org--refresh-file-buffers'
;; compared `buffer-file-name' against the tool's file path with
;; `string='.  A buffer visiting the same file through a symlinked
;; directory (or any path alias) never matches, so:
;;
;;   * the unsaved-changes guard is silently bypassed — a mutating
;;     org tool proceeds disk-first while the aliased buffer holds
;;     unsaved edits, which are then lost on the next revert;
;;   * after a successful disk write the aliased buffer is never
;;     refreshed and silently diverges from disk.
;;
;; The project already has `anvil-org--paths-equal-p' (truename-based)
;; for exactly this; these tests pin its use at both call sites.

;;; Code:

(require 'ert)
(require 'anvil)
(require 'anvil-org)

(defmacro anvil-org-symlink-paths-test--with-alias (&rest body)
  "Run BODY with REAL-FILE and ALIAS-FILE bound to the same org file.
ALIAS-FILE reaches the file through a symlinked directory."
  `(let* ((dir (make-temp-file "anvil-symlink-test" t))
          (real-dir (expand-file-name "real" dir))
          (link-dir (expand-file-name "link" dir))
          (real-file (progn (make-directory real-dir)
                            (expand-file-name "tasks.org" real-dir)))
          (alias-file (progn (make-symbolic-link real-dir link-dir)
                             (expand-file-name "tasks.org" link-dir))))
     (unwind-protect
         (progn
           (with-temp-file real-file
             (insert "* Heading\nBody.\n"))
           ,@body)
       (when-let* ((buf (find-buffer-visiting real-file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (when-let* ((buf (find-buffer-visiting alias-file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory dir t))))

(ert-deftest anvil-org-symlink-paths-test-fail-if-modified-sees-alias ()
  "Unsaved changes in a buffer visiting via a symlink block the edit."
  (anvil-org-symlink-paths-test--with-alias
   (let ((find-file-visit-truename nil))
     (with-current-buffer (find-file-noselect alias-file)
       (goto-char (point-max))
       (insert "unsaved edit\n")
       ;; Buffer records the aliased path, tool receives the real path.
       (should-not (string= (buffer-file-name) real-file))
       (should-error
        (anvil-org--fail-if-modified real-file "test edit")
        :type 'anvil-server-tool-error)))))

(ert-deftest anvil-org-symlink-paths-test-refresh-sees-alias ()
  "A clean buffer visiting via a symlink is refreshed after a disk write."
  (anvil-org-symlink-paths-test--with-alias
   (let ((find-file-visit-truename nil))
     (with-current-buffer (find-file-noselect alias-file)
       (should-not (string= (buffer-file-name) real-file))
       (with-temp-file real-file
         (insert "* Heading\nNew body from disk.\n"))
       (anvil-org--refresh-file-buffers real-file)
       (should (string-match-p "New body from disk"
                               (buffer-string)))))))

(provide 'anvil-org-symlink-paths-test)
;;; anvil-org-symlink-paths-test.el ends here

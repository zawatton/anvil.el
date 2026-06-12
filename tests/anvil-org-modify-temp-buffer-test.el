;;; anvil-org-modify-temp-buffer-test.el --- ERT for disk-first temp buffer hygiene -*- lexical-binding: t; -*-

;;; Commentary:

;; The disk-first path of `anvil-org--modify' used
;; `set-visited-file-name' to associate its temp buffer with the file.
;; That call renames the buffer to the file's base name — yielding a
;; "<basename><2>" buffer whenever the file is already visited — and
;; assigns an auto-save file name to a buffer that only lives for one
;; tool call.  Setting `buffer-file-name' / `buffer-file-truename' /
;; `default-directory' directly gives org and org-id everything they
;; need without registering the temp buffer as an interactive visitor.

;;; Code:

(require 'ert)
(require 'anvil)
(require 'anvil-org)

(ert-deftest anvil-org-modify-temp-buffer-test-no-visitor-side-effects ()
  "The disk-first temp buffer is not renamed and gets no auto-save name."
  (let* ((file (make-temp-file "anvil-modify-test" nil ".org"
                               "* Heading\nBody.\n"))
         (user-buf (find-file-noselect file))
         (observed-name nil)
         (observed-auto-save nil))
    (unwind-protect
        (progn
          ;; Force disk-first even if buffer-first modes are configured.
          (let ((anvil-modes-allow-buffer-modify nil))
            (anvil-org--modify file "test edit" nil
              (goto-char (point-min))
              (setq observed-name (buffer-name)
                    observed-auto-save buffer-auto-save-file-name)))
          ;; Temp buffer keeps its temp name instead of "<basename><2>".
          (should (string-prefix-p " *temp*" observed-name))
          (should-not observed-auto-save)
          ;; The user's buffer kept its name and the edit reached disk.
          (should (equal (buffer-name user-buf)
                         (file-name-nondirectory file)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p ":ID:" (buffer-string)))))
      (when (buffer-live-p user-buf)
        (with-current-buffer user-buf (set-buffer-modified-p nil))
        (kill-buffer user-buf))
      (delete-file file))))

(provide 'anvil-org-modify-temp-buffer-test)
;;; anvil-org-modify-temp-buffer-test.el ends here

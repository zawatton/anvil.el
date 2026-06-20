;;; anvil-wl-maildir.el --- Maildir delivery for anvil-wl -*- lexical-binding: t; -*-

;; Writes raw RFC822 bytes received over IMAP into a Maildir, using the
;; spec's atomic tmp/ -> new/ rename.  Bytes are written binary so message
;; bodies (ISO-2022-JP, 8bit, attachments) reach disk unmangled; the
;; interactive Emacs Wanderlust then reads & decodes them locally.

;;; Code:

(require 'cl-lib)

(defvar anvil-wl-maildir--counter 0
  "Per-process counter for unique Maildir filenames.")

(defun anvil-wl-maildir-ensure (mdir)
  "Ensure MDIR is a Maildir folder (creates tmp/ new/ cur/).  Return MDIR."
  (dolist (sub '("tmp" "new" "cur"))
    (make-directory (expand-file-name sub mdir) t))
  mdir)

(defun anvil-wl-maildir--host ()
  (if (fboundp 'system-name) (or (system-name) "localhost") "localhost"))

(defun anvil-wl-maildir--epoch ()
  (cond ((fboundp 'float-time) (truncate (float-time)))
        ((fboundp 'current-time) (truncate (float-time (current-time))))
        (t 0)))

(defun anvil-wl-maildir--unique ()
  "Return a Maildir-unique base filename."
  (format "%d.%d_%d.%s"
          (anvil-wl-maildir--epoch)
          (if (fboundp 'emacs-pid) (emacs-pid) 0)
          (cl-incf anvil-wl-maildir--counter)
          (anvil-wl-maildir--host)))

(defun anvil-wl-maildir-deliver (mdir raw)
  "Deliver RAW (unibyte RFC822 bytes) into MDIR.  Return the final new/ path.
Writes to tmp/ then renames into new/ (atomic Maildir delivery)."
  (anvil-wl-maildir-ensure mdir)
  (let* ((name (anvil-wl-maildir--unique))
         (tmp (expand-file-name (concat "tmp/" name) mdir))
         (new (expand-file-name (concat "new/" name) mdir))
         (coding-system-for-write
          (if (and (fboundp 'coding-system-p) (coding-system-p 'binary))
              'binary 'no-conversion)))
    (write-region raw nil tmp nil 'silent)
    (rename-file tmp new t)
    new))

(provide 'anvil-wl-maildir)
;;; anvil-wl-maildir.el ends here

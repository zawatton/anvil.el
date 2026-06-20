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

(defun anvil-wl-maildir--extract-message-id (raw)
  "Return the Message-ID header value from RAW message bytes, or nil."
  (let* ((sep (string-match "\r?\n\r?\n" raw))
         (headers (if sep (substring raw 0 sep) raw)))
    (when (string-match "^Message-I[Dd]:[ \t]*\\(.*\\)" headers)
      (string-trim (match-string 1 headers)))))

(defun anvil-wl-maildir-message-ids (mdir)
  "Return a hash table whose keys are the Message-IDs present in MDIR.
Only the first 8 KB of each file is read (enough to reach Message-ID)."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (sub '("new" "cur"))
      (let ((d (expand-file-name sub mdir)))
        (when (file-directory-p d)
          (dolist (f (directory-files d t "^[^.]"))
            (when (file-regular-p f)
              (let* ((raw (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (let ((coding-system-for-read 'binary))
                              (insert-file-contents-literally f nil 0 8192))
                            (buffer-string)))
                     (mid (anvil-wl-maildir--extract-message-id raw)))
                (when mid (puthash mid t seen))))))))
    seen))

(defun anvil-wl-maildir-count (mdir)
  "Return the number of message files in MDIR's new/ and cur/."
  (let ((n 0))
    (dolist (sub '("new" "cur"))
      (let ((d (expand-file-name sub mdir)))
        (when (file-directory-p d)
          (setq n (+ n (length (directory-files d nil "^[^.]")))))))
    n))

(provide 'anvil-wl-maildir)
;;; anvil-wl-maildir.el ends here

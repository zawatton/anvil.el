;;; anvil-wl-filter.el --- Local Maildir filters for anvil-wl -*- lexical-binding: t; -*-

;;; Commentary:

;; Small local filters over Maildir folders populated by anvil-wl sync.
;; Rules copy matching messages into another Maildir and de-duplicate by
;; Message-ID, leaving source folders untouched.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-wl)
(require 'anvil-wl-maildir)

(defgroup anvil-wl-filter nil
  "Local Maildir filtering helpers for anvil-wl."
  :group 'anvil-wl
  :prefix "anvil-wl-filter-")

(defcustom anvil-wl-filter-default-max nil
  "Default maximum number of newest messages inspected per source folder.
Nil means inspect every local message in the source folder."
  :type '(choice (const nil) integer)
  :group 'anvil-wl-filter)

(defun anvil-wl-filter--match-string-p (regexp string)
  "Return non-nil when REGEXP is nil or matches STRING."
  (or (null regexp)
      (let ((case-fold-search t))
        (string-match-p regexp (or string "")))))

(defun anvil-wl-filter--message-text (path)
  "Return decoded searchable text for message PATH."
  (let* ((headers (anvil-wl--slurp path t))
         (from (anvil-wl--decode-hdr (anvil-wl--field headers "From")))
         (to (anvil-wl--decode-hdr (anvil-wl--field headers "To")))
         (subject (anvil-wl--decode-hdr (anvil-wl--field headers "Subject")))
         (body (anvil-wl--decode-body path)))
    (list :headers headers
          :from from
          :to to
          :subject subject
          :body body
          :text (string-join (list from to subject body headers) "\n"))))

(defun anvil-wl-filter--message-matches-p (path rule)
  "Return non-nil when message PATH matches RULE."
  (let* ((headers (anvil-wl--slurp path t))
         (from (anvil-wl--decode-hdr (anvil-wl--field headers "From")))
         (to (anvil-wl--decode-hdr (anvil-wl--field headers "To")))
         (subject (anvil-wl--decode-hdr (anvil-wl--field headers "Subject"))))
    (and (anvil-wl-filter--match-string-p
          (plist-get rule :from-regexp) from)
         (anvil-wl-filter--match-string-p
          (plist-get rule :to-regexp) to)
         (anvil-wl-filter--match-string-p
          (plist-get rule :subject-regexp) subject)
         (anvil-wl-filter--match-string-p
          (plist-get rule :header-regexp) headers)
         (let ((body (and (or (plist-get rule :body-regexp)
                              (plist-get rule :regexp))
                          (anvil-wl--decode-body path))))
           (and (anvil-wl-filter--match-string-p
                 (plist-get rule :body-regexp) body)
                (anvil-wl-filter--match-string-p
                 (plist-get rule :regexp)
                 (when (plist-get rule :regexp)
                   (string-join (list from to subject body headers) "\n"))))))))

;;;###autoload
(cl-defun anvil-wl-filter-copy-matches (&key root source-folder dest-folder
                                             regexp from-regexp to-regexp
                                             subject-regexp body-regexp
                                             header-regexp max)
  "Copy messages matching the given regexps from SOURCE-FOLDER to DEST-FOLDER.
ROOT defaults to `anvil-wl-maildir-root'.  DEST-FOLDER and SOURCE-FOLDER are
relative Maildir folder names.  MAX limits the newest source messages inspected.
Matching messages are copied, not moved, and de-duplicated by Message-ID."
  (unless (and source-folder dest-folder)
    (error "anvil-wl-filter-copy-matches: :source-folder and :dest-folder are required"))
  (let* ((root (expand-file-name (or root anvil-wl-maildir-root)))
         (anvil-wl-maildir-root root)
         (dest-dir (expand-file-name dest-folder root))
         (dest-message-ids (anvil-wl-maildir-message-ids dest-dir))
         (rule (list :regexp regexp
                     :from-regexp from-regexp
                     :to-regexp to-regexp
                     :subject-regexp subject-regexp
                     :body-regexp body-regexp
                     :header-regexp header-regexp))
         (entries (anvil-wl--sorted-entries source-folder))
         (limit (or max anvil-wl-filter-default-max))
         (checked 0)
         (matched 0)
         (copied 0)
         (skipped 0))
    (anvil-wl-maildir-ensure dest-dir)
    (when limit
      (setq entries (cl-subseq entries 0 (min limit (length entries)))))
    (dolist (entry entries)
      (cl-incf checked)
      (let* ((path (car entry))
             (raw (anvil-wl--slurp path))
             (mid (anvil-wl-maildir--extract-message-id raw)))
        (when (anvil-wl-filter--message-matches-p path rule)
          (cl-incf matched)
          (if (and mid (gethash mid dest-message-ids))
              (cl-incf skipped)
            (anvil-wl-maildir-deliver dest-dir raw)
            (cl-incf copied)
            (when mid (puthash mid t dest-message-ids))))))
    (list :source source-folder
          :destination dest-folder
          :checked checked
          :matched matched
          :copied copied
          :skipped skipped)))

;;;###autoload
(defun anvil-wl-filter-apply-rules (rules &optional root)
  "Apply filter RULES under ROOT.
Each entry is (NAME . PLIST).  PLIST accepts :source-folders, :dest-folder,
:regexp, :from-regexp, :to-regexp, :subject-regexp, :body-regexp,
:header-regexp and :max."
  (mapcar
   (lambda (entry)
     (let* ((name (car entry))
            (rule (cdr entry))
            (sources (plist-get rule :source-folders))
            (dest (plist-get rule :dest-folder)))
       (cons name
             (mapcar
              (lambda (source)
                (anvil-wl-filter-copy-matches
                 :root root
                 :source-folder source
                 :dest-folder dest
                 :regexp (plist-get rule :regexp)
                 :from-regexp (plist-get rule :from-regexp)
                 :to-regexp (plist-get rule :to-regexp)
                 :subject-regexp (plist-get rule :subject-regexp)
                 :body-regexp (plist-get rule :body-regexp)
                 :header-regexp (plist-get rule :header-regexp)
                 :max (plist-get rule :max)))
              sources))))
   rules))

(provide 'anvil-wl-filter)
;;; anvil-wl-filter.el ends here

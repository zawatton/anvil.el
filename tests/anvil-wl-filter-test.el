;;; anvil-wl-filter-test.el --- Tests for anvil-wl local filters -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'anvil-wl-filter)

(defun anvil-wl-filter-test--write (folder name content)
  "Write CONTENT to FOLDER/new/NAME."
  (let ((path (expand-file-name (concat "new/" name) folder))
        (coding-system-for-write 'binary))
    (make-directory (file-name-directory path) t)
    (write-region (encode-coding-string content 'utf-8) nil path nil 'silent)))

(ert-deftest anvil-wl-filter-test-copy-matches-and-dedupes ()
  (let* ((root (make-temp-file "anvil-wl-filter-test-" t))
         (anvil-wl-maildir-root root)
         (src (expand-file-name "outlook/INBOX" root))
         (dest (expand-file-name "monitor/INBOX" root)))
    (unwind-protect
        (progn
          (anvil-wl-maildir-ensure src)
          (anvil-wl-filter-test--write
           src "monitor"
           (concat
            "From: center@mx1.musashi-net.jp\n"
            "To: me@example.jp\n"
            "Subject: alert\n"
            "Date: Fri, 26 Jun 2026 15:00:00 +0900\n"
            "Message-ID: <monitor@example.jp>\n"
            "X-MBS2-ID: 182631;32;2026/06/26 15:00:00;2;6;2;3;57\n"
            "Content-Type: text/plain; charset=UTF-8\n"
            "\n"
            "漏電\n三相Ior 57[mA]\n"))
          (anvil-wl-filter-test--write
           src "normal"
           (concat
            "From: center@mx1.musashi-net.jp\n"
            "To: me@example.jp\n"
            "Subject: normal\n"
            "Date: Fri, 26 Jun 2026 15:01:00 +0900\n"
            "Message-ID: <normal@example.jp>\n"
            "Content-Type: text/plain; charset=UTF-8\n"
            "\n"
            "ordinary mail\n"))
          (should (equal '(:source "outlook/INBOX"
                           :destination "monitor/INBOX"
                           :checked 2 :matched 1 :copied 1 :skipped 0)
                         (anvil-wl-filter-copy-matches
                          :source-folder "outlook/INBOX"
                          :dest-folder "monitor/INBOX"
                          :from-regexp "\\`center@mx1\\.musashi-net\\.jp\\'"
                          :header-regexp "^X-MBS2-ID:"
                          :body-regexp "\\(Ior\\|漏電\\)")))
          (should (= 1 (anvil-wl-maildir-count dest)))
          (should (equal '(:source "outlook/INBOX"
                           :destination "monitor/INBOX"
                           :checked 2 :matched 1 :copied 0 :skipped 1)
                         (anvil-wl-filter-copy-matches
                          :source-folder "outlook/INBOX"
                          :dest-folder "monitor/INBOX"
                          :from-regexp "\\`center@mx1\\.musashi-net\\.jp\\'"
                          :header-regexp "^X-MBS2-ID:"
                          :body-regexp "\\(Ior\\|漏電\\)"))))
      (ignore-errors (delete-directory root t)))))

(provide 'anvil-wl-filter-test)
;;; anvil-wl-filter-test.el ends here

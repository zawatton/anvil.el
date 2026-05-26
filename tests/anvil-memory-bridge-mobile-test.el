;;; anvil-memory-bridge-mobile-test.el --- Mobile scaffold static tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Static coverage for Doc 41 Phase 4 mobile thin client scaffold.

;;; Code:

(require 'ert)
(require 'json)
(require 'subr-x)

(defconst anvil-memory-bridge-mobile-test--root
  (expand-file-name "../extensions/mobile-bridge/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name)))
  "Memory bridge mobile scaffold root.")

(defun anvil-memory-bridge-mobile-test--file (name)
  "Return mobile scaffold file NAME."
  (expand-file-name name anvil-memory-bridge-mobile-test--root))

(defun anvil-memory-bridge-mobile-test--read (name)
  "Return mobile scaffold file NAME content."
  (with-temp-buffer
    (insert-file-contents (anvil-memory-bridge-mobile-test--file name))
    (buffer-string)))

(defun anvil-memory-bridge-mobile-test--manifest ()
  "Return decoded mobile web manifest."
  (json-parse-string
   (anvil-memory-bridge-mobile-test--read "app.webmanifest")
   :object-type 'alist
   :array-type 'list))

(ert-deftest anvil-memory-bridge-mobile-test/files-exist ()
  "Mobile scaffold files are present."
  (dolist (name '("README.md" "app.js" "app.webmanifest" "index.html"))
    (should (file-exists-p (anvil-memory-bridge-mobile-test--file name)))))

(ert-deftest anvil-memory-bridge-mobile-test/manifest-shape ()
  "Mobile web manifest is a standalone Pattern A app."
  (let ((manifest (anvil-memory-bridge-mobile-test--manifest)))
    (should (equal "Anvil Mobile Memory" (alist-get 'name manifest)))
    (should (equal "standalone" (alist-get 'display manifest)))
    (should (equal "./index.html" (alist-get 'start_url manifest)))))

(ert-deftest anvil-memory-bridge-mobile-test/app-uses-bridge-api ()
  "Mobile app calls the expected bridge endpoints."
  (let ((source (anvil-memory-bridge-mobile-test--read "app.js")))
    (should (string-match-p "/memory/search" source))
    (should (string-match-p "/memory/save" source))
    (should (string-match-p "/memory/health" source))
    (should (string-match-p "Authorization" source))))

(ert-deftest anvil-memory-bridge-mobile-test/offline-queue-signals ()
  "Mobile app includes localStorage offline queue and replay controls."
  (let ((source (anvil-memory-bridge-mobile-test--read "app.js"))
        (html (anvil-memory-bridge-mobile-test--read "index.html")))
    (should (string-match-p "localStorage" source))
    (should (string-match-p "QUEUE_KEY" source))
    (should (string-match-p "flushQueue" source))
    (should (string-match-p "online" source))
    (should (string-match-p "Replay Queue" html))))

(provide 'anvil-memory-bridge-mobile-test)

;;; anvil-memory-bridge-mobile-test.el ends here

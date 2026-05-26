;;; anvil-memory-bridge-extension-test.el --- Browser extension static tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Static coverage for Doc 41 Phase 3 browser extension scaffold.

;;; Code:

(require 'ert)
(require 'json)
(require 'subr-x)

(defconst anvil-memory-bridge-extension-test--root
  (expand-file-name "../extensions/memory-bridge/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name)))
  "Memory bridge extension fixture root.")

(defun anvil-memory-bridge-extension-test--file (name)
  "Return extension file NAME."
  (expand-file-name name anvil-memory-bridge-extension-test--root))

(defun anvil-memory-bridge-extension-test--read (name)
  "Return extension file NAME content."
  (with-temp-buffer
    (insert-file-contents (anvil-memory-bridge-extension-test--file name))
    (buffer-string)))

(defun anvil-memory-bridge-extension-test--manifest ()
  "Return decoded extension manifest."
  (json-parse-string
   (anvil-memory-bridge-extension-test--read "manifest.json")
   :object-type 'alist
   :array-type 'list))

(ert-deftest anvil-memory-bridge-extension-test/manifest-v3-shape ()
  "Manifest is MV3 and references existing scripts."
  (let* ((manifest (anvil-memory-bridge-extension-test--manifest))
         (background (alist-get 'background manifest))
         (scripts (alist-get 'content_scripts manifest)))
    (should (= 3 (alist-get 'manifest_version manifest)))
    (should (equal "service_worker.js"
                   (alist-get 'service_worker background)))
    (should (file-exists-p
             (anvil-memory-bridge-extension-test--file
              (alist-get 'service_worker background))))
    (should (equal "options.html" (alist-get 'options_page manifest)))
    (should (file-exists-p
             (anvil-memory-bridge-extension-test--file
              (alist-get 'options_page manifest))))
    (dolist (script (alist-get 'js (car scripts)))
      (should (file-exists-p
               (anvil-memory-bridge-extension-test--file script))))))

(ert-deftest anvil-memory-bridge-extension-test/manifest-hosts-ai-chats ()
  "Manifest matches the intended browser chat surfaces and bridge hosts."
  (let* ((manifest (anvil-memory-bridge-extension-test--manifest))
         (matches (alist-get 'matches
                             (car (alist-get 'content_scripts manifest))))
         (hosts (alist-get 'host_permissions manifest)))
    (dolist (match '("https://claude.ai/*"
                     "https://chatgpt.com/*"
                     "https://gemini.google.com/*"))
      (should (member match matches)))
    (should (member "http://127.0.0.1:8730/*" hosts))
    (should (member "http://localhost:8730/*" hosts))))

(ert-deftest anvil-memory-bridge-extension-test/service-worker-uses-bridge-api ()
  "Service worker calls the expected bridge endpoints."
  (let ((source (anvil-memory-bridge-extension-test--read
                 "service_worker.js")))
    (should (string-match-p "/memory/search" source))
    (should (string-match-p "/memory/save" source))
    (should (string-match-p "/memory/health" source))
    (should (string-match-p "Authorization" source))))

(ert-deftest anvil-memory-bridge-extension-test/content-script-shortcuts ()
  "Content script exposes keyboard shortcuts for inject and save."
  (let ((source (anvil-memory-bridge-extension-test--read
                 "content_script.js")))
    (should (string-match-p "search-memory" source))
    (should (string-match-p "save-memory" source))
    (should (string-match-p "ctrlKey" source))
    (should (string-match-p "altKey" source))))

(provide 'anvil-memory-bridge-extension-test)

;;; anvil-memory-bridge-extension-test.el ends here

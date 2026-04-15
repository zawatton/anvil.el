;;; anvil-file-test.el --- Tests for anvil-file helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for json-object-add, file-ensure-import, and file-batch-across.
;; Uses temporary files; no dependency on anvil-server registration.

;;; Code:

(require 'ert)
(require 'anvil-file)

(defun anvil-file-test--with-tmp (content fn)
  "Write CONTENT to a temp file, call FN with its path, then clean up."
  (let ((path (make-temp-file "anvil-file-test-" nil ".txt")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil path nil 'silent))
          (funcall fn path))
      (when (file-exists-p path) (delete-file path)))))

(defun anvil-file-test--read (path)
  "Return PATH's contents as a UTF-8 string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (buffer-string)))

;;;; --- json-object-add ------------------------------------------------------

(ert-deftest anvil-file-test-json-add-empty-object ()
  "Add pairs to an empty JSON object."
  (anvil-file-test--with-tmp
   "{}\n"
   (lambda (path)
     (let ((res (anvil-json-object-add
                 path '(("hello" . "world") ("foo" . "bar")))))
       (should (= 2 (plist-get res :added)))
       (let ((parsed (json-parse-string
                      (anvil-file-test--read path)
                      :object-type 'alist)))
         (should (equal "world" (alist-get 'hello parsed)))
         (should (equal "bar" (alist-get 'foo parsed))))))))

(ert-deftest anvil-file-test-json-add-populated-object ()
  "Add pairs to an object that already has entries; trailing comma auto-added."
  (anvil-file-test--with-tmp
   "{\n  \"existing\": \"yes\"\n}\n"
   (lambda (path)
     (let ((res (anvil-json-object-add
                 path '(("new1" . "a") ("new2" . "b")))))
       (should (= 2 (plist-get res :added)))
       (let ((parsed (json-parse-string
                      (anvil-file-test--read path)
                      :object-type 'alist)))
         (should (equal "yes" (alist-get 'existing parsed)))
         (should (equal "a" (alist-get 'new1 parsed)))
         (should (equal "b" (alist-get 'new2 parsed))))))))

(ert-deftest anvil-file-test-json-duplicate-skip ()
  "Duplicate keys are skipped by default."
  (anvil-file-test--with-tmp
   "{\n  \"k\": \"v1\"\n}\n"
   (lambda (path)
     (let ((res (anvil-json-object-add
                 path '(("k" . "v2") ("new" . "x")))))
       (should (= 1 (plist-get res :added)))
       (should (= 1 (plist-get res :skipped)))
       ;; Original value preserved
       (let ((parsed (json-parse-string
                      (anvil-file-test--read path)
                      :object-type 'alist)))
         (should (equal "v1" (alist-get 'k parsed))))))))

(ert-deftest anvil-file-test-json-duplicate-overwrite ()
  "on-duplicate 'overwrite replaces existing values."
  (anvil-file-test--with-tmp
   "{\n  \"k\": \"v1\",\n  \"other\": \"z\"\n}\n"
   (lambda (path)
     (let ((res (anvil-json-object-add
                 path '(("k" . "v2"))
                 '(:on-duplicate overwrite))))
       (should (= 1 (plist-get res :overwritten)))
       (let ((parsed (json-parse-string
                      (anvil-file-test--read path)
                      :object-type 'alist)))
         (should (equal "v2" (alist-get 'k parsed)))
         (should (equal "z" (alist-get 'other parsed))))))))

(ert-deftest anvil-file-test-json-duplicate-error ()
  "on-duplicate 'error raises an error."
  (anvil-file-test--with-tmp
   "{\n  \"k\": \"v\"\n}\n"
   (lambda (path)
     (should-error
      (anvil-json-object-add
       path '(("k" . "v2"))
       '(:on-duplicate error))))))

(ert-deftest anvil-file-test-json-escape ()
  "Keys and values with special chars are JSON-escaped."
  (anvil-file-test--with-tmp
   "{}\n"
   (lambda (path)
     (anvil-json-object-add
      path
      '(("with\"quote" . "has\\backslash and \"quotes\"")
        ("newlines" . "line1\nline2")))
     ;; Use hash-table form so keys can be arbitrary strings
     (let ((parsed (json-parse-string
                    (anvil-file-test--read path)
                    :object-type 'hash-table)))
       (should (equal "has\\backslash and \"quotes\""
                      (gethash "with\"quote" parsed)))
       (should (equal "line1\nline2"
                      (gethash "newlines" parsed)))))))

(ert-deftest anvil-file-test-json-indent-detection ()
  "Indentation is detected from the first existing entry."
  (anvil-file-test--with-tmp
   "{\n    \"a\": \"1\"\n}\n"     ; 4-space indent
   (lambda (path)
     (anvil-json-object-add path '(("b" . "2")))
     (let ((content (anvil-file-test--read path)))
       (should (string-match-p "^    \"b\"" content))))))

;;;; --- file-ensure-import --------------------------------------------------

(ert-deftest anvil-file-test-ensure-import-already-present ()
  "No-op when the line already exists."
  (anvil-file-test--with-tmp
   "import foo from 'foo';\nimport bar from 'bar';\n\nconst x = 1;\n"
   (lambda (path)
     (let ((res (anvil-file-ensure-import
                 path "import foo from 'foo';")))
       (should (plist-get res :already-present))
       (should-not (plist-get res :inserted))))))

(ert-deftest anvil-file-test-ensure-import-insert-after-last ()
  "Insert after the last matching import line."
  (anvil-file-test--with-tmp
   "import a from 'a';\nimport b from 'b';\n\nconst x = 1;\n"
   (lambda (path)
     (let ((res (anvil-file-ensure-import
                 path "import c from 'c';")))
       (should (plist-get res :inserted))
       (let ((content (anvil-file-test--read path)))
         ;; New import appears right after 'import b'
         (should (string-match-p
                  "import b from 'b';\nimport c from 'c';"
                  content)))))))

(ert-deftest anvil-file-test-ensure-import-no-match-inserts-top ()
  "Insert at top when no import lines exist."
  (anvil-file-test--with-tmp
   "const x = 1;\n"
   (lambda (path)
     (let ((res (anvil-file-ensure-import
                 path "import a from 'a';")))
       (should (plist-get res :inserted))
       (should (= 1 (plist-get res :line)))
       (should (string-prefix-p "import a from 'a';\n"
                                (anvil-file-test--read path)))))))

(ert-deftest anvil-file-test-ensure-import-custom-regex ()
  "Custom :after-regex allows non-import section headers."
  (anvil-file-test--with-tmp
   "(require 'cl-lib)\n(require 'subr-x)\n\n(defun foo () nil)\n"
   (lambda (path)
     (anvil-file-ensure-import
      path "(require 'json)"
      '(:after-regex "^(require "))
     (let ((content (anvil-file-test--read path)))
       (should (string-match-p
                "(require 'subr-x)\n(require 'json)"
                content))))))

;;;; --- file-batch-across ---------------------------------------------------

(ert-deftest anvil-file-test-batch-across-basic ()
  "Run simple replace across two files."
  (let ((p1 (make-temp-file "abf-" nil ".txt"))
        (p2 (make-temp-file "abf-" nil ".txt")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "hello foo\n" nil p1 nil 'silent)
            (write-region "world foo\n" nil p2 nil 'silent))
          (let ((res (anvil-file-batch-across
                      `(((path . ,p1)
                         (operations . (((op . "replace")
                                         (old . "foo")
                                         (new . "BAR")))))
                        ((path . ,p2)
                         (operations . (((op . "replace")
                                         (old . "foo")
                                         (new . "BAZ")))))))))
            (should (= 2 (plist-get res :files)))
            (should (= 2 (plist-get res :succeeded)))
            (should (= 0 (plist-get res :failed)))
            (should (equal "hello BAR\n" (anvil-file-test--read p1)))
            (should (equal "world BAZ\n" (anvil-file-test--read p2)))))
      (when (file-exists-p p1) (delete-file p1))
      (when (file-exists-p p2) (delete-file p2)))))

(ert-deftest anvil-file-test-batch-across-partial-failure ()
  "One file's failure does not block the others."
  (let ((p1 (make-temp-file "abf-" nil ".txt"))
        (p2 (make-temp-file "abf-" nil ".txt")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "no match here\n" nil p1 nil 'silent)
            (write-region "has token\n" nil p2 nil 'silent))
          (let ((res (anvil-file-batch-across
                      `(((path . ,p1)
                         (operations . (((op . "replace")
                                         (old . "NOPE")
                                         (new . "X")))))
                        ((path . ,p2)
                         (operations . (((op . "replace")
                                         (old . "token")
                                         (new . "DONE")))))))))
            (should (= 1 (plist-get res :succeeded)))
            (should (= 1 (plist-get res :failed)))
            (should (equal "has DONE\n" (anvil-file-test--read p2)))))
      (when (file-exists-p p1) (delete-file p1))
      (when (file-exists-p p2) (delete-file p2)))))

;;;; --- :warnings integration (Doc 05 Phase 2) -----------------------------

(ert-deftest anvil-file-test-read-warnings-empty-without-buffer ()
  "anvil-file-read returns :warnings nil when no buffer visits the file."
  (anvil-file-test--with-tmp
   "hello\n"
   (lambda (path)
     (let ((res (anvil-file-read path)))
       (should (null (plist-get res :warnings)))
       (should (equal "hello\n" (plist-get res :content)))))))

(ert-deftest anvil-file-test-read-warnings-flag-buffer-newer ()
  "anvil-file-read surfaces a warning when a visited buffer is dirty."
  (anvil-file-test--with-tmp
   "hello\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (let* ((res (anvil-file-read path))
                    (ws  (plist-get res :warnings)))
               (should (= 1 (length ws)))
               (should (string-match-p "buffer-newer" (car ws)))
               ;; Disk content unchanged.
               (should (equal "hello\n" (plist-get res :content)))))
         (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest anvil-file-test-replace-string-warnings-empty ()
  "anvil-file-replace-string returns :warnings nil when no buffer visits."
  (anvil-file-test--with-tmp
   "alpha beta gamma\n"
   (lambda (path)
     (let ((res (anvil-file-replace-string path "beta" "BETA")))
       (should (= 1 (plist-get res :replaced)))
       (should (null (plist-get res :warnings)))
       (should (equal "alpha BETA gamma\n"
                      (anvil-file-test--read path)))))))

(ert-deftest anvil-file-test-replace-string-warnings-flag-buffer-newer ()
  "anvil-file-replace-string surfaces divergence but still writes disk."
  (anvil-file-test--with-tmp
   "alpha beta gamma\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (let* ((res (anvil-file-replace-string path "beta" "BETA"))
                    (ws  (plist-get res :warnings)))
               (should (= 1 (plist-get res :replaced)))
               (should (= 1 (length ws)))
               (should (string-match-p "buffer-newer" (car ws)))
               (should (equal "alpha BETA gamma\n"
                              (anvil-file-test--read path)))))
         (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; anvil-file-test.el ends here

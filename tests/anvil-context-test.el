;;; anvil-context-test.el --- Tests for anvil-context -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'anvil-context)
(require 'anvil-state)

(defmacro anvil-context-test--with-state (&rest body)
  "Run BODY with an isolated `anvil-state' DB."
  (declare (indent 0))
  `(let ((anvil-state-db-path (make-temp-file "anvil-context-" nil ".db"))
         (anvil-state--db nil))
     (unwind-protect
         (progn (anvil-state-enable) ,@body)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(ert-deftest anvil-context-test-detect-json ()
  "JSON payloads route to the json compressor."
  (should (eq 'json (anvil-context-detect-kind "[{\"a\":1}]" nil))))

(ert-deftest anvil-context-test-compress-json-array ()
  "Array-of-objects JSON compresses to keys + sample shapes."
  (let* ((raw "[{\"file\":\"a.el\",\"line\":10,\"text\":\"foo\"},{\"file\":\"b.el\",\"line\":20,\"text\":\"bar\"}]")
         (res (anvil-context-compress raw :kind 'json :max-chars 1000)))
    (should (eq 'json (plist-get res :kind)))
    (should (string-match-p "json array: 2 objects" (plist-get res :compressed)))
    (should (string-match-p "keys: file, line, text" (plist-get res :compressed)))
    (should (< (plist-get res :compressed-size)
               (* 2 (plist-get res :raw-size))))))

(ert-deftest anvil-context-test-compress-json-null ()
  "JSON null is not treated as parse failure."
  (let* ((res (anvil-context-compress "null" :kind 'json :max-chars 1000))
         (out (plist-get res :compressed)))
    (should (string-match-p "json value: :null" out))))

(ert-deftest anvil-context-test-compress-diff-summary ()
  "Unified diffs compress to file/hunk/stat summaries."
  (let* ((raw (concat "diff --git a/a.el b/a.el\n"
                      "--- a/a.el\n+++ b/a.el\n"
                      "@@ -1,2 +1,2 @@\n"
                      "-old\n+new\n"))
         (res (anvil-context-compress raw :kind 'diff :max-chars 1000))
         (out (plist-get res :compressed)))
    (should (string-match-p "diff summary: 1 files, 1 hunks, \\+1 -1" out))
    (should (string-match-p "diff --git a/a.el b/a.el" out))))

(ert-deftest anvil-context-test-compress-log-keeps-critical ()
  "Log compression keeps critical lines and tail context."
  (let* ((raw (mapconcat #'identity
                         (append (mapcar #'number-to-string
                                         (number-sequence 1 30))
                                 '("ERROR failed to connect")
                                 (mapcar #'number-to-string
                                         (number-sequence 32 60)))
                         "\n"))
         (res (anvil-context-compress raw :kind 'log :max-chars 2000))
         (out (plist-get res :compressed)))
    (should (string-match-p "ERROR failed" out))
    (should (string-match-p "log summary:" out))))

(ert-deftest anvil-context-test-compress-code-outline ()
  "Code compression keeps imports and definitions."
  (let* ((raw "import os\n\nclass A:\n    pass\n\ndef f():\n    return 1\n")
         (res (anvil-context-compress raw :kind 'code :max-chars 1000))
         (out (plist-get res :compressed)))
    (should (string-match-p "1:import os" out))
    (should (string-match-p "3:class A:" out))
    (should (string-match-p "6:def f" out))))

(ert-deftest anvil-context-test-ccr-roundtrip ()
  "Stored compression can retrieve the original by ccr-id."
  (anvil-context-test--with-state
    (let* ((raw "alpha\nbeta\nERROR gamma\n")
           (res (anvil-context-compress raw :kind 'log :store t))
           (id (plist-get res :ccr-id))
           (row (anvil-context-retrieve id)))
      (should id)
      (should (equal raw (plist-get row :raw)))
      (should (eq 'log (plist-get row :kind))))))

(ert-deftest anvil-context-test-stats ()
  "Compression telemetry aggregates raw and compressed byte totals."
  (anvil-context-test--with-state
    (anvil-context-compress "line one\nline two\n" :kind 'text)
    (let ((stats (anvil-context-stats 10)))
      (should (= 1 (plist-get stats :count)))
      (should (> (plist-get stats :raw-size) 0)))))

(provide 'anvil-context-test)

;;; anvil-context-test.el ends here

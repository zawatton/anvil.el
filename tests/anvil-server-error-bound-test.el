;;; anvil-server-error-bound-test.el --- ERT for bounded tool error text -*- lexical-binding: t; -*-

;;; Commentary:

;; Tool error text is fed back into an LLM client's context.  The
;; signal data of an unexpected error can carry an arbitrarily large
;; object — an unbound-slot error on an EIEIO object, a
;; wrong-type-argument on a populated hash table — and printing it in
;; full once produced a multi-megabyte error that overflowed the
;; client's context window.  These tests pin the bound on every error
;; path: the handler macro, tool-throw text at the dispatcher, and the
;; dispatcher's internal-error fallback.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-server)
(require 'anvil-server-metrics)

(defun anvil-server-error-bound-test--big-table ()
  "Return a hash table whose printed form is far over the error cap."
  (let ((table (make-hash-table :test 'equal)))
    (dotimes (i 5000)
      (puthash (format "key-%d" i) (make-string 40 ?x) table))
    table))

(ert-deftest anvil-server-error-bound-test-truncate-text ()
  "Text over the cap is cut and ends with a length trailer."
  (should (equal (anvil-server-truncate-text "abc" 10) "abc"))
  (should (equal (anvil-server-truncate-text "abc" nil) "abc"))
  (should (eq (anvil-server-truncate-text nil 10) nil))
  (let ((out (anvil-server-truncate-text (make-string 100 ?a) 10 "narrow it")))
    (should (string-prefix-p (make-string 10 ?a) out))
    (should-not (string-prefix-p (make-string 11 ?a) out))
    (should (string-match-p "showing 10 of 100 chars; narrow it\\]\\'" out))))

(ert-deftest anvil-server-error-bound-test-macro-bounds-huge-signal-data ()
  "An error carrying a huge object yields a message within the cap."
  (let* ((anvil-server-tool-error-max-chars 4096)
         (table (anvil-server-error-bound-test--big-table))
         (err (should-error
               (anvil-server-with-error-handling
                (signal 'wrong-type-argument (list 'sequencep table)))
               :type 'anvil-server-tool-error))
         (msg (cadr err)))
    (should (string-prefix-p "Error: (wrong-type-argument sequencep" msg))
    ;; Cap plus the fixed-size trailer.
    (should (< (length msg) (+ 4096 200)))))

(ert-deftest anvil-server-error-bound-test-macro-elides-while-printing ()
  "Bounded printing elides long lists instead of printing them whole."
  (let* ((anvil-server-tool-error-max-chars nil)
         (err (should-error
               (anvil-server-with-error-handling
                (signal 'error (list (number-sequence 1 10000))))
               :type 'anvil-server-tool-error))
         (msg (cadr err)))
    (should (string-match-p "\\.\\.\\." msg))
    (should (< (length msg) 1000))))

(ert-deftest anvil-server-error-bound-test-small-error-unchanged ()
  "Ordinary errors keep their historical \"Error: %S\" text."
  (let ((err (should-error
              (anvil-server-with-error-handling
               (error "Plain failure"))
              :type 'anvil-server-tool-error)))
    (should (equal (cadr err) "Error: (error \"Plain failure\")"))))

(defun anvil-server-error-bound-test--throw-tool ()
  "Test tool that throws an oversized tool error."
  (anvil-server-tool-throw (make-string 20000 ?e)))

(defun anvil-server-error-bound-test--raw-tool ()
  "Test tool that signals an unwrapped error with huge data."
  (signal 'wrong-type-argument
          (list 'sequencep (anvil-server-error-bound-test--big-table))))

(defun anvil-server-error-bound-test--call (tool-id)
  "Dispatch TOOL-ID and return the parsed JSON-RPC response."
  (json-read-from-string
   (anvil-server--handle-tools-call
    7 `((name . ,tool-id) (arguments))
    (make-anvil-server-metrics)
    "error-bound-test")))

(ert-deftest anvil-server-error-bound-test-dispatcher-bounds-tool-throw ()
  "Tool-throw text is capped before it becomes the isError content."
  (let ((anvil-server-tool-error-max-chars 4096))
    (anvil-server-register-tool
     #'anvil-server-error-bound-test--throw-tool
     :id "error-bound-throw"
     :description "Test tool"
     :server-id "error-bound-test")
    (unwind-protect
        (let* ((parsed (anvil-server-error-bound-test--call "error-bound-throw"))
               (result (alist-get 'result parsed))
               (text (alist-get 'text (aref (alist-get 'content result) 0))))
          (should (eq (alist-get 'isError result) t))
          (should (string-match-p "showing 4096 of 20000 chars" text))
          (should (< (length text) (+ 4096 200))))
      (anvil-server-unregister-tool "error-bound-throw" "error-bound-test"))))

(ert-deftest anvil-server-error-bound-test-dispatcher-bounds-internal-error ()
  "The internal-error fallback message is capped as well."
  (let ((anvil-server-tool-error-max-chars 4096))
    (anvil-server-register-tool
     #'anvil-server-error-bound-test--raw-tool
     :id "error-bound-raw"
     :description "Test tool"
     :server-id "error-bound-test")
    (unwind-protect
        (let* ((parsed (anvil-server-error-bound-test--call "error-bound-raw"))
               (msg (alist-get 'message (alist-get 'error parsed))))
          (should (string-prefix-p "Internal error executing tool" msg))
          (should (< (length msg) (+ 4096 200))))
      (anvil-server-unregister-tool "error-bound-raw" "error-bound-test"))))

(provide 'anvil-server-error-bound-test)
;;; anvil-server-error-bound-test.el ends here

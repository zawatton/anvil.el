;;; anvil-standalone-fast.el --- Bulk standalone transport operations -*- lexical-binding: t; -*-

;; Match the existing JSON escaper, but let the regexp engine find runs that
;; actually need escaping. Ordinary Unicode text is copied in whole slices.
(defun anvil-standalone--json-escape (match)
  "Escape one JSON control character MATCH."
  (let ((char (aref match 0)))
    (cond ((= char ?\") "\\\"")
          ((= char ?\\) "\\\\")
          ((= char ?\b) "\\b")
          ((= char ?\f) "\\f")
          ((= char ?\n) "\\n")
          ((= char ?\r) "\\r")
          ((= char ?\t) "\\t")
          (t (format "\\u%04x" char)))))

(defun anvil-standalone--json-string (string)
  "Encode STRING as JSON without walking every ordinary character in Lisp."
  (concat "\"" (replace-regexp-in-string
                  "[\"\\\x00-\x1f]" #'anvil-standalone--json-escape
                  string t t) "\""))

;; Keep the regexp implementation opt-in.  It is byte-identical to both JSON
;; providers, but remains slower for search responses on the current runtime.
;; Object/array/null/false/number semantics always use the provider unchanged.
(when (fboundp 'nelisp-json-encode-string)
  (unless (fboundp 'anvil-standalone--original-nelisp-json-string)
    (defalias 'anvil-standalone--original-nelisp-json-string
      (symbol-function 'nelisp-json-encode-string))))
(when (fboundp 'emacs-json--encode-string)
  (unless (fboundp 'anvil-standalone--original-emacs-json-string)
    (defalias 'anvil-standalone--original-emacs-json-string
      (symbol-function 'emacs-json--encode-string))))
(when (equal (getenv "ANVIL_STANDALONE_FAST_JSON") "1")
  (when (fboundp 'nelisp-json-encode-string)
    (defalias 'nelisp-json-encode-string #'anvil-standalone--json-string))
  (when (fboundp 'emacs-json--encode-string)
    (defalias 'emacs-json--encode-string #'anvil-standalone--json-string)))

;; The public bulk-read primitive copies SQLite's short-lived column before
;; stepping/finalizing. It avoids an interpreted ptr-read/aset loop per byte.
(when (and (fboundp 'ptr-read-bytes)
           (not (equal (getenv "ANVIL_STANDALONE_FAST_SQLITE") "0")))
  (defun emacs-sqlite-ffi--read (pointer size &optional textp)
    "Read SIZE bytes at POINTER, decoding UTF-8 for TEXTP."
    (let ((bytes (ptr-read-bytes pointer size)))
      (if (and textp (fboundp 'decode-coding-string))
          (decode-coding-string bytes 'utf-8 t)
        bytes))))

(defvar anvil-standalone--encoding-cache nil)
(defvar anvil-standalone--response-cache nil)
(defvar anvil-standalone--cache-response nil)
(setq anvil-standalone--encoding-cache nil
      anvil-standalone--response-cache nil)

(defun anvil-standalone--cache-get (table key)
  "Find an equal KEY in the bounded alist named TABLE.
The current runtime's equal hash tables miss structurally equal cons keys."
  (let ((tail (symbol-value table)) found)
    (while (and tail (not found))
      (when (equal key (caar tail)) (setq found (cdar tail)))
      (setq tail (cdr tail)))
    found))

(defun anvil-standalone--cache-put (table key value)
  "Keep at most 64 encoded VALUEs, each at most 128 KiB, in TABLE."
  (when (<= (string-bytes value) 131072)
    (when (>= (length (symbol-value table)) 64) (set table nil))
    (set table (cons (cons key value) (symbol-value table))))
  value)

(defun anvil-standalone--provider-json-string (string)
  "Encode STRING with the unchanged JSON provider."
  (cond ((fboundp 'anvil-standalone--original-emacs-json-string)
         (anvil-standalone--original-emacs-json-string string))
        ((fboundp 'anvil-standalone--original-nelisp-json-string)
         (anvil-standalone--original-nelisp-json-string string))
        (t (json-encode string))))

(defun anvil-standalone--text-result-json (result)
  "Return exact JSON for the fixed successful text RESULT, or nil."
  (let* ((content (alist-get 'content result))
         (item (and (vectorp content) (= (length content) 1)
                    (aref content 0)))
         (text (and (listp item) (alist-get 'text item))))
    (when (and (eq json-false :json-false)
               (symbolp json-null) (not (eq json-null :json-false))
               (= (length result) 2)
               (listp item) (= (length item) 2)
               (equal (alist-get 'type item) "text")
               (stringp text)
               (eq (alist-get 'isError result) :json-false))
      (concat "{\"content\":[{\"type\":\"text\",\"text\":"
              (anvil-standalone--provider-json-string text)
              "}],\"isError\":false}"))))

(unless (equal (getenv "ANVIL_STANDALONE_CACHE") "0")
  ;; Cache serialization of the actual value, never a database query. Every
  ;; handler still runs, so commits, rollbacks and configuration changes are
  ;; observed before lookup. Both JSON encoders use these two dynamic sentinels.
  (unless (fboundp 'anvil-standalone--original-encode-for-mcp)
    (defalias 'anvil-standalone--original-encode-for-mcp
      (symbol-function 'anvil-server-encode-for-mcp)))
  (defun anvil-server-encode-for-mcp (value)
    "Reuse JSON only for an equal actual VALUE in the same encoding context."
    (if (or (not anvil-standalone--cache-response) (null value) (stringp value)
            (not (symbolp json-null)) (not (symbolp json-false)))
        (anvil-standalone--original-encode-for-mcp value)
      (let ((key (list value json-null json-false)))
        (or (anvil-standalone--cache-get 'anvil-standalone--encoding-cache key)
            (anvil-standalone--cache-put
             'anvil-standalone--encoding-cache key
             (anvil-standalone--original-encode-for-mcp value))))))
  (unless (fboundp 'anvil-standalone--original-tools-call)
    (defalias 'anvil-standalone--original-tools-call
      (symbol-function 'anvil-server--handle-tools-call)))
  (defun anvil-server--handle-tools-call (id params metrics server-id)
    "Run ordinary validation, disclosure, metrics and hooks on every call."
    (let ((anvil-standalone--cache-response
           (member (alist-get 'name params) '("memory-search" "worklog-search"))))
      (anvil-standalone--original-tools-call id params metrics server-id)))
  (unless (fboundp 'anvil-standalone--original-jsonrpc-response)
    (defalias 'anvil-standalone--original-jsonrpc-response
      (symbol-function 'anvil-server--jsonrpc-response)))
  (defun anvil-server--jsonrpc-response (id result)
    "Reuse only the encoded RESULT fragment; encode the current ID each time."
    (if (and anvil-standalone--cache-response
             (symbolp json-null) (symbolp json-false)
             (eq (alist-get 'isError result) :json-false))
        (anvil-server--jsonrpc-response-from-result-json
         id (or (anvil-standalone--cache-get
                 'anvil-standalone--response-cache (list result json-null json-false))
                (anvil-standalone--cache-put
                 'anvil-standalone--response-cache (list result json-null json-false)
                 (or (anvil-standalone--text-result-json result)
                     (json-encode result)))))
      (anvil-standalone--original-jsonrpc-response id result))))

(provide 'anvil-standalone-fast)

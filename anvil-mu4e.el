;;; anvil-mu4e.el --- Structured MCP tools for mu4e e-mail -*- lexical-binding: t; -*-

;; Author: zawatton
;; Keywords: tools, mcp, mail, mu4e

;;; Commentary:

;; Opt-in anvil module (add `mu4e' to `anvil-optional-modules') that
;; exposes e-mail operations as structured MCP tools, so an AI agent can
;; search and read mail without guessing raw mu4e Elisp and text-scraping
;; buffers.  Design: docs/design/53-mu4e-tools.org (Doc 53), filed in
;; response to issue #50.
;;
;; Core decision: read/search are backed by the synchronous `mu' CLI
;; (`mu find --format=sexp', `mu view'), NOT mu4e's asynchronous
;; server/buffers.  This returns data Emacs can `read' directly, needs no
;; mu4e UI session, and sidesteps anvil's single-thread /
;; `accept-process-output'-does-not-yield constraint.  mu4e proper is only
;; needed for Phase 2 (compose/send), which is not part of this module yet.
;;
;; Phase 1 tools (all read-only, side-effect-free):
;;   mu4e-search      QUERY [max_results] [sort]  -> JSON message summaries
;;   mu4e-list-mails  [query] [max_results]       -> inbox-defaulted search
;;   mu4e-read-mail   MESSAGE_ID [html]           -> JSON headers + body
;;
;; Requires the external `mu' binary and an existing `mu index'.  When mu
;; is absent the tools error loudly, so enabling the module on a machine
;; without mu is harmless.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-server)

(defconst anvil-mu4e--server-id "emacs-eval"
  "MCP server id the mu4e tools register under (the main eval server).")

(defgroup anvil-mu4e nil
  "Structured MCP tools for mu4e e-mail, backed by the mu CLI."
  :group 'anvil
  :prefix "anvil-mu4e-")

(defcustom anvil-mu4e-mu-bin (or (executable-find "mu") "mu")
  "Path to the `mu' binary used for search and read."
  :type 'string
  :group 'anvil-mu4e)

(defcustom anvil-mu4e-muhome nil
  "Optional explicit mu home directory (the xapian store location).
When nil, mu uses its own default.  Mainly useful for tests that
index a throwaway maildir; production normally leaves this nil."
  :type '(choice (const :tag "mu default" nil) directory)
  :group 'anvil-mu4e)

(defcustom anvil-mu4e-max-results 50
  "Default cap on the number of messages a search returns."
  :type 'integer
  :group 'anvil-mu4e)

(defcustom anvil-mu4e-inbox-query "maildir:/INBOX"
  "Default mu query used by `mu4e-list-mails' when no query is given."
  :type 'string
  :group 'anvil-mu4e)

;;;; --- mu CLI invocation ---------------------------------------------------

(defun anvil-mu4e--program ()
  "Return the mu program, or signal an actionable error if absent."
  (let ((bin (or anvil-mu4e-mu-bin "mu")))
    (unless (or (and (file-name-absolute-p bin) (file-executable-p bin))
                (executable-find bin))
      (error "anvil-mu4e: mu binary not found (looked for %S); install mu \
and/or set `anvil-mu4e-mu-bin'" bin))
    bin))

(defun anvil-mu4e--run (args)
  "Run mu with ARGS (a list of strings).
Return a list (EXIT-CODE STDOUT STDERR)."
  (let ((bin (anvil-mu4e--program))
        (stderr (make-temp-file "anvil-mu4e-err-")))
    (unwind-protect
        (with-temp-buffer
          (let ((exit (apply #'call-process bin nil
                             (list (current-buffer) stderr) nil args)))
            (list exit
                  (buffer-string)
                  (with-temp-buffer
                    (ignore-errors (insert-file-contents stderr))
                    (buffer-string)))))
      (ignore-errors (delete-file stderr)))))

(defun anvil-mu4e--muhome-args ()
  "Return the `--muhome=' argument list when `anvil-mu4e-muhome' is set."
  (when (and anvil-mu4e-muhome (not (string-empty-p anvil-mu4e-muhome)))
    (list (concat "--muhome=" (expand-file-name anvil-mu4e-muhome)))))

(defun anvil-mu4e--sort-args (sort)
  "Translate a SORT keyword into `mu find' sort flags.
Default (nil / \"date\") is newest-first."
  (pcase (and (stringp sort) (downcase (string-trim sort)))
    ("subject" (list "--sortfield=subject"))
    ("from"    (list "--sortfield=from"))
    ("to"      (list "--sortfield=to"))
    ("size"    (list "--sortfield=size" "--reverse"))
    (_         (list "--sortfield=date" "--reverse"))))

(defun anvil-mu4e--read-sexps (string)
  "Read every top-level sexp in STRING into a list, in order."
  (let ((pos 0) (len (length string)) acc done)
    (while (and (not done) (< pos len))
      (condition-case nil
          (let ((r (read-from-string string pos)))
            (if (> (cdr r) pos)
                (progn (push (car r) acc) (setq pos (cdr r)))
              (setq done t)))
        ((end-of-file invalid-read-syntax) (setq done t))))
    (nreverse acc)))

;;;; --- sexp -> JSON-ready alist --------------------------------------------

(defun anvil-mu4e--coerce-int (val default)
  "Coerce VAL (integer, numeric string, or number) to an integer, else DEFAULT."
  (cond
   ((integerp val) val)
   ((and (stringp val) (string-match-p "\\`[0-9]+\\'" (string-trim val)))
    (string-to-number (string-trim val)))
   ((numberp val) (truncate val))
   (t default)))

(defun anvil-mu4e--truthy (v)
  "Interpret V as a boolean, tolerating JSON false and string forms."
  (cond
   ((eq v t) t)
   ((null v) nil)
   ((eq v :json-false) nil)
   ((and (stringp v) (member (downcase (string-trim v)) '("true" "t" "1" "yes"))) t)
   (t (and v t))))

(defun anvil-mu4e--contact->alist (c)
  "Normalize one mu contact C into a ((name . S) (email . S)) alist.
Handles both the (:email E :name N) plist (mu 1.8+) and the legacy
\(NAME . EMAIL) cons."
  (cond
   ((and (listp c) (plist-member c :email))
    `((name . ,(or (plist-get c :name) ""))
      (email . ,(or (plist-get c :email) ""))))
   ((and (consp c) (not (listp (cdr c))))
    `((name . ,(or (car c) "")) (email . ,(or (cdr c) ""))))
   (t `((name . "") (email . ,(format "%s" c))))))

(defun anvil-mu4e--contacts (lst)
  "Return a JSON array (vector) of normalized contacts from LST."
  (vconcat (mapcar #'anvil-mu4e--contact->alist lst)))

(defun anvil-mu4e--iso-date (time)
  "Format a mu TIME value as an ISO-8601 string, or nil."
  (when time
    (ignore-errors (format-time-string "%Y-%m-%dT%H:%M:%S%z" time))))

(defun anvil-mu4e--flags (lst)
  "Return a JSON array (vector) of flag name strings from symbol LST."
  (vconcat (mapcar (lambda (f) (if (symbolp f) (symbol-name f) (format "%s" f)))
                   lst)))

(defun anvil-mu4e--summary (pl)
  "Build a JSON-ready summary alist from a mu message plist PL."
  `((message_id . ,(or (plist-get pl :message-id) ""))
    (date . ,(or (anvil-mu4e--iso-date (plist-get pl :date)) ""))
    (from . ,(anvil-mu4e--contacts (plist-get pl :from)))
    (to . ,(anvil-mu4e--contacts (plist-get pl :to)))
    (cc . ,(anvil-mu4e--contacts (plist-get pl :cc)))
    (subject . ,(or (plist-get pl :subject) ""))
    (flags . ,(anvil-mu4e--flags (plist-get pl :flags)))
    (maildir . ,(or (plist-get pl :maildir) ""))
    (size . ,(or (plist-get pl :size) 0))
    (path . ,(or (plist-get pl :path) ""))))

;;;; --- search --------------------------------------------------------------

(defun anvil-mu4e--find (query &optional maxnum sort)
  "Run `mu find QUERY' and return a list of message plists.
MAXNUM caps results (defaults to `anvil-mu4e-max-results').  SORT is a
keyword understood by `anvil-mu4e--sort-args'.  A no-match result is an
empty list, not an error."
  (let* ((args (append (list "find" "--format=sexp")
                       (anvil-mu4e--muhome-args)
                       (list (format "--maxnum=%d"
                                     (anvil-mu4e--coerce-int
                                      maxnum anvil-mu4e-max-results)))
                       (anvil-mu4e--sort-args sort)
                       (list (or query ""))))
         (res (anvil-mu4e--run args))
         (exit (nth 0 res)) (out (nth 1 res)) (err (nth 2 res)))
    (cond
     ((= exit 0) (anvil-mu4e--read-sexps out))
     ;; mu exits 2 ("no matches for search expression") on an empty result.
     ((= exit 2) nil)
     (t (error "anvil-mu4e: mu find failed (exit %d): %s"
               exit (string-trim (or err "")))))))

(defun anvil-mu4e--tool-search (query &optional max_results sort)
  "Search the mu index and return matching messages as JSON.

MCP Parameters:
  query - mu query string, e.g. \"from:alice subject:invoice date:1w..now\".
  max_results - Optional integer cap (default `anvil-mu4e-max-results').
  sort - Optional sort field: date (default, newest first), subject, from,
         to, or size."
  (let ((msgs (anvil-mu4e--find query max_results sort)))
    (json-encode
     `((query . ,(or query ""))
       (count . ,(length msgs))
       (messages . ,(vconcat (mapcar #'anvil-mu4e--summary msgs)))))))

(defun anvil-mu4e--tool-list-mails (&optional query max_results)
  "List recent mail, defaulting to the inbox, as JSON.

MCP Parameters:
  query - Optional mu query; defaults to `anvil-mu4e-inbox-query'.
  max_results - Optional integer cap (default `anvil-mu4e-max-results')."
  (let ((q (if (and query (stringp query)
                    (not (string-empty-p (string-trim query))))
               query
             anvil-mu4e-inbox-query)))
    (anvil-mu4e--tool-search q max_results nil)))

;;;; --- read ----------------------------------------------------------------

(defun anvil-mu4e--resolve (message-id)
  "Return the mu message plist for MESSAGE-ID (Message-ID or numeric docid)."
  (let* ((raw (string-trim (format "%s" message-id)))
         (clean (replace-regexp-in-string "\\`<\\|>\\'" "" raw))
         (query (if (string-match-p "\\`[0-9]+\\'" clean)
                    (format "docid:%s" clean)
                  (format "msgid:%s" clean))))
    (car (anvil-mu4e--find query 1 nil))))

(defun anvil-mu4e--strip-headers (text)
  "Drop the leading header block `mu view' prints before the body."
  (if (string-match "\n[ \t]*\n" text)
      (substring text (match-end 0))
    text))

(defun anvil-mu4e--body (path html)
  "Return the decoded body of the message file at PATH.
With HTML non-nil, return the HTML rendering; otherwise plain text."
  (let* ((args (append (list "view")
                       (when html (list "--format=html"))
                       (list path)))
         (res (anvil-mu4e--run args))
         (exit (nth 0 res)) (out (nth 1 res)))
    (if (= exit 0)
        (if html out (anvil-mu4e--strip-headers out))
      "")))

(defun anvil-mu4e--tool-read-mail (message_id &optional html)
  "Return a single message (headers + body) as JSON.

MCP Parameters:
  message_id - The RFC Message-ID (angle brackets optional) or a numeric
               mu docid.
  html - Optional boolean; when true also include the HTML body part.
         Defaults to false (plain text only)."
  (let ((pl (anvil-mu4e--resolve message_id)))
    (unless pl
      (error "anvil-mu4e: no message found for %S" message_id))
    (let* ((path (plist-get pl :path))
           (want-html (anvil-mu4e--truthy html)))
      (json-encode
       (append
        (anvil-mu4e--summary pl)
        `((in_reply_to . ,(or (plist-get pl :in-reply-to) ""))
          (references . ,(vconcat (plist-get pl :references)))
          (body_plain . ,(anvil-mu4e--body path nil)))
        (when want-html
          `((body_html . ,(anvil-mu4e--body path t)))))))))

;;;; --- module lifecycle ----------------------------------------------------

(defun anvil-mu4e-enable ()
  "Register the anvil-mu4e MCP tools."
  (anvil-server-register-tool
   #'anvil-mu4e--tool-search
   :id "mu4e-search"
   :intent '(mail read search)
   :layer 'workflow
   :read-only t
   :server-id anvil-mu4e--server-id
   :description
   "Search e-mail via mu's index and return structured results.

Parameters:
  query - mu query string, e.g.
          \"from:alice subject:invoice date:1w..now\".
  max_results - Optional integer cap (default 50).
  sort - Optional: date (default, newest first), subject, from, to, size.

Returns JSON object: query, count, and a messages array of
{message_id, date, from, to, cc, subject, flags, maildir, size, path}.")

  (anvil-server-register-tool
   #'anvil-mu4e--tool-list-mails
   :id "mu4e-list-mails"
   :intent '(mail read)
   :layer 'workflow
   :read-only t
   :server-id anvil-mu4e--server-id
   :description
   "List recent mail, defaulting to the inbox.

Parameters:
  query - Optional mu query; defaults to maildir:/INBOX.
  max_results - Optional integer cap (default 50).

Returns the same JSON shape as mu4e-search.")

  (anvil-server-register-tool
   #'anvil-mu4e--tool-read-mail
   :id "mu4e-read-mail"
   :intent '(mail read)
   :layer 'workflow
   :read-only t
   :server-id anvil-mu4e--server-id
   :description
   "Read one message in full, by Message-ID or docid.

Parameters:
  message_id - RFC Message-ID (angle brackets optional) or numeric docid.
  html - Optional boolean; when true also include the HTML body.

Returns JSON object: the message summary fields plus in_reply_to,
references (array), body_plain, and body_html when requested."))

(defun anvil-mu4e-disable ()
  "Unregister the anvil-mu4e MCP tools."
  (dolist (id '("mu4e-search" "mu4e-list-mails" "mu4e-read-mail"))
    (anvil-server-unregister-tool id anvil-mu4e--server-id)))

(provide 'anvil-mu4e)
;;; anvil-mu4e.el ends here

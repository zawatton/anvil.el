;;; anvil-wl-smtp.el --- Portable SMTP-over-tunnel sender for anvil-wl -*- lexical-binding: t; -*-

;; Part of the anvil-wl mail layer: a substrate-agnostic SMTP client that
;; sends one message over an openssl TLS tunnel, mirroring anvil-wl-imap.el.
;; Same portability constraints (so the SAME code can later run on pure
;; NeLisp): only `make-process' / `process-send-string' /
;; `accept-process-output' (via :filter); TLS delegated to a generic external
;; tunnel (openssl s_client -crlf), so the SMTP logic stays pure Elisp and the
;; transport upgrades bare LF to CRLF; bytes are read/written binary.
;;
;; Default transport: implicit-TLS SMTPS (port 465).  AUTH PLAIN over the
;; tunnel.  The message body is dot-stuffed and terminated with the SMTP
;; CRLF.CRLF sentinel.

;;; Code:

(require 'cl-lib)

(defvar anvil-wl-smtp-openssl-program "openssl"
  "Program used to open the TLS tunnel to the SMTP server.")

(defvar anvil-wl-smtp-default-timeout 30
  "Default seconds to wait for an SMTP reply.")

(cl-defstruct (anvil-wl-smtp-conn (:constructor anvil-wl-smtp--make-conn))
  process              ; the tunnel process object
  (acc "")             ; accumulated raw bytes received (unibyte)
  (terminator "\n")    ; bytes appended after each command line
  host port)

(defun anvil-wl-smtp--binary-coding ()
  "Return a binary coding system symbol if the runtime has coding systems."
  (and (fboundp 'coding-system-p) (coding-system-p 'binary) 'binary))

(defun anvil-wl-smtp--filter (conn chunk)
  "Append CHUNK (a string) to CONN's accumulator."
  (when (stringp chunk)
    (setf (anvil-wl-smtp-conn-acc conn)
          (concat (anvil-wl-smtp-conn-acc conn) chunk))))

(defun anvil-wl-smtp--pump (conn predicate timeout)
  "Pump CONN's process output until PREDICATE returns non-nil or TIMEOUT.
Returns the value of PREDICATE on success, nil on timeout/death."
  (let* ((proc (anvil-wl-smtp-conn-process conn))
         (step 0.2)
         (elapsed 0.0)
         (result nil))
    (while (and (null (setq result (funcall predicate)))
                (< elapsed timeout)
                (process-live-p proc))
      (accept-process-output proc step)
      (setq elapsed (+ elapsed step)))
    (or result (funcall predicate))))

(defun anvil-wl-smtp--reply-complete (text)
  "If TEXT holds a complete SMTP reply, return its 3-digit code (string), else nil.
SMTP multi-line replies repeat the code with a hyphen (\"250-...\") on every
line but the last, which uses a space (\"250 ...\").  A reply is complete at
the first line matching `^<code> ' (space, not hyphen)."
  (let ((i 0) (len (length text)) (code nil) (stop nil))
    (while (and (not stop) (< i len))
      (let ((nl (string-match "\n" text i)))
        (if (not nl)
            (setq stop t)               ; incomplete trailing line
          (let* ((raw (substring text i nl))
                 (line (if (and (> (length raw) 0)
                                (eq (aref raw (1- (length raw))) ?\r))
                           (substring raw 0 -1)
                         raw)))
            (if (string-match "\\`\\([0-9][0-9][0-9]\\) " line)
                (setq code (match-string 1 line) stop t)
              (setq i (+ nl 1)))))))
    code))

(defun anvil-wl-smtp--dot-stuff (message)
  "Return MESSAGE dot-stuffed for the SMTP DATA phase.
Normalizes CRLF to bare LF (the tunnel's -crlf re-adds CRLF) and doubles any
leading `.' so a body line never looks like the CRLF.CRLF terminator."
  (let ((m (replace-regexp-in-string "\r\n" "\n" (or message ""))))
    (replace-regexp-in-string "^\\." ".." m)))

(defun anvil-wl-smtp--b64 (s)
  "Return UTF-8 S base64-encoded with no line breaks."
  (base64-encode-string (encode-coding-string s 'utf-8) t))

(defun anvil-wl-smtp-open-tunnel (host port)
  "Open an openssl TLS tunnel to HOST:PORT and wait for the SMTP greeting.
Return an `anvil-wl-smtp-conn', or signal an error on failure."
  (let* ((conn (anvil-wl-smtp--make-conn :host host :port port :terminator "\n"))
         (coding (anvil-wl-smtp--binary-coding))
         (proc (make-process
                :name (format "anvil-wl-smtp-%s" host)
                :command (list anvil-wl-smtp-openssl-program
                               "s_client" "-quiet" "-crlf" "-ign_eof"
                               "-connect" (format "%s:%s" host port))
                :noquery t
                :connection-type 'pipe
                :coding coding
                :filter (lambda (_p s) (anvil-wl-smtp--filter conn s)))))
    (setf (anvil-wl-smtp-conn-process conn) proc)
    (unless (anvil-wl-smtp--pump
             conn
             (lambda ()
               (let ((c (anvil-wl-smtp--reply-complete (anvil-wl-smtp-conn-acc conn))))
                 (and c (string-prefix-p "2" c))))
             anvil-wl-smtp-default-timeout)
      (ignore-errors (delete-process proc))
      (error "anvil-wl-smtp: no 220 greeting from %s:%s" host port))
    conn))

(defun anvil-wl-smtp--command (conn line &optional timeout)
  "Send LINE (no terminator) to CONN, wait for the reply, return a plist.
Plist keys: :code (3-digit string or nil) :text (slice since send)."
  (let* ((proc (anvil-wl-smtp-conn-process conn))
         (start (length (anvil-wl-smtp-conn-acc conn))))
    (unless (process-live-p proc)
      (error "anvil-wl-smtp: tunnel is dead"))
    (process-send-string proc (concat line (anvil-wl-smtp-conn-terminator conn)))
    (let ((code (anvil-wl-smtp--pump
                 conn
                 (lambda ()
                   (anvil-wl-smtp--reply-complete
                    (substring (anvil-wl-smtp-conn-acc conn) start)))
                 (or timeout anvil-wl-smtp-default-timeout))))
      (list :code code
            :text (substring (anvil-wl-smtp-conn-acc conn) start)))))

(defun anvil-wl-smtp--expect (conn line ok-prefix &optional timeout)
  "Send LINE to CONN; require the reply code to start with OK-PREFIX, else signal.
Return the reply plist on success."
  (let* ((res (anvil-wl-smtp--command conn line timeout))
         (code (plist-get res :code)))
    (unless (and code (string-prefix-p ok-prefix code))
      (error "anvil-wl-smtp: %S got %s (expected %s*): %s"
             line (or code "no-reply") ok-prefix
             (string-trim (or (plist-get res :text) ""))))
    res))

(defun anvil-wl-smtp-login (conn user pass)
  "Authenticate CONN as USER with PASS via AUTH PLAIN.  Signal on failure."
  (anvil-wl-smtp--expect
   conn
   (format "AUTH PLAIN %s"
           (anvil-wl-smtp--b64 (concat (string 0) user (string 0) pass)))
   "2"))

(defun anvil-wl-smtp-send (host port user pass from recipients message)
  "Send MESSAGE (an RFC822 string) FROM to RECIPIENTS via SMTP over a tunnel.
Authenticates with USER/PASS when USER is non-empty.  Returns t on success;
signals on any non-2xx (non-3xx for DATA) reply."
  (when (or (null recipients) (null (delq nil recipients)))
    (error "anvil-wl-smtp: no recipients"))
  (let ((conn (anvil-wl-smtp-open-tunnel host port)))
    (unwind-protect
        (progn
          (anvil-wl-smtp--expect
           conn (format "EHLO %s"
                        (if (fboundp 'system-name) (or (system-name) "localhost")
                          "localhost"))
           "2")
          (when (and user (stringp user) (not (string-empty-p user)))
            (anvil-wl-smtp-login conn user pass))
          (anvil-wl-smtp--expect conn (format "MAIL FROM:<%s>" from) "2")
          (dolist (rcpt recipients)
            (anvil-wl-smtp--expect conn (format "RCPT TO:<%s>" rcpt) "2"))
          (anvil-wl-smtp--expect conn "DATA" "3") ; 354 intermediate
          (let* ((proc (anvil-wl-smtp-conn-process conn))
                 (start (length (anvil-wl-smtp-conn-acc conn)))
                 (body (anvil-wl-smtp--dot-stuff message)))
            (unless (string-suffix-p "\n" body) (setq body (concat body "\n")))
            (process-send-string proc body)
            (process-send-string proc ".\n") ; tunnel upgrades to CRLF.CRLF
            (let ((code (anvil-wl-smtp--pump
                         conn
                         (lambda ()
                           (anvil-wl-smtp--reply-complete
                            (substring (anvil-wl-smtp-conn-acc conn) start)))
                         60)))
              (unless (and code (string-prefix-p "2" code))
                (error "anvil-wl-smtp: message rejected after DATA (%s): %s"
                       (or code "no-reply")
                       (string-trim (substring (anvil-wl-smtp-conn-acc conn) start))))))
          (ignore-errors (anvil-wl-smtp--command conn "QUIT" 5))
          t)
      (ignore-errors (delete-process (anvil-wl-smtp-conn-process conn))))))

(provide 'anvil-wl-smtp)
;;; anvil-wl-smtp.el ends here

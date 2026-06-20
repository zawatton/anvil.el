;;; anvil-wl-imap.el --- Portable IMAP-over-tunnel client -*- lexical-binding: t; -*-

;; Part of the anvil-wl mail layer (Strategy B): a substrate-agnostic IMAP
;; client that runs the heavy mail-sync work in a process *separate* from the
;; interactive Emacs daemon, so syncing never freezes the editor.
;;
;; Design constraints (so the SAME code runs on host Emacs --batch today and
;; on a pure-NeLisp runtime once its subprocess I/O lands):
;;   - Only standard process primitives: make-process / process-send-string /
;;     set-process-filter (via :filter) / accept-process-output.  No
;;     make-network-process, no gnutls, no coding-system machinery required.
;;   - TLS is delegated to a generic external tunnel (openssl s_client), so the
;;     mail logic stays pure elisp.  IMAPS = plaintext IMAP over the tunnel.
;;   - The transport decides the line terminator: the openssl tunnel runs with
;;     `-crlf' so we emit bare LF and openssl upgrades it to CRLF; a future raw
;;     socket transport would set the terminator to CRLF directly.
;;   - Bytes are read in binary so IMAP {N} literal byte-counts are exact and
;;     message bodies reach the Maildir without UTF-8 mangling.

;;; Code:

(require 'cl-lib)

(defvar anvil-wl-imap-openssl-program "openssl"
  "Program used to open the TLS tunnel to the IMAP server.")

(defvar anvil-wl-imap-default-timeout 20
  "Default seconds to wait for a tagged IMAP response.")

(cl-defstruct (anvil-wl-imap-conn (:constructor anvil-wl-imap--make-conn))
  process       ; the tunnel process object
  (acc "")      ; accumulated raw bytes received (unibyte)
  (tag-n 0)     ; monotonically increasing tag counter
  (terminator "\n") ; bytes appended after each command line
  host port)

(defun anvil-wl-imap--binary-coding ()
  "Return a binary coding system symbol if the runtime has coding systems."
  (and (fboundp 'coding-system-p) (coding-system-p 'binary) 'binary))

(defun anvil-wl-imap--filter (conn chunk)
  "Append CHUNK (a string) to CONN's accumulator."
  (when (stringp chunk)
    (setf (anvil-wl-imap-conn-acc conn)
          (concat (anvil-wl-imap-conn-acc conn) chunk))))

(defun anvil-wl-imap--pump (conn predicate timeout)
  "Pump CONN's process output until PREDICATE returns non-nil or TIMEOUT.
PREDICATE is called with no args and sees the current accumulator via CONN.
Returns the value of PREDICATE on success, nil on timeout/death."
  (let* ((proc (anvil-wl-imap-conn-process conn))
         (step 0.2)
         (elapsed 0.0)
         (result nil))
    (while (and (null (setq result (funcall predicate)))
                (< elapsed timeout)
                (process-live-p proc))
      (accept-process-output proc step)
      (setq elapsed (+ elapsed step)))
    (or result (funcall predicate))))

(defun anvil-wl-imap--scan-status (text tag)
  "Return \"OK\"/\"NO\"/\"BAD\" if TEXT contains TAG's completion, else nil.
Honors IMAP {N} literals so tag-like bytes inside a literal never match."
  (let ((i 0) (len (length text)) (status nil) (stop nil))
    (while (and (not stop) (< i len))
      (let ((nl (string-match "\n" text i)))
        (if (not nl)
            (setq stop t)               ; incomplete trailing line
          (let* ((raw (substring text i nl))
                 (line (if (and (> (length raw) 0)
                                (eq (aref raw (1- (length raw))) ?\r))
                           (substring raw 0 -1)
                         raw))
                 (lit (save-match-data
                        (and (string-match "{\\([0-9]+\\)}\\'" line)
                             (string-to-number (match-string 1 line))))))
            (if lit
                (setq i (+ nl 1 lit))   ; skip the literal octets opaquely
              (save-match-data
                (when (string-match
                       (concat "\\`" (regexp-quote tag) " \\(OK\\|NO\\|BAD\\)")
                       line)
                  (setq status (match-string 1 line) stop t)))
              (setq i (+ nl 1)))))))
    status))

(defun anvil-wl-imap-open-tunnel (host port)
  "Open an openssl TLS tunnel to HOST:PORT and wait for the IMAP greeting.
Return an `anvil-wl-imap-conn', or signal an error on failure."
  (let* ((conn (anvil-wl-imap--make-conn :host host :port port :terminator "\n"))
         (coding (anvil-wl-imap--binary-coding))
         (proc (make-process
                :name (format "anvil-wl-imap-%s" host)
                :command (list anvil-wl-imap-openssl-program
                               "s_client" "-quiet" "-crlf" "-ign_eof"
                               "-connect" (format "%s:%s" host port))
                :noquery t
                :connection-type 'pipe
                :coding coding
                :filter (lambda (_p s) (anvil-wl-imap--filter conn s)))))
    (setf (anvil-wl-imap-conn-process conn) proc)
    (unless (anvil-wl-imap--pump
             conn
             (lambda () (string-match-p "^\\* OK" (anvil-wl-imap-conn-acc conn)))
             anvil-wl-imap-default-timeout)
      (ignore-errors (delete-process proc))
      (error "anvil-wl-imap: no IMAP greeting from %s:%s" host port))
    conn))

(defun anvil-wl-imap--next-tag (conn)
  "Return the next unique command tag for CONN."
  (setf (anvil-wl-imap-conn-tag-n conn) (1+ (anvil-wl-imap-conn-tag-n conn)))
  (format "a%d" (anvil-wl-imap-conn-tag-n conn)))

(cl-defun anvil-wl-imap-command (conn cmd &optional (timeout anvil-wl-imap-default-timeout))
  "Send CMD to CONN, wait for its tagged completion, return a plist.
Plist keys: :tag :status (\"OK\"/\"NO\"/\"BAD\"/nil) :text (full slice since send)."
  (let* ((proc (anvil-wl-imap-conn-process conn))
         (tag (anvil-wl-imap--next-tag conn))
         (start (length (anvil-wl-imap-conn-acc conn))))
    (unless (process-live-p proc)
      (error "anvil-wl-imap: tunnel is dead"))
    (process-send-string
     proc (concat tag " " cmd (anvil-wl-imap-conn-terminator conn)))
    (let ((status (anvil-wl-imap--pump
                   conn
                   (lambda ()
                     (anvil-wl-imap--scan-status
                      (substring (anvil-wl-imap-conn-acc conn) start) tag))
                   timeout)))
      (list :tag tag :status status
            :text (substring (anvil-wl-imap-conn-acc conn) start)))))

(defun anvil-wl-imap--quote (s)
  "Return S as an IMAP quoted string."
  (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" s) "\""))

(defun anvil-wl-imap-login (conn user pass)
  "LOGIN to CONN as USER with PASS.  Return t on success, else signal."
  (let ((res (anvil-wl-imap-command
              conn (format "LOGIN %s %s"
                           (anvil-wl-imap--quote user)
                           (anvil-wl-imap--quote pass)))))
    (if (equal (plist-get res :status) "OK")
        t
      (error "anvil-wl-imap: LOGIN failed (%s)" (plist-get res :status)))))

(defun anvil-wl-imap-select (conn mailbox)
  "SELECT MAILBOX on CONN.  Return the EXISTS count (integer) or signal."
  (let* ((res (anvil-wl-imap-command conn (format "SELECT %s"
                                                  (anvil-wl-imap--quote mailbox))))
         (text (plist-get res :text)))
    (unless (equal (plist-get res :status) "OK")
      (error "anvil-wl-imap: SELECT %s failed (%s)" mailbox (plist-get res :status)))
    (if (string-match "^\\* \\([0-9]+\\) EXISTS" text)
        (string-to-number (match-string 1 text))
      0)))

(defun anvil-wl-imap-uid-search (conn criteria)
  "Run UID SEARCH CRITERIA on CONN.  Return a list of UID strings."
  (let* ((res (anvil-wl-imap-command conn (format "UID SEARCH %s" criteria)))
         (text (plist-get res :text)))
    (unless (equal (plist-get res :status) "OK")
      (error "anvil-wl-imap: UID SEARCH failed (%s)" (plist-get res :status)))
    (when (string-match "^\\* SEARCH\\([ 0-9]*\\)" text)
      (split-string (match-string 1 text) " " t))))

(defun anvil-wl-imap-uid-fetch-message (conn uid)
  "Fetch the raw RFC822 bytes of message UID via BODY.PEEK[].
Return a unibyte string of exactly the literal byte count, or nil."
  (let* ((res (anvil-wl-imap-command
               conn (format "UID FETCH %s BODY.PEEK[]" uid) 60))
         (text (plist-get res :text)))
    (when (and (equal (plist-get res :status) "OK")
               (string-match "{\\([0-9]+\\)}\r?\n" text))
      (let ((size (string-to-number (match-string 1 text)))
            (beg (match-end 0)))
        (when (>= (length text) (+ beg size))
          (substring text beg (+ beg size)))))))

(defun anvil-wl-imap-logout (conn)
  "LOGOUT and tear down CONN's tunnel."
  (let ((proc (anvil-wl-imap-conn-process conn)))
    (when (process-live-p proc)
      (ignore-errors (anvil-wl-imap-command conn "LOGOUT" 5)))
    (ignore-errors (delete-process proc))))

(defun anvil-wl-imap-read-app-password (path)
  "Read an app password from PATH, stripping spaces and line endings."
  (with-temp-buffer
    (let ((coding-system-for-read (or (anvil-wl-imap--binary-coding) 'no-conversion)))
      (insert-file-contents-literally path))
    (replace-regexp-in-string "[ \r\n]" "" (buffer-string))))

(provide 'anvil-wl-imap)
;;; anvil-wl-imap.el ends here

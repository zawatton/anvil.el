;;; anvil-wl-wanderlust.el --- Wanderlust XOAUTH2 bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; Small integration layer for Wanderlust/ELMO.  Wanderlust already knows how
;; to run SASL XOAUTH2; this bridge supplies the OAuth2 access token through
;; ELMO's password lookup path for selected IMAP accounts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup anvil-wl-wanderlust nil
  "Wanderlust integration helpers for anvil-wl."
  :group 'anvil-wl
  :prefix "anvil-wl-wanderlust-")

(defcustom anvil-wl-wanderlust-xoauth2-token-files nil
  "Token file rules for Wanderlust XOAUTH2 IMAP accounts.
Each entry is a plist with keys:
  :server IMAP server name, for example \"outlook.office365.com\"
  :user account name, for example \"user@example.com\"
  :port IMAP port, usually 993
  :token-file file containing the current OAuth2 access token.

The token file must contain only the bearer access token.  Refresh-token
management is intentionally separate; this bridge only feeds the current access
token into ELMO's existing SASL XOAUTH2 implementation."
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'anvil-wl-wanderlust)

(defcustom anvil-wl-wanderlust-refresh-function nil
  "Function called before reading a configured XOAUTH2 token file.
When non-nil, the function is called with no arguments.  It can refresh the
access token file or signal an error."
  :type '(choice (const nil) function)
  :group 'anvil-wl-wanderlust)

(defun anvil-wl-wanderlust--read-token-file (path)
  "Read an OAuth2 access token from PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (string-trim (buffer-string))))

(defun anvil-wl-wanderlust--token-file-for-key (key)
  "Return a configured token file for ELMO password KEY, or nil.
KEY has the shape (PROTOCOL USER AUTH SERVER PORT)."
  (pcase-let ((`(,protocol ,user ,auth ,server ,port) key))
    (and (equal protocol "IMAP")
         (eq auth 'xoauth2)
         (cl-loop for rule in anvil-wl-wanderlust-xoauth2-token-files
                  when (and (equal (plist-get rule :user) user)
                            (equal (plist-get rule :server) server)
                            (= (or (plist-get rule :port) 993) port))
                  return (plist-get rule :token-file)))))

;;;###autoload
(defun anvil-wl-wanderlust-elmo-get-passwd-advice (orig key)
  "Return an XOAUTH2 token for configured Outlook KEY, else call ORIG."
  (if-let ((token-file (anvil-wl-wanderlust--token-file-for-key key)))
      (progn
        (when anvil-wl-wanderlust-refresh-function
          (funcall anvil-wl-wanderlust-refresh-function))
        (anvil-wl-wanderlust--read-token-file token-file))
    (funcall orig key)))

;;;###autoload
(defun anvil-wl-wanderlust-enable ()
  "Enable the Wanderlust XOAUTH2 token-file bridge."
  (interactive)
  (advice-add 'elmo-get-passwd :around
              #'anvil-wl-wanderlust-elmo-get-passwd-advice))

;;;###autoload
(defun anvil-wl-wanderlust-disable ()
  "Disable the Wanderlust XOAUTH2 token-file bridge."
  (interactive)
  (advice-remove 'elmo-get-passwd
                 #'anvil-wl-wanderlust-elmo-get-passwd-advice))

(provide 'anvil-wl-wanderlust)
;;; anvil-wl-wanderlust.el ends here

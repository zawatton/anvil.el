;;; anvil-wl-oauth.el --- OAuth2 token helper for anvil-wl -*- lexical-binding: t; -*-

;;; Commentary:

;; Minimal OAuth2 device-code helper for mail clients that need a bearer token
;; for SASL XOAUTH2.  The caller must provide a Microsoft Entra public-client
;; client id; this module does not embed third-party client ids.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-http)
(require 'url-util)

(defgroup anvil-wl-oauth nil
  "OAuth2 helpers for anvil-wl."
  :group 'anvil-wl
  :prefix "anvil-wl-oauth-")

(defcustom anvil-wl-oauth-microsoft-tenant "consumers"
  "Microsoft identity tenant used for Outlook.com OAuth.
Use \"consumers\" for personal Microsoft accounts, \"common\" for mixed
personal/work accounts, or a tenant id for a specific organization."
  :type 'string
  :group 'anvil-wl-oauth)

(defcustom anvil-wl-oauth-microsoft-scope
  "offline_access https://outlook.office.com/IMAP.AccessAsUser.All"
  "OAuth2 scope string for Outlook IMAP access."
  :type 'string
  :group 'anvil-wl-oauth)

(defcustom anvil-wl-oauth-client-id nil
  "Microsoft Entra public-client application id for device-code auth."
  :type '(choice (const nil) string)
  :group 'anvil-wl-oauth)

(defcustom anvil-wl-oauth-token-cache-file
  "D:/Mail-wl/.secrets/outlook-oauth-token.el"
  "File used to store the OAuth token response plist."
  :type 'file
  :group 'anvil-wl-oauth)

(defcustom anvil-wl-oauth-access-token-file
  "D:/Mail-wl/.secrets/outlook-access-token.txt"
  "File used to store only the current access token."
  :type 'file
  :group 'anvil-wl-oauth)

(defcustom anvil-wl-oauth-http-backend 'auto
  "HTTP backend used for OAuth token requests.
`url' uses Emacs `url-retrieve-synchronously'.  `powershell' uses
PowerShell's `Invoke-RestMethod'.  `auto' tries Emacs URL first and falls back
to PowerShell on Windows when the URL backend fails."
  :type '(choice (const auto) (const url) (const powershell))
  :group 'anvil-wl-oauth)

(defcustom anvil-wl-oauth-powershell-program
  (or (executable-find "powershell.exe")
      (executable-find "pwsh.exe")
      "powershell.exe")
  "PowerShell executable used by the OAuth HTTP fallback."
  :type 'file
  :group 'anvil-wl-oauth)

(defun anvil-wl-oauth--endpoint (tenant suffix)
  "Return Microsoft identity endpoint for TENANT and SUFFIX."
  (format "https://login.microsoftonline.com/%s/oauth2/v2.0/%s"
          tenant suffix))

(defun anvil-wl-oauth--json-read ()
  "Read JSON response from the current url buffer as an alist."
  (goto-char (point-min))
  (re-search-forward "\r?\n\r?\n" nil t)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false nil))
    (json-read)))

(defun anvil-wl-oauth--query-params (params)
  "Return PARAMS in the list shape expected by `url-build-query-string'."
  (mapcar (lambda (param)
            (if (consp param)
                (list (car param) (cdr param))
              param))
          params))

(defun anvil-wl-oauth--post-form-url (url params)
  "POST PARAMS to URL with Emacs URL and return JSON."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (url-build-query-string (anvil-wl-oauth--query-params params)))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf
      (error "anvil-wl-oauth: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (anvil-wl-oauth--json-read))
      (kill-buffer buf))))

(defun anvil-wl-oauth--post-form-powershell (url params)
  "POST PARAMS to URL with PowerShell and return JSON."
  (let* ((params-file (make-temp-file "anvil-wl-oauth-params-" nil ".json"))
         (script-file (make-temp-file "anvil-wl-oauth-post-" nil ".ps1"))
         (out-buf (generate-new-buffer " *anvil-wl-oauth-powershell*"))
         (err-buf (generate-new-buffer " *anvil-wl-oauth-powershell-error*"))
         status output)
    (unwind-protect
        (progn
          (with-temp-file params-file
            (insert (json-encode params)))
          (with-temp-file script-file
            (insert
             "$ErrorActionPreference = 'Stop'\n"
             "try {\n"
             "  $params = Get-Content -LiteralPath $args[1] -Raw | ConvertFrom-Json\n"
             "  $body = @{}\n"
             "  $params.PSObject.Properties | ForEach-Object { $body[$_.Name] = $_.Value }\n"
             "  $resp = Invoke-RestMethod -Method Post -Uri $args[0] -Body $body -ContentType 'application/x-www-form-urlencoded' -TimeoutSec 60\n"
             "  $resp | ConvertTo-Json -Compress -Depth 20\n"
             "} catch {\n"
             "  $msg = $_.ErrorDetails.Message\n"
             "  if (-not $msg) { $msg = $_.Exception.Message }\n"
             "  [Console]::Error.WriteLine($msg)\n"
             "  exit 1\n"
             "}\n"))
          (setq status (call-process anvil-wl-oauth-powershell-program
                                     nil (list out-buf err-buf) nil
                                     "-NoProfile" "-ExecutionPolicy" "Bypass"
                                     "-File" script-file url params-file))
          (unless (zerop status)
            (error "anvil-wl-oauth: PowerShell HTTP failed: %s"
                   (string-trim
                    (with-current-buffer err-buf (buffer-string)))))
          (setq output (string-trim
                        (with-current-buffer out-buf (buffer-string))))
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-false nil))
            (json-read-from-string output)))
      (ignore-errors (delete-file params-file))
      (ignore-errors (delete-file script-file))
      (when (buffer-live-p out-buf) (kill-buffer out-buf))
      (when (buffer-live-p err-buf) (kill-buffer err-buf)))))

(defun anvil-wl-oauth--post-form (url params)
  "POST PARAMS to URL as application/x-www-form-urlencoded and return JSON."
  (pcase anvil-wl-oauth-http-backend
    ('url (anvil-wl-oauth--post-form-url url params))
    ('powershell (anvil-wl-oauth--post-form-powershell url params))
    (_
     (condition-case err
         (anvil-wl-oauth--post-form-url url params)
       (error
        (if (memq system-type '(windows-nt ms-dos))
            (anvil-wl-oauth--post-form-powershell url params)
          (signal (car err) (cdr err))))))))

(defun anvil-wl-oauth--require-client-id (&optional client-id)
  "Return CLIENT-ID or configured client id, else signal."
  (or client-id
      anvil-wl-oauth-client-id
      (error "anvil-wl-oauth: `anvil-wl-oauth-client-id' is not set")))

(defun anvil-wl-oauth--now ()
  "Return current Unix time."
  (floor (float-time)))

(defun anvil-wl-oauth--token-plist (json &optional refresh-token)
  "Convert token response JSON alist to a persisted plist.
REFRESH-TOKEN is preserved when the response omits a new one."
  (let ((access (alist-get 'access_token json))
        (refresh (or (alist-get 'refresh_token json) refresh-token))
        (expires (alist-get 'expires_in json))
        (scope (alist-get 'scope json)))
    (unless access
      (error "anvil-wl-oauth: token response did not include access_token"))
    (list :access-token access
          :refresh-token refresh
          :scope scope
          :expires-at (+ (anvil-wl-oauth--now) (or expires 3600)))))

(defun anvil-wl-oauth--save-token (token)
  "Persist TOKEN plist to cache and access-token files."
  (make-directory (file-name-directory anvil-wl-oauth-token-cache-file) t)
  (with-temp-file anvil-wl-oauth-token-cache-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 token (current-buffer))))
  (when-let ((access (plist-get token :access-token)))
    (with-temp-file anvil-wl-oauth-access-token-file
      (insert access "\n")))
  token)

(defun anvil-wl-oauth-load-token ()
  "Load the cached OAuth token plist, or nil."
  (when (file-exists-p anvil-wl-oauth-token-cache-file)
    (with-temp-buffer
      (insert-file-contents anvil-wl-oauth-token-cache-file)
      (read (current-buffer)))))

(defun anvil-wl-oauth-token-expired-p (&optional token skew)
  "Return non-nil when TOKEN expires within SKEW seconds."
  (let ((token (or token (anvil-wl-oauth-load-token)))
        (skew (or skew 300)))
    (or (null token)
        (<= (or (plist-get token :expires-at) 0)
            (+ (anvil-wl-oauth--now) skew)))))

;;;###autoload
(defun anvil-wl-oauth-refresh (&optional client-id)
  "Refresh the cached Microsoft OAuth token.
CLIENT-ID overrides `anvil-wl-oauth-client-id'.  The refreshed access token is
written to `anvil-wl-oauth-access-token-file'."
  (interactive)
  (let* ((token (or (anvil-wl-oauth-load-token)
                    (error "anvil-wl-oauth: no cached token")))
         (refresh (or (plist-get token :refresh-token)
                      (error "anvil-wl-oauth: no refresh_token in cache")))
         (json (anvil-wl-oauth--post-form
                (anvil-wl-oauth--endpoint
                 anvil-wl-oauth-microsoft-tenant "token")
                `(("client_id" . ,(anvil-wl-oauth--require-client-id client-id))
                  ("scope" . ,anvil-wl-oauth-microsoft-scope)
                  ("grant_type" . "refresh_token")
                  ("refresh_token" . ,refresh)))))
    (anvil-wl-oauth--save-token
     (anvil-wl-oauth--token-plist json refresh))))

;;;###autoload
(defun anvil-wl-oauth-ensure-access-token (&optional client-id)
  "Return a usable access token, refreshing it if needed."
  (let ((token (anvil-wl-oauth-load-token)))
    (when (anvil-wl-oauth-token-expired-p token)
      (setq token (anvil-wl-oauth-refresh client-id)))
    (or (plist-get token :access-token)
        (error "anvil-wl-oauth: no access token available"))))

;;;###autoload
(defun anvil-wl-oauth-device-login (&optional client-id)
  "Start Microsoft OAuth device-code login and save the resulting token.
CLIENT-ID overrides `anvil-wl-oauth-client-id'.  The command prints the
verification URL and user code, then polls until the browser login completes."
  (interactive)
  (let* ((client-id (anvil-wl-oauth--require-client-id client-id))
         (tenant anvil-wl-oauth-microsoft-tenant)
         (device (anvil-wl-oauth--post-form
                  (anvil-wl-oauth--endpoint tenant "devicecode")
                  `(("client_id" . ,client-id)
                    ("scope" . ,anvil-wl-oauth-microsoft-scope))))
         (device-code (alist-get 'device_code device))
         (user-code (alist-get 'user_code device))
         (verification-uri (or (alist-get 'verification_uri device)
                               (alist-get 'verification_url device)))
         (message (alist-get 'message device))
         (interval (or (alist-get 'interval device) 5))
         (expires-at (+ (anvil-wl-oauth--now)
                        (or (alist-get 'expires_in device) 900)))
         done result)
    (unless device-code
      (error "anvil-wl-oauth: device code response was incomplete"))
    (message "%s"
             (or message
                 (format "Open %s and enter code %s"
                         verification-uri user-code)))
    (while (and (not done) (< (anvil-wl-oauth--now) expires-at))
      (sleep-for interval)
      (let ((json (anvil-wl-oauth--post-form
                   (anvil-wl-oauth--endpoint tenant "token")
                   `(("grant_type" . "urn:ietf:params:oauth:grant-type:device_code")
                     ("client_id" . ,client-id)
                     ("device_code" . ,device-code)))))
        (pcase (alist-get 'error json)
          ((or "authorization_pending" 'authorization_pending)
           nil)
          ((or "slow_down" 'slow_down)
           (setq interval (+ interval 5)))
          ((or "authorization_declined" 'authorization_declined)
           (error "anvil-wl-oauth: authorization declined"))
          ((or "expired_token" 'expired_token)
           (error "anvil-wl-oauth: device code expired"))
          (`nil
           (setq result (anvil-wl-oauth--save-token
                         (anvil-wl-oauth--token-plist json))
                 done t))
          (_
           (error "anvil-wl-oauth: token polling failed: %s"
                  (or (alist-get 'error_description json)
                      (alist-get 'error json)))))))
    (or result
        (error "anvil-wl-oauth: device login timed out"))))

(provide 'anvil-wl-oauth)
;;; anvil-wl-oauth.el ends here

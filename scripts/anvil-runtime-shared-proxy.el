;;; anvil-runtime-shared-proxy.el --- MCP stdio proxy to the shared anvil daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by the bootstrap `bin/anvil-runtime mcp shared' writes.  Instead
;; of loading anvil's module chain in every MCP session (~450 MB and a
;; 23-36 s cold start each on windows-x86_64), each session runs this small
;; proxy and forwards its JSON-RPC messages to one shared daemon
;; (`anvil-runtime mcp daemon', the shell loop's service branch), using
;; NeLisp's nelisp-service package (NeLisp Doc 213).
;;
;; The proxy answers what it can without the daemon, so a session that
;; starts the daemon does not wait out its cold start for the handshake:
;;   - `initialize' with the same result the shell loop's fast path gives;
;;   - `notifications/initialized' (nothing to answer);
;;   - `tools/list' from the fast-handshake cache while no daemon
;;     connection exists yet;
;;   - any request before `initialize' (e.g. `server/discover') with
;;     Method not found, so the client falls back to `initialize'.
;; Everything else goes to the daemon.
;;
;; Bootstrap variables (all set by bin/anvil-runtime):
;;   anvil-runtime-bootstrap-service-dir        nelisp-service src dir
;;   anvil-runtime-bootstrap-service-state-dir  lock/state directory
;;   anvil-runtime-bootstrap-service-version    daemon version string
;;   anvil-runtime-bootstrap-service-command    daemon start command list
;;   anvil-runtime-bootstrap-fast-tools-file    fast-handshake cache file
;;   anvil-runtime-bootstrap-tool-modules       modules the cache must match

;;; Code:

(add-to-list 'load-path anvil-runtime-bootstrap-service-dir)
(require 'nelisp-service-client)

(setq nelisp-service-state-directory anvil-runtime-bootstrap-service-state-dir)

;; Set by the fast-handshake cache file this proxy loads.  Declared
;; special here: a `let' of an undeclared name is lexical in this file and
;; would never see the values the cache file sets.
(defvar anvil-runtime-shell--fast-tools-modules nil)
(defvar anvil-runtime-shell--fast-tools-json nil)

(defvar anvil-runtime-proxy--initialized nil
  "Non-nil once this session's `initialize' has been answered.")

(defvar anvil-runtime-proxy--tools-json 'unread
  "Cached tools/list result JSON, nil when unusable, `unread' before loading.")

(defun anvil-runtime-proxy--skip-space (s i)
  "Return the first index at or after I in S that is not JSON space or `:'."
  (while (and (< i (length s)) (memq (aref s i) '(?\s ?\t ?\n ?\r ?:)))
    (setq i (1+ i)))
  i)

(defun anvil-runtime-proxy--value-after (s key)
  "Return the raw JSON token after the first \"KEY\" in S, or nil.
A string token is returned with its quotes."
  (let ((pos (string-search (concat "\"" key "\"") s)))
    (when pos
      (let* ((i (anvil-runtime-proxy--skip-space s (+ pos (length key) 2)))
             (j i))
        (cond
         ((>= i (length s)) nil)
         ((= (aref s i) ?\")
          (setq j (1+ i))
          (while (and (< j (length s)) (/= (aref s j) ?\"))
            (when (= (aref s j) ?\\) (setq j (1+ j)))
            (setq j (1+ j)))
          (substring s i (min (length s) (1+ j))))
         (t
          (while (and (< j (length s)) (not (memq (aref s j) '(?, ?} ?\s ?\n ?\r))))
            (setq j (1+ j)))
          (substring s i j)))))))

(defun anvil-runtime-proxy--method (json)
  "Return JSON's method name, or nil."
  (let ((token (anvil-runtime-proxy--value-after json "method")))
    (and token (> (length token) 1) (= (aref token 0) ?\")
         (substring token 1 -1))))

(defun anvil-runtime-proxy--cached-tools ()
  "Return the cached tools/list result JSON when it matches the modules."
  (when (eq anvil-runtime-proxy--tools-json 'unread)
    (setq anvil-runtime-proxy--tools-json nil)
    (let ((file anvil-runtime-bootstrap-fast-tools-file))
      (when (file-exists-p file)
        (condition-case nil
            (progn
              (setq anvil-runtime-shell--fast-tools-modules nil
                    anvil-runtime-shell--fast-tools-json nil)
              (load file nil t)
              (when (and (stringp anvil-runtime-shell--fast-tools-json)
                         (equal anvil-runtime-shell--fast-tools-modules
                                anvil-runtime-bootstrap-tool-modules))
                (setq anvil-runtime-proxy--tools-json
                      anvil-runtime-shell--fast-tools-json)))
          (error nil)))))
  anvil-runtime-proxy--tools-json)

(defun anvil-runtime-proxy--local (json connected)
  "Answer JSON locally when possible; see the Commentary.
CONNECTED is non-nil once a daemon connection exists."
  (let ((method (anvil-runtime-proxy--method json))
        (id (or (anvil-runtime-proxy--value-after json "id") "null")))
    (cond
     ((equal method "initialize")
      (setq anvil-runtime-proxy--initialized t)
      (concat "{\"jsonrpc\":\"2.0\",\"id\":" id
              ",\"result\":{\"protocolVersion\":\"2025-03-26\","
              "\"serverInfo\":{\"name\":\"anvil\",\"version\":\"2025-03-26\"},"
              "\"capabilities\":{\"tools\":{}}}}"))
     ((equal method "notifications/initialized") "")
     ((and (not anvil-runtime-proxy--initialized) method (not (equal id "null")))
      (concat "{\"jsonrpc\":\"2.0\",\"id\":" id
              ",\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}"))
     ((and (equal method "tools/list") (not connected)
           (anvil-runtime-proxy--cached-tools))
      (concat "{\"jsonrpc\":\"2.0\",\"id\":" id
              ",\"result\":" (anvil-runtime-proxy--cached-tools) "}"))
     (t nil))))

(nelisp-service-client-stdio-proxy
 "anvil"
 :version anvil-runtime-bootstrap-service-version
 :start-command anvil-runtime-bootstrap-service-command
 ;; A first-ever daemon start generates the schema cache (~9 minutes on
 ;; the reader); a normal cold start is 23-36 s.
 :timeout 900
 :stale-after 900
 :local-handler #'anvil-runtime-proxy--local)

;;; anvil-runtime-shared-proxy.el ends here

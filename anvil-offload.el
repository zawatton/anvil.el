;;; anvil-offload.el --- Offload heavy elisp to a batch subprocess -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 of Doc 03 (Offload Framework).  Provides a long-lived
;; `emacs --batch' REPL subprocess and a future-based API to hand
;; heavy elisp off the main daemon.
;;
;; Phase 1 scope: standalone REPL (no worker-pool integration).
;; Phase 2 will route through the batch lane of `anvil-worker'.
;;
;; Public API:
;;   (anvil-offload FORM &rest KEYS)
;;       Returns an `anvil-future'.  FORM evaluates in the REPL.
;;       KEYS reserved for future use (:timeout, :require, :env).
;;   (anvil-future-done-p FUTURE)
;;   (anvil-future-await FUTURE &optional TIMEOUT)
;;   (anvil-future-value FUTURE)
;;   (anvil-future-error FUTURE)
;;   (anvil-future-cancel FUTURE)
;;
;; Protocol (one S-exp per line, utf-8-unix):
;;   request : (:id N :form EXPR)
;;   reply   : (:id N :ok VALUE) | (:id N :error MSG)
;;
;; The REPL uses `send-string-to-terminal' for replies because it
;; calls fflush(stdout) in batch mode — a plain `princ' may stay in
;; the C stdio buffer on Windows pipes and never reach the client.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Customization

(defgroup anvil-offload nil
  "Offload heavy elisp to a batch subprocess."
  :group 'anvil
  :prefix "anvil-offload-")

(defcustom anvil-offload-emacs-bin (or (executable-find "emacs") "emacs")
  "Emacs binary used to spawn the offload REPL."
  :type 'file
  :group 'anvil-offload)

(defcustom anvil-offload-default-await-timeout 300
  "Default timeout (seconds) for `anvil-future-await' when omitted."
  :type 'integer
  :group 'anvil-offload)

(defcustom anvil-offload-poll-interval 0.05
  "Seconds to block in `accept-process-output' per await iteration."
  :type 'number
  :group 'anvil-offload)

;;; State

(defvar anvil-offload--repl-process nil
  "Current long-lived REPL subprocess, or nil.")

(defvar anvil-offload--next-id 0
  "Monotonic request ID counter.")

(defvar anvil-offload--pending nil
  "Hash table mapping request-id → `anvil-future'.
Created lazily in `anvil-offload--ensure-pending'.")

(defvar anvil-offload--repl-init-file nil
  "Path to the generated REPL init file, or nil if not yet written.")

(defun anvil-offload--ensure-pending ()
  "Return the pending-futures hash, creating it if needed."
  (or anvil-offload--pending
      (setq anvil-offload--pending (make-hash-table :test 'eql))))

;;; REPL init file

(defconst anvil-offload--repl-body
  ";; anvil-offload REPL — auto-generated, do not edit.
\(setq coding-system-for-read 'utf-8-unix
      coding-system-for-write 'utf-8-unix)
\(condition-case nil
    (while t
      (let* ((msg (read t))
             (id (and (listp msg) (plist-get msg :id)))
             (form (and (listp msg) (plist-get msg :form))))
        (when id
          (let ((reply
                 (condition-case err
                     (list :id id :ok (eval form t))
                   (error (list :id id :error (format \"%S\" err))))))
            (send-string-to-terminal (prin1-to-string reply))
            (send-string-to-terminal \"\\n\")))))
  (end-of-file (kill-emacs 0)))
"
  "Body of the REPL loop written into the subprocess init file.")

(defun anvil-offload--repl-init-file ()
  "Return the path to the REPL init file, writing it if necessary."
  (unless (and anvil-offload--repl-init-file
               (file-exists-p anvil-offload--repl-init-file))
    (let ((file (make-temp-file "anvil-offload-repl-" nil ".el")))
      (with-temp-file file
        (let ((coding-system-for-write 'utf-8-unix))
          (insert anvil-offload--repl-body)))
      (setq anvil-offload--repl-init-file file)))
  anvil-offload--repl-init-file)

;;; Future struct

(cl-defstruct (anvil-future (:conc-name anvil-future--))
  id
  process
  status                                ; 'pending 'done 'error 'cancelled
  result
  err
  (created-at (float-time))
  done-at)

(defun anvil-future-done-p (future)
  "Non-nil when FUTURE has settled (done/error/cancelled)."
  (not (eq (anvil-future--status future) 'pending)))

(defun anvil-future-value (future)
  "Return the value of FUTURE; signal if errored, cancelled, or pending."
  (pcase (anvil-future--status future)
    ('done      (anvil-future--result future))
    ('error     (error "anvil-offload: remote error: %s"
                       (anvil-future--err future)))
    ('cancelled (error "anvil-offload: future was cancelled"))
    ('pending   (error "anvil-offload: future still pending"))
    (other      (error "anvil-offload: unknown status %S" other))))

(defun anvil-future-error (future)
  "Return the error payload of FUTURE, or nil if not errored."
  (and (eq 'error (anvil-future--status future))
       (anvil-future--err future)))

(defun anvil-future-await (future &optional timeout)
  "Block until FUTURE settles or TIMEOUT seconds elapse.
Return non-nil if settled, nil on timeout."
  (let* ((limit (or timeout anvil-offload-default-await-timeout))
         (deadline (and limit (+ (float-time) limit)))
         (proc (anvil-future--process future)))
    (while (and (not (anvil-future-done-p future))
                (or (null deadline) (< (float-time) deadline))
                (process-live-p proc))
      (accept-process-output proc anvil-offload-poll-interval))
    (anvil-future-done-p future)))

(defun anvil-future-cancel (future)
  "Drop local tracking for FUTURE and mark it cancelled.
The subprocess keeps running; its eventual reply is silently
discarded.  To hard-stop offload work, call
`anvil-offload-stop-repl' which terminates the REPL."
  (when (eq 'pending (anvil-future--status future))
    (remhash (anvil-future--id future) (anvil-offload--ensure-pending))
    (setf (anvil-future--status future) 'cancelled
          (anvil-future--done-at future) (float-time)))
  future)

;;; Process filter / sentinel

(defun anvil-offload--dispatch-reply (msg)
  "Route decoded reply MSG to its registered future, if any."
  (when (listp msg)
    (let* ((id (plist-get msg :id))
           (table (anvil-offload--ensure-pending))
           (future (and id (gethash id table))))
      (when future
        (remhash id table)
        (cond
         ((plist-member msg :ok)
          (setf (anvil-future--status future) 'done
                (anvil-future--result future) (plist-get msg :ok)
                (anvil-future--done-at future) (float-time)))
         ((plist-member msg :error)
          (setf (anvil-future--status future) 'error
                (anvil-future--err future) (plist-get msg :error)
                (anvil-future--done-at future) (float-time))))))))

(defun anvil-offload--filter (proc string)
  "Accumulate STRING bytes on PROC and dispatch complete S-exprs."
  (let ((buf (concat (or (process-get proc 'anvil-pending-bytes) "") string))
        (done nil))
    (while (not done)
      (setq buf (string-trim-left buf))
      (if (string-empty-p buf)
          (setq done t)
        (let ((parsed (condition-case err
                          (read-from-string buf)
                        (end-of-file :partial)
                        (invalid-read-syntax
                         (message "anvil-offload: unreadable reply: %s" err)
                         ;; Drop the junk up to the next newline (or all).
                         (let ((nl (string-match "\n" buf)))
                           (if nl
                               (cons nil (1+ nl))
                             (cons nil (length buf))))))))
          (if (eq parsed :partial)
              (setq done t)
            (setq buf (substring buf (cdr parsed)))
            (anvil-offload--dispatch-reply (car parsed))))))
    (process-put proc 'anvil-pending-bytes buf)))

(defun anvil-offload--sentinel (proc event)
  "Handle death of PROC; fail only the pending futures bound to PROC.
Filtering by `:process' is load-bearing: if the REPL is stopped
and a fresh one spawned before this sentinel runs, we must not
error-settle the new REPL's pending futures."
  (unless (process-live-p proc)
    (let ((table (anvil-offload--ensure-pending))
          (reason (format "offload REPL exited: %s" (string-trim event))))
      (maphash
       (lambda (id future)
         (when (eq proc (anvil-future--process future))
           (setf (anvil-future--status future) 'error
                 (anvil-future--err future) reason
                 (anvil-future--done-at future) (float-time))
           (remhash id table)))
       table))
    (when (eq proc anvil-offload--repl-process)
      (setq anvil-offload--repl-process nil))))

;;; REPL lifecycle

(defun anvil-offload--ensure-repl ()
  "Return the live REPL process, spawning it if necessary."
  (unless (and anvil-offload--repl-process
               (process-live-p anvil-offload--repl-process))
    (let* ((init-file (anvil-offload--repl-init-file))
           (proc (make-process
                  :name "anvil-offload-repl"
                  :buffer (get-buffer-create " *anvil-offload-repl*")
                  :command (list anvil-offload-emacs-bin
                                 "--batch"
                                 "-l" init-file)
                  :connection-type 'pipe
                  :coding 'utf-8-unix
                  :noquery t
                  :filter #'anvil-offload--filter
                  :sentinel #'anvil-offload--sentinel)))
      (process-put proc 'anvil-pending-bytes "")
      (setq anvil-offload--repl-process proc)))
  anvil-offload--repl-process)

;;;###autoload
(defun anvil-offload-stop-repl ()
  "Terminate the offload REPL.  Pending futures become errored."
  (interactive)
  (when (and anvil-offload--repl-process
             (process-live-p anvil-offload--repl-process))
    (kill-process anvil-offload--repl-process))
  (setq anvil-offload--repl-process nil))

;;;###autoload
(defun anvil-offload-repl-alive-p ()
  "Non-nil when the offload REPL subprocess is alive."
  (and anvil-offload--repl-process
       (process-live-p anvil-offload--repl-process)))

;;; Public entry point

;;;###autoload
(cl-defun anvil-offload (form &rest _keys)
  "Evaluate FORM in the offload REPL subprocess; return an `anvil-future'.

FORM is sent as a single S-expression.  The subprocess evaluates
it with lexical binding and sends back either `(:id N :ok VALUE)'
or `(:id N :error MSG)'.  The main daemon never blocks.

Additional keyword arguments are accepted but ignored in Phase 1
to reserve the shape of the API:
  :timeout SECONDS  — reserved for Phase 3 preemption
  :require LIST     — reserved; libraries to preload
  :env PLIST        — reserved"
  (let* ((proc (anvil-offload--ensure-repl))
         (id (cl-incf anvil-offload--next-id))
         (future (make-anvil-future
                  :id id :process proc :status 'pending)))
    (puthash id future (anvil-offload--ensure-pending))
    (process-send-string proc
                         (concat (prin1-to-string (list :id id :form form))
                                 "\n"))
    future))

;;; Module enable / disable (for `anvil-enable' integration)

;;;###autoload
(defun anvil-offload-enable ()
  "Enable the anvil-offload module.

Does *not* spawn the REPL — spawning is lazy on first
`anvil-offload' call.  Kept as a no-op so module registration
stays uniform across anvil modules."
  (interactive)
  t)

;;;###autoload
(defun anvil-offload-disable ()
  "Disable the anvil-offload module — stops the REPL if running."
  (interactive)
  (anvil-offload-stop-repl))

(provide 'anvil-offload)
;;; anvil-offload.el ends here

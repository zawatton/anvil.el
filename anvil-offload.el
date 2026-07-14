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
;;       KEYS :require, :load-path, :isolated, :on-start, :on-settle,
;;       :process-environment, :exec-path, :default-directory,
;;       :shell-file-name, :shell-command-switch, :exact-load-path.
;;   (anvil-future-done-p FUTURE)
;;   (anvil-future-await FUTURE &optional TIMEOUT)
;;   (anvil-future-value FUTURE)
;;   (anvil-future-error FUTURE)
;;   (anvil-future-cancel FUTURE)
;;   (anvil-future-kill FUTURE)          ; Phase 3a
;;   (anvil-future-checkpoint FUTURE)    ; Phase 3b
;;   (anvil-preempt-checkpoint V &optional C)  ; handler-side, Phase 3b
;;
;; Protocol:
;;   request to stdin            : raw sexp `(:id N :payload BASE64(FORM))'
;;   reply / checkpoint to stdout: PREFIX + BASE64(UTF-8(prin1(MSG))) + "\n"
;;     where decoded MSG is:
;;       (:id N :ok VALUE) | (:id N :error MSG)
;;       (:id N :checkpoint (:value V :cursor C))  (Phase 3b)
;;
;; Checkpoints are intermediate, non-settling messages sent by handlers
;; via `anvil-preempt-checkpoint' so the main daemon can return the last
;; known partial state if the call is killed for running over budget.
;;
;; Replies are framed so stray stdout chatter from handlers / `require'
;; does not poison the transport.  The REPL still uses
;; `send-string-to-terminal' because it calls fflush(stdout) in batch
;; mode — a plain `princ' may stay in the C stdio buffer on Windows
;; pipes and never reach the client.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Customization

(defgroup anvil-offload nil
  "Offload heavy elisp to a batch subprocess."
  :group 'anvil
  :prefix "anvil-offload-")

(defcustom anvil-offload-emacs-bin
  (let ((invoked-emacs
         (and (stringp invocation-name)
              (stringp invocation-directory)
              (expand-file-name invocation-name invocation-directory))))
    (or (and invoked-emacs
             (file-executable-p invoked-emacs)
             invoked-emacs)
        (executable-find "emacs")
        "emacs"))
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

(defcustom anvil-offload-pool-size 1
  "Number of REPL subprocesses in the offload pool.
Round-robin dispatch spreads offload requests across live slots;
futures bound to distinct slots execute in parallel.  Default 1
is the Phase 1 behaviour.  Changes take effect after the next
`anvil-offload-stop-repl'."
  :type 'integer
  :group 'anvil-offload)

(defcustom anvil-offload-max-frame-bytes (* 2 1024 1024)
  "Maximum bytes accepted for one offload protocol frame.
An over-limit frame fails every future owned by that subprocess and
terminates it before base64 decoding or Lisp reading can monopolize
the host Emacs."
  :type 'integer
  :group 'anvil-offload)

(defcustom anvil-offload-init-files nil
  "Files loaded in every offload subprocess before the REPL loop.
Dedicated backends use this to install a controlled environment.  Isolated
children start with `-Q' and do not clone live functions, features, buffers,
or buffer-local state from the root Emacs; list explicit initializer files
here when child expressions require that configuration.  Request-local
environment, directory, executable, shell, and load-path bindings are
transported separately around each submitted form."
  :type '(repeat file)
  :group 'anvil-offload)

(defcustom anvil-offload-spawn-environment-function nil
  "Optional function returning `process-environment' for child spawn.
The function is called immediately before `make-process'.  A nil
return means to copy the caller's current environment.  Request-local
environment bindings are transported separately and never affect
subprocess creation."
  :type '(choice (const :tag "Inherit caller environment" nil) function)
  :group 'anvil-offload)

;;; State

(defvar anvil-offload--pool nil
  "Vector of live REPL processes, or nil before the first dispatch.
Length matches `anvil-offload-pool-size' at the moment the pool
was initialised.  Each slot is either a live `make-process' or nil
\(unspawned / died and not yet respawned).")

(defvar anvil-offload--round-robin 0
  "Rolling index for pool dispatch.")

(defvar anvil-offload--next-id 0
  "Monotonic request ID counter.")

(defvar anvil-offload--pending nil
  "Hash table mapping request-id → `anvil-future'.
Created lazily in `anvil-offload--ensure-pending'.")

(defvar anvil-offload--isolated-processes nil
  "Hash table of one-shot subprocesses owned by isolated futures.")

(defvar anvil-offload--repl-init-file nil
  "Path to the generated REPL init file, or nil if not yet written.")

(defconst anvil-offload--protocol-version 3
  "Wire-format version spoken by the offload REPL pool.")

(defconst anvil-offload--frame-prefix "ANVIL-OFFLOAD "
  "Line prefix tagging framed stdout messages from the offload REPL.")

(defconst anvil-offload--ignored-junk-prefixes
  '("Lisp expression: ")
  "Known benign stdout prefixes emitted by the batch REPL.")

(defun anvil-offload--frame-encode-payload (string)
  "Return STRING encoded as a single-line transport payload."
  (base64-encode-string
   (encode-coding-string string 'utf-8-unix)
   t))

(defun anvil-offload--frame-decode-payload (payload)
  "Decode PAYLOAD from the offload transport into a UTF-8 string."
  (decode-coding-string
   (base64-decode-string payload)
   'utf-8-unix))

(defun anvil-offload--line-preview (string)
  "Return a short, single-line preview of STRING for diagnostics."
  (let ((flat (replace-regexp-in-string "[\r\n]+" "\\n" string)))
    (if (> (length flat) 120)
        (concat (substring flat 0 117) "...")
      flat)))

(defun anvil-offload--strip-ignored-junk-prefixes (string)
  "Drop known benign stdout prefixes from STRING."
  (let ((out string)
        changed)
    (while
        (progn
          (setq changed nil)
          (dolist (prefix anvil-offload--ignored-junk-prefixes)
            (when (string-prefix-p prefix out)
              (setq out (substring out (length prefix))
                    changed t)))
          changed))
    out))

(defun anvil-offload--ensure-pending ()
  "Return the pending-futures hash, creating it if needed."
  (or anvil-offload--pending
      (setq anvil-offload--pending (make-hash-table :test 'eql))))

(defun anvil-offload--ensure-isolated-processes ()
  "Return the isolated-process table, creating it if needed."
  (or (and (hash-table-p anvil-offload--isolated-processes)
           anvil-offload--isolated-processes)
      (setq anvil-offload--isolated-processes
            (make-hash-table :test 'eq))))

(defun anvil-offload--hard-delete-process (proc)
  "Hard-delete the exact owned subprocess PROC.
Pool and isolated-process tracking are cleared only after PROC is
observed dead.  If either termination operation signals or PROC stays
live, retain its tracking so an owned live child cannot become orphaned."
  (when (processp proc)
    (let (termination-errors)
      (when (process-live-p proc)
        (let ((inhibit-quit t))
          (condition-case err
              (kill-process proc)
            ((error quit)
             (push (format "kill-process: %s" (error-message-string err))
                   termination-errors)))
          (when (process-live-p proc)
            (condition-case err
                (delete-process proc)
              ((error quit)
               (push (format "delete-process: %s"
                             (error-message-string err))
                     termination-errors))))))
      (if (or termination-errors (process-live-p proc))
          (progn
            (message
             "anvil-offload: child %s remains tracked after failed hard delete%s"
             (process-name proc)
             (if termination-errors
                 (format ": %s"
                         (string-join (nreverse termination-errors) "; "))
               ""))
            nil)
        (when anvil-offload--pool
          (dotimes (i (length anvil-offload--pool))
            (when (eq proc (aref anvil-offload--pool i))
              (aset anvil-offload--pool i nil))))
        (when (hash-table-p anvil-offload--isolated-processes)
          (remhash proc anvil-offload--isolated-processes))
        t))))

;;; REPL init file

(defconst anvil-offload--repl-body
  (format ";; anvil-offload REPL — auto-generated, do not edit -*- lexical-binding: t; -*-
\(setq coding-system-for-read 'utf-8-unix
      coding-system-for-write 'utf-8-unix)
\(defconst anvil-offload--frame-prefix %S)
\(defun anvil-offload--emit-frame (msg)
  \"Write MSG as one framed line to stdout.\"
  (send-string-to-terminal
   (concat anvil-offload--frame-prefix
           (base64-encode-string
            (encode-coding-string (prin1-to-string msg) 'utf-8-unix)
            t)
           \"\\n\")))
\(defvar anvil-offload--repl-current-id nil
  \"Request id currently being evaluated — tags checkpoint messages.\")
\(defun anvil-preempt-checkpoint (value &optional cursor)
  \"Send an interim (:value VALUE :cursor CURSOR) checkpoint, return VALUE.
Handlers call this periodically during long work so the main daemon
has the latest partial state if the call is killed over budget.\"
  (when anvil-offload--repl-current-id
    (anvil-offload--emit-frame
     (list :id anvil-offload--repl-current-id
           :checkpoint (list :value value :cursor cursor))))
  value)
\(condition-case nil
    (while t
      (let* ((msg (read t))
             (id (and (listp msg) (plist-get msg :id)))
             (payload (and (listp msg) (plist-get msg :payload)))
             (quit-after (and (listp msg) (plist-get msg :quit-after)))
             (form (and payload
                        (car (read-from-string
                              (decode-coding-string
                               (base64-decode-string payload)
                               'utf-8-unix))))))
        (when id
          (let* ((anvil-offload--repl-current-id id)
                 (_started
                  (anvil-offload--emit-frame (list :id id :started t)))
                 (reply
                  (condition-case err
                      (list :id id
                            :ok
                            (with-temp-buffer
                              (let ((standard-output (current-buffer)))
                                (eval form t))))
                    (error (list :id id :error (format \"%%S\" err))))))
            (anvil-offload--emit-frame reply)
            (when quit-after
              (kill-emacs 0))))))
  (end-of-file (kill-emacs 0)))
" anvil-offload--frame-prefix)
  "Body of the REPL loop written into the subprocess init file.")

(defun anvil-offload--repl-init-file ()
  "Return the path to the REPL init file, rewriting it when stale."
  (unless (and anvil-offload--repl-init-file
               (file-exists-p anvil-offload--repl-init-file)
               (with-temp-buffer
                 (insert-file-contents anvil-offload--repl-init-file)
                 (equal (buffer-string) anvil-offload--repl-body)))
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
  status                 ; 'pending 'done 'error 'cancelled 'killed
  result
  err
  checkpoint             ; latest (:value V :cursor C) from subprocess, or nil
  started-at
  terminal-reason
  isolated-p
  on-start
  on-settle
  (created-at (float-time))
  done-at)

(defun anvil-offload--invoke-callback (future callback event)
  "Invoke CALLBACK for FUTURE, containing errors and quits from filters.
EVENT is a diagnostic symbol used only in the error message."
  (when callback
    (condition-case err
        (funcall callback future)
      ((error quit)
       (message "anvil-offload: %s callback failed for %s: %s"
                event (anvil-future--id future)
                (error-message-string err))))))

(defun anvil-offload--mark-started (future)
  "Mark pending FUTURE started and notify its start callback once."
  (when (and (eq 'pending (anvil-future--status future))
             (null (anvil-future--started-at future)))
    (setf (anvil-future--started-at future) (float-time))
    (let ((callback (anvil-future--on-start future))
          completed)
      (setf (anvil-future--on-start future) nil)
      (unwind-protect
          (progn
            (anvil-offload--invoke-callback future callback 'start)
            (setq completed t))
        (unless completed
          (anvil-future-kill future 'start-callback-aborted)))))
  future)

(defun anvil-offload--settle (future status &optional payload reason)
  "Settle pending FUTURE once with STATUS, PAYLOAD, and REASON.
STATUS is one of `done', `error', `cancelled', or `killed'."
  (when (eq 'pending (anvil-future--status future))
    (remhash (anvil-future--id future) (anvil-offload--ensure-pending))
    (setf (anvil-future--status future) status
          (anvil-future--terminal-reason future) reason
          (anvil-future--done-at future) (float-time))
    (pcase status
      ('done (setf (anvil-future--result future) payload))
      ((or 'error 'killed) (setf (anvil-future--err future) payload)))
    (let ((callback (anvil-future--on-settle future)))
      (setf (anvil-future--on-start future) nil
            (anvil-future--on-settle future) nil)
      (anvil-offload--invoke-callback future callback 'settle))
    t))

(defun anvil-future-status (future)
  "Return FUTURE's status (pending/done/error/cancelled/killed)."
  (anvil-future--status future))

(defun anvil-future-checkpoint (future)
  "Return the latest checkpoint plist for FUTURE, or nil.
The plist has keys `:value' and `:cursor', matching the arguments
last handed to `anvil-preempt-checkpoint' inside the REPL."
  (anvil-future--checkpoint future))

(defun anvil-future-done-p (future)
  "Non-nil when FUTURE has settled (done/error/cancelled/killed)."
  (not (eq (anvil-future--status future) 'pending)))

(defun anvil-future-value (future)
  "Return FUTURE's value; signal if errored, killed, cancelled, or pending."
  (pcase (anvil-future--status future)
    ('done      (anvil-future--result future))
    ('error     (error "anvil-offload: remote error: %s"
                       (anvil-future--err future)))
    ('killed    (error "anvil-offload: %s" (anvil-future--err future)))
    ('cancelled (error "anvil-offload: future was cancelled"))
    ('pending   (error "anvil-offload: future still pending"))
    (other      (error "anvil-offload: unknown status %S" other))))

(defun anvil-future-error (future)
  "Return the error payload of FUTURE, or nil if it has no error.
Both remotely errored and locally killed futures retain a diagnostic
payload."
  (and (memq (anvil-future--status future) '(error killed))
       (anvil-future--err future)))

(defun anvil-future-await (future &optional timeout)
  "Block until FUTURE settles or TIMEOUT seconds elapse.
Return non-nil if settled, nil on timeout.

Only output from FUTURE's REPL is accepted during each poll, while
timers (including `keyboard-quit' timers used by callers) remain
eligible to run.  If the REPL dies, settle its pending futures
synchronously rather than depending on the sentinel's zero-delay
fallback timer."
  (let* ((limit (or timeout anvil-offload-default-await-timeout))
         (deadline (and limit (+ (float-time) limit)))
         (proc (anvil-future--process future)))
    (while (and (not (anvil-future-done-p future))
                (or (null deadline) (< (float-time) deadline))
                (process-live-p proc))
      (accept-process-output
       proc anvil-offload-poll-interval nil t))
    (when (and (not (anvil-future-done-p future))
               (not (process-live-p proc)))
      ;; Give a final process-filter callback and the sentinel's zero-delay
      ;; fallback timer one bounded event-loop turn.  If neither settles the
      ;; future, do so synchronously; no pending future may outlive its REPL.
      (accept-process-output
       proc anvil-offload-poll-interval nil t)
      (unless (anvil-future-done-p future)
        (anvil-offload--finalize-dead-process
         proc
         (or (process-get proc 'anvil-offload-death-reason)
             (format "offload REPL exited: %s" (process-status proc))))))
    (anvil-future-done-p future)))

(defun anvil-future-cancel (future)
  "Drop local tracking for FUTURE and mark it cancelled.
The subprocess keeps running; its eventual reply is silently
discarded.  To hard-stop offload work, call `anvil-future-kill'
\(per-future) or `anvil-offload-stop-repl' (whole pool)."
  (anvil-offload--settle future 'cancelled nil 'cancelled)
  future)

(defun anvil-future-kill (future &optional reason)
  "Hard-kill the subprocess slot owning pending FUTURE.
Unlike `anvil-future-cancel', this attempts to terminate the REPL
process.  The future is removed from the pending table immediately.
Pool or isolated-process tracking is cleared only after the child is
observed dead.  If termination fails, its slot or table entry remains
tracked so an owned live child cannot become orphaned; the process
sentinel clears it when the child eventually exits.

The elapsed wall time (seconds since the future's `created-at') is
stored in `anvil-future--err'.  Repeated calls are idempotent.  Return
FUTURE."
  (when (eq 'pending (anvil-future--status future))
    (let* ((proc (anvil-future--process future))
           (elapsed (- (float-time) (anvil-future--created-at future)))
           (message (if reason
                        (format "%s after %.2fs" reason elapsed)
                      (format "killed after %.2fs" elapsed))))
      ;; Settle locally before signalling the child.  A synchronous
      ;; sentinel/filter callback can no longer race this future into a
      ;; different terminal state.
      (unwind-protect
          (anvil-offload--settle
           future 'killed message (or reason 'killed))
        (anvil-offload--hard-delete-process proc))))
  future)

(defun anvil-future-elapsed (future)
  "Return seconds between FUTURE's creation and now (or completion)."
  (- (or (anvil-future--done-at future) (float-time))
     (anvil-future--created-at future)))

;;; Process filter / sentinel

(defvar anvil-offload--repl-current-id nil
  "Bound to the request id while a handler runs inside the REPL.
Defined in the daemon as a no-op anchor so `anvil-preempt-checkpoint'
compiles; the subprocess's init file has its own defvar which is what
actually tags outbound checkpoint messages.")

(defun anvil-preempt-checkpoint (value &optional cursor)
  "Record VALUE (and optional CURSOR) as an interim checkpoint.
Inside the offload REPL subprocess this writes a
`(:id N :checkpoint (:value VALUE :cursor CURSOR))' message so the
main daemon can fold it into the `partial' reply on budget exceed.
In the main daemon this is a harmless no-op — handlers can call it
unconditionally.  Returns VALUE."
  (when (and anvil-offload--repl-current-id
             (fboundp 'send-string-to-terminal))
    (send-string-to-terminal
     (concat anvil-offload--frame-prefix
             (anvil-offload--frame-encode-payload
              (prin1-to-string
               (list :id anvil-offload--repl-current-id
                     :checkpoint (list :value value :cursor cursor))))
             "\n")))
  value)

(defun anvil-offload--settle-reply
    (proc future status payload reason)
  "Settle FUTURE from PROC and hard-delete a terminal isolated child."
  (let (settled)
    (unwind-protect
        (setq settled
              (anvil-offload--settle future status payload reason))
      ;; The child has already flushed a complete terminal frame.  Delete the
      ;; exact one-shot process here instead of trusting its subsequent
      ;; `kill-emacs': evaluated code or initializer files may install a
      ;; blocking exit hook, and a completed job no longer owns a deadline.
      ;; This cleanup must also survive a tagged nonlocal exit from a public
      ;; callback invoked during settlement.
      (when (and (anvil-future--isolated-p future)
                 (eq proc (anvil-future--process future)))
        (anvil-offload--hard-delete-process proc)))
    settled))

(defun anvil-offload--dispatch-reply (proc msg)
  "Route decoded reply MSG from PROC to its registered future, if any.
Every started, checkpoint, and terminal frame is accepted only from the
process that owns its request id.  Terminal replies hard-delete an isolated
one-shot child after settlement."
  (when (listp msg)
    (let* ((id (plist-get msg :id))
           (table (anvil-offload--ensure-pending))
           (future (and id (gethash id table))))
      (when future
        (if (not (eq proc (anvil-future--process future)))
            (unless (process-get proc 'anvil-owner-mismatch-logged)
              (process-put proc 'anvil-owner-mismatch-logged t)
              (message "anvil-offload: ignored request %s frame from %s"
                       id (process-name proc)))
          (cond
           ((plist-member msg :started)
            (anvil-offload--mark-started future))
           ((plist-member msg :checkpoint)
            (setf (anvil-future--checkpoint future)
                  (plist-get msg :checkpoint)))
           ((plist-member msg :ok)
            (anvil-offload--settle-reply
             proc future 'done (plist-get msg :ok) 'done))
           ((plist-member msg :error)
            (anvil-offload--settle-reply
             proc future 'error (plist-get msg :error) 'remote-error))))))))

(defun anvil-offload--filter (proc string)
  "Accumulate STRING bytes on PROC and dispatch complete framed replies.
The byte ceiling applies to each newline-terminated frame and to the one
unterminated remainder, not to an arbitrary process-filter chunk that may
coalesce several independently valid frames."
  (let ((buf (concat (or (process-get proc 'anvil-pending-bytes) "") string))
        (prefix-re (regexp-quote anvil-offload--frame-prefix))
        line-end failure)
    (while (and (null failure)
                (setq line-end (string-match "\n" buf)))
      (let ((raw-line (substring buf 0 line-end)))
        (setq buf (substring buf (1+ line-end)))
        (if (> (1+ (string-bytes raw-line))
               anvil-offload-max-frame-bytes)
            (setq failure
                  (format "offload frame exceeded %d-byte limit"
                          anvil-offload-max-frame-bytes))
          (let ((line (anvil-offload--strip-ignored-junk-prefixes raw-line)))
            (unless (string-blank-p line)
              (let ((idx (string-match prefix-re line)))
                (cond
                 ((null idx)
                  (unless (process-get proc 'anvil-junk-reply-logged)
                    (process-put proc 'anvil-junk-reply-logged t)
                    (message "anvil-offload: dropped junk reply line: %S"
                             (anvil-offload--line-preview line))))
                 (t
                  (condition-case err
                      (anvil-offload--dispatch-reply
                       proc
                       (car
                        (read-from-string
                         (anvil-offload--frame-decode-payload
                          (substring
                           line
                           (+ idx (length anvil-offload--frame-prefix)))))))
                    (error
                     (message "anvil-offload: unreadable reply frame: %s"
                              err)))))))))))
    (when (and (null failure)
               (> (string-bytes buf) anvil-offload-max-frame-bytes))
      (setq failure
            (format "offload frame exceeded %d-byte limit"
                    anvil-offload-max-frame-bytes)))
    (if failure
        (progn
          (process-put proc 'anvil-pending-bytes "")
          (unwind-protect
              (anvil-offload--finalize-dead-process proc failure)
            (anvil-offload--hard-delete-process proc)))
      (process-put proc 'anvil-pending-bytes buf))))

(defun anvil-offload--finalize-dead-process (proc reason)
  "Settle pending futures still owned by dead PROC with REASON."
  (let ((table (anvil-offload--ensure-pending))
        futures)
    (maphash
     (lambda (_id future)
       (when (and (eq proc (anvil-future--process future))
                  (eq 'pending (anvil-future--status future)))
         (push future futures)))
     table)
    (unwind-protect
        (dolist (future futures)
          (anvil-offload--settle future 'error reason 'child-exit))
      ;; A tagged nonlocal exit from one public callback must not leave sibling
      ;; futures pending forever on a process that is already dead.  Their
      ;; callbacks cannot safely run while unwinding, but their terminal state
      ;; and pending-table cleanup are mandatory.
      (dolist (future futures)
        (when (eq 'pending (anvil-future--status future))
          (setf (anvil-future--on-start future) nil
                (anvil-future--on-settle future) nil)
          (anvil-offload--settle future 'error reason 'child-exit))))))

(defun anvil-offload--sentinel (proc event)
  "Handle death of PROC; fail only the pending futures bound to PROC.
Filtering by `:process' is load-bearing: if the REPL is stopped and a
fresh one spawned before this sentinel runs, we must not error-settle
the new REPL's pending futures.  EVENT describes the process status."
  (unless (process-live-p proc)
    (let ((reason (format "offload REPL exited: %s" (string-trim event))))
      (process-put proc 'anvil-offload-death-reason reason)
      ;; Let any final filter callback drain queued bytes before we mark
      ;; still-pending futures as errored.  `anvil-future-await' also
      ;; performs this finalization synchronously after it has serviced
      ;; PROC, so this timer is an idempotent fallback for other callers.
      (run-at-time 0 nil #'anvil-offload--finalize-dead-process proc reason))
    (remhash proc (anvil-offload--ensure-isolated-processes))
    ;; Clear the dying slot so the next dispatch respawns it.
    (when anvil-offload--pool
      (dotimes (i (length anvil-offload--pool))
        (when (eq proc (aref anvil-offload--pool i))
          (aset anvil-offload--pool i nil))))
    (when (process-get proc 'anvil-offload-isolated)
      (when-let ((buffer (process-buffer proc)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

;;; Pool lifecycle

(defun anvil-offload--process-command ()
  "Return the command used for an offload subprocess."
  (append
   (list anvil-offload-emacs-bin "-Q" "--batch")
   (cl-mapcan (lambda (file) (list "-l" file)) anvil-offload-init-files)
   (list "-l" (anvil-offload--repl-init-file))))

(defun anvil-offload--spawn-named-process (name &optional isolated)
  "Spawn a REPL subprocess named NAME.
When ISOLATED is non-nil, record it as a one-shot owned process."
  (let* ((candidate
          (and anvil-offload-spawn-environment-function
               (funcall anvil-offload-spawn-environment-function)))
         (process-environment
          (copy-sequence (or candidate process-environment)))
         (proc (make-process
                :name name
                :buffer (get-buffer-create (format " *%s*" name))
                :command (anvil-offload--process-command)
                :connection-type 'pipe
                :coding 'utf-8-unix
                :noquery t
                :filter #'anvil-offload--filter
                :sentinel #'anvil-offload--sentinel)))
    (process-put proc 'anvil-pending-bytes "")
    (process-put proc 'anvil-offload-protocol-version
                 anvil-offload--protocol-version)
    (when isolated
      (process-put proc 'anvil-offload-isolated t)
      (puthash proc t (anvil-offload--ensure-isolated-processes)))
    proc))

(defun anvil-offload--spawn-process (slot-index)
  "Spawn a fresh pooled REPL subprocess tagged for SLOT-INDEX."
  (anvil-offload--spawn-named-process
   (format "anvil-offload-repl-%d" slot-index)))

(defun anvil-offload--spawn-isolated-process (request-id)
  "Spawn a one-shot REPL subprocess for REQUEST-ID."
  (anvil-offload--spawn-named-process
   (format "anvil-offload-job-%d" request-id) t))

(defun anvil-offload--ensure-pool-vector ()
  "Ensure `anvil-offload--pool' is sized to the current pool size.
When an old child resists termination, retain the old vector so its
sole ownership reference is not lost; a later call retries the resize."
  (let ((n (max 1 anvil-offload-pool-size)))
    (unless (and anvil-offload--pool
                 (= (length anvil-offload--pool) n))
      (when anvil-offload--pool
        (dotimes (i (length anvil-offload--pool))
          (let ((proc (aref anvil-offload--pool i)))
            (when proc
              (anvil-offload--hard-delete-process proc)))))
      (unless (and anvil-offload--pool
                   (cl-some #'identity
                            (append anvil-offload--pool nil)))
        (setq anvil-offload--pool (make-vector n nil))))
    anvil-offload--pool))

(defun anvil-offload--ensure-slot (idx)
  "Ensure slot IDX holds a live, protocol-compatible REPL; return it.
Refuse replacement while the old child remains live so the pool cannot
lose its sole ownership reference."
  (anvil-offload--ensure-pool-vector)
  (let ((cur (aref anvil-offload--pool idx)))
    (if (and cur
             (process-live-p cur)
             (eq (process-get cur 'anvil-offload-protocol-version)
                 anvil-offload--protocol-version))
        cur
      (when (and cur
                 (not (anvil-offload--hard-delete-process cur)))
        (error "anvil-offload: cannot replace live child in slot %d" idx))
      (let ((proc (anvil-offload--spawn-process idx)))
        (aset anvil-offload--pool idx proc)
        proc))))

(defun anvil-offload--pick-worker ()
  "Return a live REPL from the pool via round-robin."
  (anvil-offload--ensure-pool-vector)
  (let ((n (length anvil-offload--pool)))
    (anvil-offload--ensure-slot (mod (cl-incf anvil-offload--round-robin) n))))

;;;###autoload
(defun anvil-offload-stop-repl ()
  "Terminate every pooled and isolated REPL without losing ownership.
Pending futures bound to those processes settle as errored via
`anvil-offload--sentinel'.  Subsequent dispatch rebuilds every slot
whose child terminated, using the current `anvil-offload-pool-size'.
A child that remains live after failed termination stays tracked for
a later sentinel or cleanup attempt."
  (interactive)
  (when anvil-offload--pool
    (dotimes (i (length anvil-offload--pool))
      (let ((p (aref anvil-offload--pool i)))
        (when p
          (anvil-offload--hard-delete-process p))))
    (unless (cl-some #'identity (append anvil-offload--pool nil))
      (setq anvil-offload--pool nil)))
  (when (hash-table-p anvil-offload--isolated-processes)
    (let (processes)
      (maphash (lambda (proc _value) (push proc processes))
               anvil-offload--isolated-processes)
      (dolist (proc processes)
        (anvil-offload--hard-delete-process proc)))))

;;;###autoload
(defun anvil-offload-repl-alive-p ()
  "Non-nil when at least one pooled REPL is alive."
  (and anvil-offload--pool
       (cl-some (lambda (p) (and p (process-live-p p)))
                (append anvil-offload--pool nil))))

;;;###autoload
(defun anvil-offload-pool-status ()
  "Return a list of slot descriptors describing the pool.
Each element is `(:slot IDX :alive t-or-nil :pid PID-or-nil)'."
  (and anvil-offload--pool
       (cl-loop for i below (length anvil-offload--pool)
                for p = (aref anvil-offload--pool i)
                collect (list :slot i
                              :alive (and p (process-live-p p) t)
                              :pid (and p (process-live-p p)
                                        (process-id p))))))

;;; Public entry point

(defun anvil-offload--build-preamble (requires extra-load-path)
  "Build the preamble forms to run before the user FORM in the REPL.
REQUIRES is a feature symbol or list of symbols to (require \\='X).
EXTRA-LOAD-PATH is a list of directories to prepend to `load-path'
in the subprocess.  Returns a list of forms (possibly empty)."
  (let ((features (cond
                   ((null requires) nil)
                   ((symbolp requires) (list requires))
                   ((listp requires) requires)
                   (t (error "anvil-offload :require must be symbol or list, got %S"
                             requires)))))
    (append
     (and extra-load-path
          ;; `add-to-list' prepends, so emit the additions in reverse to
          ;; preserve the caller's left-to-right search precedence.
          (list `(dolist (d ',(reverse extra-load-path))
                   (add-to-list 'load-path d))))
     (mapcar (lambda (f) `(require ',f)) features))))

(defun anvil-offload--bind-context (form keys)
  "Wrap FORM in request-local dynamic bindings selected from KEYS."
  (let (bindings)
    (dolist (entry '((:process-environment . process-environment)
                     (:exec-path . exec-path)
                     (:default-directory . default-directory)
                     (:shell-file-name . shell-file-name)
                     (:shell-command-switch . shell-command-switch)
                     (:exact-load-path . load-path)))
      (when (plist-member keys (car entry))
        (push (list (cdr entry) (list 'quote (plist-get keys (car entry))))
              bindings)))
    (if bindings `(let ,(nreverse bindings) ,form) form)))

;;;###autoload
(cl-defun anvil-offload (form &rest keys)
  "Evaluate FORM in the offload REPL subprocess; return an `anvil-future'.

FORM is sent as a single S-expression.  The subprocess evaluates
its printed form via a base64 payload inside the request sexp.  The
subprocess evaluates it with lexical binding and sends back either
`(:id N :ok VALUE)' or `(:id N :error MSG)'.  The main daemon never
blocks.

Keyword arguments:
  :require FEATURES   Symbol or list of symbols to `require' in the
                      subprocess before FORM is evaluated.
  :load-path DIRS     List of directories prepended to `load-path'
                      in the subprocess (applied before :require).
  :isolated BOOL      Use a fresh one-shot subprocess owned only by
                      this future.
  :on-start FN        Called once with the future before evaluation.
  :on-settle FN       Called once with the terminal future.
  :process-environment, :exec-path, :default-directory,
  :shell-file-name, :shell-command-switch
                      Request-local bindings in the subprocess.
  :exact-load-path DIRS
                      Replace `load-path' exactly around FORM.  This is used
                      by isolated async evaluation to preserve request search
                      precedence; ordinary callers normally use :load-path.

Dispatch uses round-robin across the pool (`anvil-offload-pool-size')."
  (let* ((requires (plist-get keys :require))
         (extra-load-path (plist-get keys :load-path))
         (preamble (anvil-offload--build-preamble requires extra-load-path))
         (id (cl-incf anvil-offload--next-id))
         (isolated (plist-get keys :isolated))
         (context-form (anvil-offload--bind-context form keys))
         (full-form
          (if preamble `(progn ,@preamble ,context-form) context-form))
         (proc (if isolated
                   (anvil-offload--spawn-isolated-process id)
                 (anvil-offload--pick-worker)))
         (future (make-anvil-future
                  :id id
                  :process proc
                  :status 'pending
                  :isolated-p isolated
                  :on-start (plist-get keys :on-start)
                  :on-settle (plist-get keys :on-settle))))
    (puthash id future (anvil-offload--ensure-pending))
    (condition-case err
        (process-send-string
         proc
         (concat
          (prin1-to-string
           (list :id id
                 :quit-after (and isolated t)
                 :payload
                 (anvil-offload--frame-encode-payload
                  (let ((print-circle t)) (prin1-to-string full-form)))))
          "\n"))
      (error
       (unwind-protect
           (progn
             (anvil-offload--settle
              future 'error (format "offload send failed: %s"
                                    (error-message-string err))
              'send-error)
             (signal (car err) (cdr err)))
         (anvil-offload--hard-delete-process proc))))
    future))

;;;###autoload
(cl-defun anvil-offload-isolated (form &rest keys)
  "Evaluate FORM in a fresh one-shot subprocess and return its future."
  (apply #'anvil-offload form :isolated t keys))

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

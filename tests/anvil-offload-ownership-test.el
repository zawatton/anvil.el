;;; anvil-offload-ownership-test.el --- Transactional custody regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Adversarial regressions for issue #53.  Every fixture dynamically binds all
;; ownership generations and proves cleanup convergence before scope exit.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'anvil-offload)

(defconst anvil-offload-ownership-test--prefix "anvil-ownership-")

(defun anvil-offload-ownership-test--idle-process (name &optional buffer)
  "Return a quiet process named NAME, optionally using BUFFER."
  (make-process
   :name name
   :buffer buffer
   :command (list shell-file-name shell-command-switch "sleep 30")
   :connection-type 'pipe
   :noquery t
   :sentinel #'anvil-offload--sentinel))

(defun anvil-offload-ownership-test--compatible (proc)
  "Tag PROC with the current offload protocol and return it."
  (process-put proc 'anvil-offload-protocol-version
               anvil-offload--protocol-version)
  proc)

(defun anvil-offload-ownership-test--owned-p (proc)
  "Return non-nil when any registered table owns PROC."
  (cl-some
   (lambda (table) (gethash proc table))
   (anvil-offload--registered-ownership-tables)))

(defun anvil-offload-ownership-test--assert-clean ()
  "Assert that every bound ownership channel has converged."
  (let ((deadline (+ (float-time) 1.0)))
    (while (and (hash-table-p anvil-offload--pending)
                (> (hash-table-count anvil-offload--pending) 0)
                (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (let (owned pending)
    (dolist (table (anvil-offload--registered-ownership-tables))
      (maphash
       (lambda (proc value) (push (list table proc value) owned))
       table))
    (when (hash-table-p anvil-offload--pending)
      (maphash (lambda (id _future) (push id pending))
               anvil-offload--pending))
    (should-not anvil-offload--pool)
    (should-not anvil-offload--pool-retiring-p)
    (should-not anvil-offload--pool-cleanup-active-p)
    (should-not anvil-offload--submission-active-p)
    (should-not anvil-offload--stop-retiring-p)
    (should-not anvil-offload--retired-pools)
    (should-not owned)
    (should-not pending)))

(defun anvil-offload-ownership-test--cleanup ()
  "Delete test children and prove bound ownership convergence."
  (dolist (proc (process-list))
    (when (string-prefix-p anvil-offload-ownership-test--prefix
                           (process-name proc))
      (set-process-sentinel proc #'anvil-offload--sentinel)
      (anvil-offload--hard-delete-process proc)))
  (anvil-offload-stop-repl)
  (anvil-offload-ownership-test--assert-clean))

(defmacro anvil-offload-ownership-test--with-state (&rest body)
  "Run BODY with dynamically isolated offload ownership state."
  (declare (indent 0))
  `(let ((anvil-offload--pool nil)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--isolated-processes (make-hash-table :test 'eq))
         (anvil-offload--pending (make-hash-table :test 'eql))
         (anvil-offload--round-robin 0)
         (anvil-offload--next-id 0)
         (anvil-offload-pool-size 1)
         (anvil-offload-spawn-environment-function nil))
     (unwind-protect
         (progn ,@body)
       (anvil-offload-ownership-test--cleanup))))

(defun anvil-offload-ownership-test--frame (message)
  "Encode MESSAGE as one complete offload frame."
  (concat anvil-offload--frame-prefix
          (anvil-offload--frame-encode-payload
           (prin1-to-string message))
          "\n"))

(defun anvil-offload-ownership-test--future (id proc &rest keys)
  "Return a production-shaped registered future for ID owned by PROC."
  (apply
   #'make-anvil-future
   :id id
   :process proc
   :status 'pending
   :registration-p t
   :registered-id id
   :registered-process proc
   :registered-status 'pending
   :registered-isolated-p (and (plist-get keys :isolated-p) t)
   keys))

(ert-deftest anvil-offload-ownership-retired-retry-sweeps-all-generations ()
  "A later resize cannot publish while an older retired child survives."
  (anvil-offload-ownership-test--with-state
    (let* ((stubborn
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-retired-stubborn"))
           (old (vector stubborn))
           (replacement (vector nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           swapped)
      (cl-letf (((symbol-function 'kill-process)
                 (lambda (_proc)
                   (unless swapped
                     (setq swapped t
                           anvil-offload--pool replacement))))
                ((symbol-function 'delete-process) #'ignore))
        (should-error (anvil-offload--ensure-pool-vector))
        (should (eq replacement anvil-offload--pool))
        (should (memq old anvil-offload--retired-pools))
        (should anvil-offload--pool-retiring-p)
        (should-error (anvil-offload--ensure-pool-vector))
        (should (eq replacement anvil-offload--pool))
        (should (memq old anvil-offload--retired-pools))
        (should (process-live-p stubborn)))
      (let ((fresh (anvil-offload--ensure-pool-vector)))
        (should (= 2 (length fresh)))
        (should-not (eq fresh replacement))
        (should-not anvil-offload--retired-pools)
        (should-not anvil-offload--pool-retiring-p)
        (should-not (process-live-p stubborn))))))

(ert-deftest anvil-offload-ownership-pool-watcher-signal-keeps-old-custody ()
  "A signaling pool watcher sees the guard and prevents publication."
  (anvil-offload-ownership-test--with-state
    (let* ((old (vector nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           (anvil-offload--next-id 17)
           (count 0)
           active reentrant)
      (let ((watcher
             (lambda (_symbol _new operation _where)
               (when (eq operation 'set)
                 (cl-incf count)
                 (setq active anvil-offload--pool-cleanup-active-p)
                 (condition-case err
                     (anvil-offload '(+ 1 2) :isolated t)
                   (error (setq reentrant err)))
                 (error "ownership watcher stopped publication")))))
        (add-variable-watcher 'anvil-offload--pool watcher)
        (unwind-protect
            (should-error (anvil-offload--ensure-pool-vector))
          (remove-variable-watcher 'anvil-offload--pool watcher)))
      (should (= 1 count))
      (should active)
      (should (string-match-p "already active"
                              (error-message-string reentrant)))
      (should (= 17 anvil-offload--next-id))
      (should (eq old anvil-offload--pool))
      (should anvil-offload--pool-retiring-p)
      (should-not anvil-offload--retired-pools)
      (should (= 2 (length (anvil-offload--ensure-pool-vector)))))))

(ert-deftest anvil-offload-ownership-slot-swap-retains-stubborn-exact-child ()
  "A slot delete callback cannot orphan its exact surviving child."
  (anvil-offload-ownership-test--with-state
    (let* ((cur
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-slot-stubborn"))
           (peer
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-slot-peer")))
           (old (vector cur peer))
           (replacement (vector peer nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           swapped)
      (process-put cur 'anvil-offload-protocol-version
                   (1- anvil-offload--protocol-version))
      (cl-letf (((symbol-function 'kill-process)
                 (lambda (_proc)
                   (unless swapped
                     (setq swapped t
                           anvil-offload--pool replacement))))
                ((symbol-function 'delete-process) #'ignore))
        (should-error (anvil-offload--ensure-slot 0)))
      (should (eq replacement anvil-offload--pool))
      (should (process-live-p cur))
      (should (process-live-p peer))
      (should (anvil-offload-ownership-test--owned-p cur))
      (anvil-offload-stop-repl)
      (should-not (process-live-p cur))
      (should-not (process-live-p peer))
      (should-not (anvil-offload-ownership-test--owned-p cur)))))

(ert-deftest anvil-offload-ownership-isolated-only-stop-remains-fail-closed ()
  "An auxiliary-only survivor keeps stop-retiring until a later retry."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-isolated-stop"))
           (other (make-hash-table :test 'eq))
           (anvil-offload--ownership-table-registry
            (list other anvil-offload--isolated-processes))
           (anvil-offload--next-id 23)
           spawned)
      (puthash proc t other)
      (cl-letf (((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore)
                ((symbol-function 'anvil-offload--spawn-isolated-process)
                 (lambda (_id) (setq spawned t) proc)))
        (anvil-offload-stop-repl)
        (should anvil-offload--stop-retiring-p)
        (should anvil-offload--pool-retiring-p)
        (should (gethash proc other))
        (should-error (anvil-offload '(+ 1 2) :isolated t))
        (should-error (anvil-offload--ensure-pool-vector))
        (should (= 23 anvil-offload--next-id))
        (should-not spawned))
      (anvil-offload-stop-repl)
      (should-not anvil-offload--stop-retiring-p)
      (should-not anvil-offload--pool-retiring-p)
      (should-not (gethash proc other))
      (should-not (process-live-p proc)))))

(ert-deftest anvil-offload-ownership-submission-guard-blocks-callback-stop ()
  "Environment callbacks cannot stop or recursively submit mid-publication."
  (anvil-offload-ownership-test--with-state
    (let (stop-error reentrant-error observed)
      (setq anvil-offload-spawn-environment-function
            (lambda ()
              (setq observed anvil-offload--submission-active-p)
              (condition-case err
                  (anvil-offload-stop-repl)
                (error (setq stop-error err)))
              (condition-case err
                  (anvil-offload '(+ 4 5) :isolated t)
                (error (setq reentrant-error err)))
              (copy-sequence process-environment)))
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30"))))
        (let ((future (anvil-offload '(+ 1 2) :isolated t)))
          (should observed)
          (should (string-match-p "submission already active"
                                  (error-message-string stop-error)))
          (should (string-match-p "submission already active"
                                  (error-message-string reentrant-error)))
          (should (= 1 anvil-offload--next-id))
          (anvil-future-kill future 'test-cleanup))))))

(ert-deftest anvil-offload-ownership-first-table-watcher-prevents-spawn ()
  "A first-table watcher signal occurs before any child is created."
  (anvil-offload-ownership-test--with-state
    (setq anvil-offload--isolated-processes nil)
    (let ((make-count 0)
          watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (eq operation 'set)
                (error "ownership table watcher"))))
      (add-variable-watcher 'anvil-offload--isolated-processes watcher)
      (unwind-protect
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest _args)
                       (cl-incf make-count)
                       (error "make-process unexpectedly called"))))
            (should-error (anvil-offload--spawn-isolated-process 1)))
        (remove-variable-watcher 'anvil-offload--isolated-processes watcher))
      (should (= 0 make-count)))))

(ert-deftest anvil-offload-ownership-registered-table-is-not-republished ()
  "Post-spawn custody does not reassign an already registered watched table."
  (anvil-offload-ownership-test--with-state
    (let ((real-make (symbol-function 'make-process))
          created
          result
          (registry-sets 0)
          watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (eq operation 'set)
                (cl-incf registry-sets)
                (when (and (processp created)
                           (process-live-p created))
                  (error "registry watcher after spawn")))))
      (add-variable-watcher
       'anvil-offload--ownership-table-registry watcher)
      (unwind-protect
          (cl-letf
              (((symbol-function 'anvil-offload--process-command)
                (lambda ()
                  (list shell-file-name shell-command-switch "sleep 30")))
               ((symbol-function 'make-process)
                (lambda (&rest args)
                  (setq created (apply real-make args)))))
            (setq result
                  (anvil-offload--spawn-named-process
                   "anvil-ownership-registry-watcher" t)))
        (remove-variable-watcher
         'anvil-offload--ownership-table-registry watcher))
      (should (eq created result))
      (should (= 1 registry-sets))
      (should (process-live-p result))
      (should (anvil-offload-ownership-test--owned-p result))
      (should (anvil-offload--hard-delete-process result))
      (should-not (process-live-p result)))))

(ert-deftest anvil-offload-ownership-hard-delete-marks-before-table-watcher ()
  "A first-table watcher cannot reacquire the exact retiring child."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-delete-watcher")))
           (anvil-offload--pool (vector proc))
           (anvil-offload--isolated-processes nil)
           fired observed reentrant watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'set) (not fired))
                (setq fired t
                      observed
                      (process-get proc 'anvil-offload-retiring))
                (condition-case err
                    (anvil-offload--ensure-slot 0)
                  (error (setq reentrant err)))
                (error "hard-delete table watcher"))))
      (add-variable-watcher 'anvil-offload--isolated-processes watcher)
      (unwind-protect
          (cl-letf (((symbol-function 'kill-process) #'ignore)
                    ((symbol-function 'delete-process) #'ignore))
            (should-error (anvil-offload--hard-delete-process proc)))
        (remove-variable-watcher
         'anvil-offload--isolated-processes watcher))
      (should fired)
      (should observed)
      (should reentrant)
      (should (process-live-p proc))
      (should (eq proc (aref anvil-offload--pool 0)))
      (should (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-stop-watcher-keeps-fail-closed-state ()
  "A signaling final-publication watcher leaves stop fail closed."
  (anvil-offload-ownership-test--with-state
    (let* ((old (vector nil))
           (anvil-offload--pool old)
           (anvil-offload--next-id 71)
           fired observed reentrant watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'set) (not fired))
                (setq fired t
                      observed
                      (list anvil-offload--pool-cleanup-active-p
                            anvil-offload--pool-retiring-p
                            anvil-offload--stop-retiring-p))
                (condition-case err
                    (anvil-offload '(+ 1 2) :isolated t)
                  (error (setq reentrant err)))
                (error "stop pool watcher"))))
      (add-variable-watcher 'anvil-offload--pool watcher)
      (unwind-protect
          (should-error (anvil-offload-stop-repl))
        (remove-variable-watcher 'anvil-offload--pool watcher))
      (should fired)
      (should (equal observed '(t t t)))
      (should (string-match-p "cleanup already active"
                              (error-message-string reentrant)))
      (should (= 71 anvil-offload--next-id))
      (should (eq old anvil-offload--pool))
      (should anvil-offload--pool-retiring-p)
      (should anvil-offload--stop-retiring-p)
      (anvil-offload-stop-repl)
      (should-not anvil-offload--pool)
      (should-not anvil-offload--pool-retiring-p)
      (should-not anvil-offload--stop-retiring-p))))

(ert-deftest anvil-offload-ownership-spawn-unwind-prefers-exact-local-proc ()
  "A post-return staging error owns the local proc, not a buffer peer."
  (anvil-offload-ownership-test--with-state
    (let* ((name "anvil-ownership-exact-spawn")
           (buffer (get-buffer-create (format " *%s*" name)))
           (prior
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-exact-prior" buffer))
           (other-buffer (get-buffer-create " *anvil-ownership-exact-new*"))
           (real-process-put (symbol-function 'process-put))
           new fired)
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30")))
                ((symbol-function 'process-put)
                 (lambda (proc property value)
                   (when (and (processp proc)
                              (eq property 'anvil-offload-ownership-tables)
                              (string= name (process-name proc))
                              (not fired))
                     (setq fired t new proc)
                     (set-process-buffer proc other-buffer)
                     (error "post-return staging failure"))
                   (funcall real-process-put proc property value)))
                ((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (should-error (anvil-offload--spawn-named-process name t)))
      (should fired)
      (should (process-live-p new))
      (should (process-live-p prior))
      (should (anvil-offload-ownership-test--owned-p new))
      (should-not (anvil-offload-ownership-test--owned-p prior)))))

(ert-deftest anvil-offload-ownership-spawn-quit-retains-exact-custody ()
  "Quit after child creation leaves the exact child globally retriable."
  (anvil-offload-ownership-test--with-state
    (let ((real-make (symbol-function 'make-process))
          created quit-seen)
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30")))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq created (apply real-make args))
                   (signal 'quit nil)))
                ((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (condition-case nil
            (anvil-offload--spawn-named-process
             "anvil-ownership-spawn-quit" t)
          (quit (setq quit-seen t))))
      (should quit-seen)
      (should (process-live-p created))
      (should (anvil-offload-ownership-test--owned-p created)))))

(ert-deftest anvil-offload-ownership-spawn-bypasses-puthash-advice ()
  "Runtime hash advice cannot interrupt first child custody publication."
  (anvil-offload-ownership-test--with-state
    (let ((real-puthash (symbol-function 'puthash))
          proc
          (process-key-calls 0))
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30")))
                ((symbol-function 'puthash)
                 (lambda (key value table)
                   (if (processp key)
                       (progn
                         (cl-incf process-key-calls)
                         (error "process-key puthash refused"))
                     (funcall real-puthash key value table)))))
        (setq proc
              (anvil-offload--spawn-named-process
               "anvil-ownership-puthash-advice" t)))
      (should (process-live-p proc))
      (should (= 0 process-key-calls))
      (should (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-spawn-recovers-moved-buffer-child ()
  "A child moved before make-process throws remains globally owned."
  (anvil-offload-ownership-test--with-state
    (let ((real-make (symbol-function 'make-process))
          (other-buffer (get-buffer-create " *anvil-ownership-moved-child*"))
          created outcome)
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30")))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq created (apply real-make args))
                   (set-process-buffer created other-buffer)
                   (throw 'anvil-ownership-moved-exit 'moved-exit)))
                ((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (setq outcome
              (catch 'anvil-ownership-moved-exit
            (anvil-offload--spawn-named-process
                 "anvil-ownership-moved-spawn" t)
                'returned)))
      (should (eq 'moved-exit outcome))
      (should (process-live-p created))
      (should (eq other-buffer (process-buffer created)))
      (should (anvil-offload-ownership-test--owned-p created)))))

(defun anvil-offload-ownership-test--exercise-send-exit (isolated exit)
  "Exercise public send EXIT for an ISOLATED or pooled submission."
  (let* ((proc
          (anvil-offload-ownership-test--compatible
           (anvil-offload-ownership-test--idle-process
            (format "anvil-ownership-send-%s-%s" isolated exit))))
         (table anvil-offload--isolated-processes)
         (real-maker (symbol-function 'make-anvil-future))
         future callback-called pending-seen outcome)
    (unless isolated
      (setq anvil-offload--pool (vector proc)))
    (cl-letf
        (((symbol-function 'make-anvil-future)
          (lambda (&rest args)
            (setq future (apply real-maker args))))
         ((symbol-function 'anvil-offload--pick-worker)
          (lambda () proc))
         ((symbol-function 'anvil-offload--spawn-isolated-process)
          (lambda (_id)
            (process-put proc 'anvil-offload-isolated t)
            (anvil-offload--track-process-ownership proc t table)
            proc))
         ((symbol-function 'process-send-string)
          (lambda (_proc _string)
            (setq pending-seen
                  (= 1 (hash-table-count anvil-offload--pending)))
            (pcase exit
              ('error (error "deterministic send error"))
              ('quit (signal 'quit nil))
              ('throw (throw 'anvil-ownership-send-exit 'throw))))))
      (setq outcome
            (catch 'anvil-ownership-send-exit
              (condition-case _err
                  (progn
                    (anvil-offload
                     '(+ 1 2) :isolated isolated
                     :on-settle (lambda (_future)
                                  (setq callback-called t)))
                    'returned)
                (quit 'quit)
                (error 'error)))))
    (should (eq exit outcome))
    (should pending-seen)
    (should-not callback-called)
    (should (anvil-future-p future))
    (should (eq 'error (anvil-future-status future)))
    (should (eq 'submission-aborted
                (anvil-future--terminal-reason future)))
    (should (= 0 (hash-table-count anvil-offload--pending)))
    (should (process-get proc 'anvil-offload-retiring))
    (should (anvil-offload-ownership-test--owned-p proc))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should-not (process-live-p proc))
    (should-not (anvil-offload-ownership-test--owned-p proc))
    (when (and (not isolated) anvil-offload--pool)
      (should-not (aref anvil-offload--pool 0)))))

(ert-deftest anvil-offload-ownership-pooled-send-error-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit nil 'error)))

(ert-deftest anvil-offload-ownership-isolated-send-error-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit t 'error)))

(ert-deftest anvil-offload-ownership-pooled-send-quit-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit nil 'quit)))

(ert-deftest anvil-offload-ownership-isolated-send-quit-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit t 'quit)))

(ert-deftest anvil-offload-ownership-pooled-send-throw-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit nil 'throw)))

(ert-deftest anvil-offload-ownership-isolated-send-throw-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit t 'throw)))

(ert-deftest anvil-offload-ownership-abort-preserves-original-cleanup-throw ()
  "Fallible custody advice cannot replace the original transport throw."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-abort-cleanup")))
           (anvil-offload--pool (vector proc))
           (real-puthash (symbol-function 'puthash))
           (process-key-calls 0)
           (pending-key-calls 0)
           (timer-calls 0)
           outcome)
      (cl-letf
          (((symbol-function 'anvil-offload--pick-worker)
            (lambda () proc))
           ((symbol-function 'puthash)
            (lambda (key value table)
              (if (processp key)
                  (progn
                    (cl-incf process-key-calls)
                    (throw 'anvil-ownership-custody-fault 'custody-fault))
                (cl-incf pending-key-calls)
                (funcall real-puthash key value table))))
           ((symbol-function 'process-send-string)
            (lambda (_proc _string)
              (throw 'anvil-ownership-original-send 'original-send)))
           ((symbol-function 'run-at-time)
            (lambda (&rest _args)
              (cl-incf timer-calls)
              (throw 'anvil-ownership-cleanup-timer 'cleanup-timer))))
        ;; Prove the installed process-key fault really would throw if the
        ;; abort path called the advised symbol instead of its captured
        ;; primitive.
        (should
         (eq 'custody-fault
             (catch 'anvil-ownership-custody-fault
               (puthash proc t (make-hash-table :test 'eq))
               'returned)))
        (setq process-key-calls 0)
        (setq outcome
              (catch 'anvil-ownership-original-send
                (anvil-offload '(+ 1 2))
                'returned)))
      (should (eq 'original-send outcome))
      (should (= 0 process-key-calls))
      (should (= 1 pending-key-calls))
      (should (= 0 timer-calls))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (process-get proc 'anvil-offload-retiring))
      (should (anvil-offload-ownership-test--owned-p proc))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (process-live-p proc) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should-not (process-live-p proc)))))

(ert-deftest anvil-offload-ownership-future-kill-retires-before-callback ()
  "A kill callback cannot reacquire the exact compatible pooled child."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-future-kill")))
           (anvil-offload--pool (vector proc))
           observed reentrant
           (future
            (make-anvil-future
             :id 41 :process proc :status 'pending
             :on-settle
             (lambda (_future)
               (setq observed
                     (process-get proc 'anvil-offload-retiring))
               (condition-case err
                   (anvil-offload--ensure-slot 0)
                 (error (setq reentrant err)))))))
      (puthash 41 future anvil-offload--pending)
      (cl-letf (((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (anvil-future-kill future 'test-kill))
      (should observed)
      (should reentrant)
      (should (eq proc (aref anvil-offload--pool 0)))
      (should (process-live-p proc))
      (should (anvil-offload-ownership-test--owned-p proc))
      (should-not (gethash 41 anvil-offload--pending)))))

(ert-deftest anvil-offload-ownership-frame-overflow-retires-before-callback ()
  "A frame failure callback cannot reacquire the exact pooled child."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-frame-overflow")))
           (anvil-offload--pool (vector proc))
           (anvil-offload-max-frame-bytes 1)
           observed reentrant
           (future
            (make-anvil-future
             :id 42 :process proc :status 'pending
             :on-settle
             (lambda (_future)
               (setq observed
                     (process-get proc 'anvil-offload-retiring))
               (condition-case err
                   (anvil-offload--ensure-slot 0)
                 (error (setq reentrant err)))))))
      (puthash 42 future anvil-offload--pending)
      (cl-letf (((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (anvil-offload--filter proc "xx"))
      (should observed)
      (should reentrant)
      (should (eq 'error (anvil-future-status future)))
      (should (eq proc (aref anvil-offload--pool 0)))
      (should (process-live-p proc))
      (should (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-settle-snapshots-isolated-child ()
  "A settling callback cannot redirect isolated cleanup to a peer."
  (anvil-offload-ownership-test--with-state
    (let* ((child
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-settle-child"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-settle-peer"))
           (future
            (make-anvil-future
             :id 51 :process child :status 'pending :isolated-p t
             :on-settle
             (lambda (settled)
               (setf (anvil-future--process settled) peer
                     (anvil-future--isolated-p settled) nil)
               (throw 'anvil-ownership-settle-exit 'escaped)))))
      (anvil-offload--track-process-ownership
       child t anvil-offload--isolated-processes)
      (puthash 51 future anvil-offload--pending)
      (should
       (eq 'escaped
           (catch 'anvil-ownership-settle-exit
             (anvil-offload--settle-reply child future 'done 9 'done)
             'returned)))
      (should-not (process-live-p child))
      (should (process-live-p peer))
      (should-not (gethash 51 anvil-offload--pending)))))

(ert-deftest anvil-offload-ownership-start-snapshots-exact-owner ()
  "A start callback cannot redirect abort cleanup to a peer."
  (anvil-offload-ownership-test--with-state
    (let* ((child
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-child"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-peer"))
           (future
            (make-anvil-future
             :id 52 :process child :status 'pending :isolated-p t
             :on-start
             (lambda (started)
               (setf (anvil-future--id started) 999
                     (anvil-future--process started) peer
                     (anvil-future--isolated-p started) nil)
               (throw 'anvil-ownership-start-exit 'escaped)))))
      (anvil-offload--track-process-ownership
       child t anvil-offload--isolated-processes)
      (puthash 52 future anvil-offload--pending)
      (should
       (eq 'escaped
           (catch 'anvil-ownership-start-exit
             (anvil-offload--mark-started future)
             'returned)))
      (should-not (process-live-p child))
      (should (process-live-p peer))
      (should (eq child (anvil-future--process future)))
      (should (= 52 (anvil-future--id future)))
      (should (anvil-future--isolated-p future))
      (should (eq 'killed (anvil-future-status future)))
      (should-not (gethash 52 anvil-offload--pending)))))

(ert-deftest anvil-offload-ownership-coalesced-throw-replays-later-frame ()
  "A tagged first callback exit cannot discard a later coalesced frame."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-coalesced"))
           (first-count 0)
           (second-count 0)
           (timer-calls 0)
           (first
            (make-anvil-future
             :id 61 :process proc :status 'pending
             :on-settle
             (lambda (_future)
               (cl-incf first-count)
               (throw 'anvil-ownership-frame-exit 'escaped))))
           (second
            (make-anvil-future
             :id 62 :process proc :status 'pending
             :on-settle (lambda (_future) (cl-incf second-count))))
           (second-frame
            (anvil-offload-ownership-test--frame '(:id 62 :ok second)))
           (chunk
            (concat
             (anvil-offload-ownership-test--frame '(:id 61 :ok first))
             second-frame)))
      (puthash 61 first anvil-offload--pending)
      (puthash 62 second anvil-offload--pending)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (cl-incf timer-calls)
                   (throw 'anvil-ownership-wrong-timer 'wrong-timer))))
        (should
         (eq 'escaped
             (catch 'anvil-ownership-frame-exit
               (anvil-offload--filter proc chunk)
               'returned))))
      (should (= 0 timer-calls))
      (should (eq 'done (anvil-future-status first)))
      (should (eq 'pending (anvil-future-status second)))
      (should (equal second-frame (process-get proc 'anvil-pending-bytes)))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (eq 'pending (anvil-future-status second))
                    (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'second (anvil-future-value second)))
      (should (= 1 first-count))
      (should (= 1 second-count))
      (should (string-empty-p
               (or (process-get proc 'anvil-pending-bytes) "")))
      (should (= 0 (hash-table-count anvil-offload--pending))))))

(ert-deftest anvil-offload-ownership-sibling-mutation-cannot-redirect-reply ()
  "One callback cannot redirect or terminalize a sibling registration."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-sibling-owner"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-sibling-peer"))
           second
           (first
            (anvil-offload-ownership-test--future
             81 owner
             :on-settle
             (lambda (_future)
               (setf (anvil-future--id second) 999
                     (anvil-future--process second) peer
                     (anvil-future--status second) 'done
                     (anvil-future--isolated-p second) t))))
           (second-count 0))
      (setq second
            (anvil-offload-ownership-test--future
             82 owner :on-settle
             (lambda (_future) (cl-incf second-count))))
      (puthash 81 first anvil-offload--pending)
      (puthash 82 second anvil-offload--pending)
      (anvil-offload--filter
       owner
       (concat
        (anvil-offload-ownership-test--frame '(:id 81 :ok first))
        (anvil-offload-ownership-test--frame '(:id 82 :ok second))))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'second (anvil-future-value second)))
      (should (= 82 (anvil-future--id second)))
      (should (eq owner (anvil-future--process second)))
      (should-not (anvil-future--isolated-p second))
      (should (= 1 second-count))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (process-live-p peer)))))

(ert-deftest anvil-offload-ownership-start-public-kill-uses-registration ()
  "A start callback kill cannot be redirected through visible fields."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-kill-owner"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-kill-peer"))
           future)
      (setq future
            (anvil-offload-ownership-test--future
             83 owner :on-start
             (lambda (started)
               (setf (anvil-future--id started) 999
                     (anvil-future--process started) peer)
               (anvil-future-kill started 'callback-kill))))
      (puthash 83 future anvil-offload--pending)
      (anvil-offload--mark-started future)
      (should (eq 'killed (anvil-future-status future)))
      (should (= 83 (anvil-future--id future)))
      (should (eq owner (anvil-future--process future)))
      (should-not (gethash 83 anvil-offload--pending))
      (should-not (process-live-p owner))
      (should (process-live-p peer)))))

(ert-deftest anvil-offload-ownership-start-status-mutation-is-reconciled ()
  "A contained start callback error cannot forge terminal state."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-status"))
           (future
            (anvil-offload-ownership-test--future
             84 owner :on-start
             (lambda (started)
               (setf (anvil-future--status started) 'done)
               (error "contained start error")))))
      (puthash 84 future anvil-offload--pending)
      (anvil-offload--mark-started future)
      (should (eq 'pending (anvil-future-status future)))
      (should (eq future (gethash 84 anvil-offload--pending)))
      (should (process-live-p owner))
      (anvil-future-kill future 'test-cleanup))))

(ert-deftest anvil-offload-ownership-start-await-cancel-use-registration ()
  "Mutated visible owner/id cannot redirect await or cancel a sibling key."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-await-cancel-owner"))
           (dead-peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-await-cancel-dead"))
           (sibling (anvil-offload-ownership-test--future 94 owner))
           awaited
           future)
      (delete-process dead-peer)
      (setq future
            (anvil-offload-ownership-test--future
             93 owner :on-start
             (lambda (started)
               (setf (anvil-future--id started) 94
                     (anvil-future--process started) dead-peer)
               (setq awaited (anvil-future-await started 0))
               (anvil-future-cancel started))))
      (puthash 93 future anvil-offload--pending)
      (puthash 94 sibling anvil-offload--pending)
      (anvil-offload--mark-started future)
      (should-not awaited)
      (should (eq 'cancelled (anvil-future-status future)))
      (should (= 93 (anvil-future--id future)))
      (should (eq owner (anvil-future--process future)))
      (should-not (gethash 93 anvil-offload--pending))
      (should (eq sibling (gethash 94 anvil-offload--pending)))
      (should (process-live-p owner))
      (anvil-future-kill sibling 'test-cleanup))))

(ert-deftest anvil-offload-ownership-finalize-snapshots-all-siblings ()
  "Dead-process finalization is independent of sibling callback mutation."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-finalize-owner"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-finalize-peer"))
           second
           (first
            (anvil-offload-ownership-test--future
             85 owner :on-settle
             (lambda (_future)
               (setf (anvil-future--id second) 999
                     (anvil-future--process second) peer
                     (anvil-future--status second) 'done)
               (throw 'anvil-ownership-finalize-exit 'escaped)))))
      (setq second (anvil-offload-ownership-test--future 86 owner))
      (puthash 85 first anvil-offload--pending)
      (puthash 86 second anvil-offload--pending)
      (delete-process owner)
      (should
       (eq 'escaped
           (catch 'anvil-ownership-finalize-exit
             (anvil-offload--finalize-dead-process owner "dead owner")
             'returned)))
      (dolist (future (list first second))
        (should (eq 'error (anvil-future-status future)))
        (should (equal "dead owner" (anvil-future-error future))))
      (should (= 86 (anvil-future--id second)))
      (should (eq owner (anvil-future--process second)))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (process-live-p peer)))))

(ert-deftest anvil-offload-ownership-recursive-filter-preserves-partial-frame ()
  "Recursive filter input appends behind the outer wire-order queue."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-recursive-filter"))
           (third-frame
            (anvil-offload-ownership-test--frame '(:id 89 :ok third)))
           (split (/ (length third-frame) 2))
           (third-first (substring third-frame 0 split))
           (third-rest (substring third-frame split))
           (first
            (anvil-offload-ownership-test--future
             87 proc :on-settle
             (lambda (_future)
               (anvil-offload--filter proc third-first))))
           (second (anvil-offload-ownership-test--future 88 proc))
           (third (anvil-offload-ownership-test--future 89 proc)))
      (puthash 87 first anvil-offload--pending)
      (puthash 88 second anvil-offload--pending)
      (puthash 89 third anvil-offload--pending)
      (anvil-offload--filter
       proc
       (concat
        (anvil-offload-ownership-test--frame '(:id 87 :ok first))
        (anvil-offload-ownership-test--frame '(:id 88 :ok second))))
      (should (eq 'done (anvil-future-status first)))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'pending (anvil-future-status third)))
      (should (equal third-first (process-get proc 'anvil-pending-bytes)))
      (anvil-offload--filter proc third-rest)
      (should (eq 'done (anvil-future-status third)))
      (should (eq 'third (anvil-future-value third)))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (string-empty-p
               (or (process-get proc 'anvil-pending-bytes) ""))))))

(ert-deftest anvil-offload-ownership-callback-can-await-later-frame ()
  "A callback can service and await a later registered frame reentrantly."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-callback-await"))
           (second (anvil-offload-ownership-test--future 92 proc))
           awaited
           (first
            (anvil-offload-ownership-test--future
             91 proc :on-settle
             (lambda (_future)
               (run-at-time
                0 nil #'anvil-offload--filter proc
                (anvil-offload-ownership-test--frame
                 '(:id 92 :ok awaited)))
               (setq awaited (anvil-future-await second 0.5))))))
      (puthash 91 first anvil-offload--pending)
      (puthash 92 second anvil-offload--pending)
      (anvil-offload--filter
       proc (anvil-offload-ownership-test--frame '(:id 91 :ok first)))
      (should awaited)
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'awaited (anvil-future-value second)))
      (should (= 0 (hash-table-count anvil-offload--pending))))))

(ert-deftest anvil-offload-ownership-callback-awaits-coalesced-sibling ()
  "A callback can await a sibling already queued in the same input chunk."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-coalesced-await"))
           (second (anvil-offload-ownership-test--future 94 proc))
           awaited
           callback-status
           (first
            (anvil-offload-ownership-test--future
             93 proc :on-settle
             (lambda (_future)
               (setq awaited (anvil-future-await second 0.5)
                     callback-status (anvil-future-status second))))))
      (puthash 93 first anvil-offload--pending)
      (puthash 94 second anvil-offload--pending)
      (anvil-offload--filter
       proc
       (concat
        (anvil-offload-ownership-test--frame '(:id 93 :ok first))
        (anvil-offload-ownership-test--frame '(:id 94 :ok second))))
      (should awaited)
      (should (eq 'done callback-status))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'second (anvil-future-value second)))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (string-empty-p
               (or (process-get proc 'anvil-pending-bytes) ""))))))

(defun anvil-offload-ownership-test--assert-bad-frame-terminal
    (name payload)
  "Assert framed PAYLOAD terminalizes NAME's owned request."
  (let* ((proc (anvil-offload-ownership-test--idle-process name))
         (future (anvil-offload-ownership-test--future 90 proc)))
    (puthash 90 future anvil-offload--pending)
    (anvil-offload--filter
     proc (concat anvil-offload--frame-prefix payload "\n"))
    (should (eq 'error (anvil-future-status future)))
    (should (string-match-p "reply frame" (anvil-future-error future)))
    (should-not (gethash 90 anvil-offload--pending))
    (should-not (process-live-p proc))))

(ert-deftest anvil-offload-ownership-malformed-frames-fail-owned-process ()
  "Malformed reserved frames cannot leave futures pending forever."
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-bad-base64" "%%%not-base64%%%")
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-bad-shape"
     (anvil-offload--frame-encode-payload "(:id 90 :unknown t)"))
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-missing-start-value"
     (anvil-offload--frame-encode-payload "(:id 90 :started)"))
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-missing-checkpoint-value"
     (anvil-offload--frame-encode-payload "(:id 90 :checkpoint)"))
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-duplicate-key"
     (anvil-offload--frame-encode-payload
      "(:id 90 :id 90 :ok duplicate)"))))

(ert-deftest anvil-offload-ownership-unreadable-result-is-readable-error ()
  "An unreadable child value errors cleanly and leaves its pool reusable."
  (anvil-offload-ownership-test--with-state
    (let* ((future
            (anvil-offload
             '(progn
                (setq print-unreadable-function
                      (lambda (&rest _args) "nil"))
                (current-buffer))))
           (owner (anvil-offload--registered-process future)))
      (should (anvil-future-await future 5))
      (should (eq 'error (anvil-future-status future)))
      (should (string-match-p
               "unreadable offload reply" (anvil-future-error future)))
      (should (process-live-p owner))
      (let ((bare
             (anvil-offload
              '(progn
                 (setq print-symbols-bare t)
                 (car (read-positioning-symbols "(wire-position)"))))))
        (should (anvil-future-await bare 5))
        (should (eq 'error (anvil-future-status bare)))
        (should (string-match-p
                 "unreadable offload reply" (anvil-future-error bare))))
      (let ((next (anvil-offload '(+ 1 2))))
        (should (eq owner (anvil-offload--registered-process next)))
        (should (anvil-future-await next 5))
        (should (eq 3 (anvil-future-value next)))))))

(ert-deftest anvil-offload-ownership-normalizes-remote-error-diagnostics ()
  "Hostile child printer state cannot truncate or replace error details."
  (anvil-offload-ownership-test--with-state
    (let ((future
           (anvil-offload
            '(progn
               (setq print-length 1
                     print-level 1
                     print-unreadable-function
                     (lambda (&rest _args) "nil")
                     print-continuous-numbering t
                     print-number-table nil
                     print-symbols-bare t)
               (signal 'error (list "wire boom" '(a b c)))))))
      (should (anvil-future-await future 5))
      (should (eq 'error (anvil-future-status future)))
      (should
       (equal "(error \"wire boom\" (a b c))"
              (anvil-future-error future))))
    (let ((unreadable
           (anvil-offload
            '(signal 'error (list "buffer boom" (current-buffer))))))
      (should (anvil-future-await unreadable 5))
      (should (eq 'error (anvil-future-status unreadable)))
      (should (string-match-p
               "buffer boom.*#<.*buffer"
               (anvil-future-error unreadable))))
    (let ((next (anvil-offload '(+ 1 2))))
      (should (anvil-future-await next 5))
      (should (= 3 (anvil-future-value next))))))

(ert-deftest anvil-offload-ownership-unreadable-checkpoint-aborts-eval ()
  "An unreadable checkpoint cannot terminalize while its worker stays busy."
  (anvil-offload-ownership-test--with-state
    (let* ((future
            (anvil-offload
             '(progn
                (anvil-preempt-checkpoint (current-buffer))
                (while t))))
           (owner (anvil-offload--registered-process future)))
      (should (anvil-future-await future 5))
      (should (eq 'error (anvil-future-status future)))
      (should-not (anvil-future-checkpoint future))
      (let ((next (anvil-offload '(+ 1 2))))
        (should (eq owner (anvil-offload--registered-process next)))
        (should (anvil-future-await next 5))
        (should (= 3 (anvil-future-value next)))))))

(ert-deftest anvil-offload-ownership-circular-results-preserve-topology ()
  "Readable circular and shared values survive the framed child boundary."
  (anvil-offload-ownership-test--with-state
    (let ((circular
           (anvil-offload
            '(let ((value (list 'node)))
               (setcdr value value)
               value))))
      (should (anvil-future-await circular 5))
      (let ((value (anvil-future-value circular)))
        (should (eq value (cdr value)))))
    (let ((shared
           (anvil-offload
            '(let ((value (list 'shared)))
               (list value value)))))
      (should (anvil-future-await shared 5))
      (let ((value (anvil-future-value shared)))
        (should (eq (car value) (cadr value)))))))

(ert-deftest anvil-offload-ownership-normalizes-wire-printer-state ()
  "Caller and pooled-child printer globals cannot corrupt wire objects."
  (anvil-offload-ownership-test--with-state
    (let ((print-length 3)
          (print-level 1)
          (print-circle nil)
          (print-gensym nil)
          (float-output-format "%.1f")
          (future (anvil-offload '(+ 1 2) :isolated t)))
      (should (anvil-future-await future 5))
      (should (= 3 (anvil-future-value future))))
    (let ((poison
           (anvil-offload
            '(progn
               (setq print-length 5
                     print-level 2
                     print-circle nil
                     print-gensym nil
                     print-unreadable-function
                     (lambda (&rest _args) "nil")
                     print-continuous-numbering t
                     print-number-table nil
                     print-symbols-bare t
                     print-quoted nil
                     print-integers-as-characters t
                     float-output-format "%.1f")
               'configured))))
      (should (anvil-future-await poison 5))
      (should (eq 'configured (anvil-future-value poison))))
    (let ((complete
           (anvil-offload
            '(list (number-sequence 1 10) 1.23456789))))
      (should (anvil-future-await complete 5))
      (should
       (equal '((1 2 3 4 5 6 7 8 9 10) 1.23456789)
              (anvil-future-value complete))))
    (let ((property
           (anvil-offload
            '(progn
               (setq print-charset-text-property nil)
               (propertize "x" 'charset 'latin-iso8859-1)))))
      (should (anvil-future-await property 5))
      (should
       (eq 'latin-iso8859-1
           (get-text-property
            0 'charset (anvil-future-value property)))))
    (let* ((name "anvil-ownership-uninterned-wire-symbol")
           (gensym (anvil-offload `(make-symbol ,name)))
           value)
      (should (anvil-future-await gensym 5))
      (setq value (anvil-future-value gensym))
      (should (symbolp value))
      (should (equal name (symbol-name value)))
      (should-not (intern-soft name anvil-offload--canonical-obarray)))
    (let* ((shared (list 'shared-wire-value))
           (form (list 'list (list 'quote shared) (list 'quote shared)))
           (print-continuous-numbering t)
           (print-number-table nil))
      (dotimes (_ 2)
        (let ((future (anvil-offload form)))
          (should (anvil-future-await future 5))
          (let ((value (anvil-future-value future)))
            (should (eq (car value) (cadr value)))))))
    (let* ((text (propertize "x" 'charset 'latin-iso8859-1))
           (print-charset-text-property nil)
           (future (anvil-offload (list 'identity text))))
      (should (anvil-future-await future 5))
      (should
       (eq 'latin-iso8859-1
           (get-text-property
            0 'charset (anvil-future-value future)))))))

(ert-deftest anvil-offload-ownership-unreadable-request-aborts-cleanly ()
  "An unreadable FORM is rejected before transport and strands no custody."
  (anvil-offload-ownership-test--with-state
    (let ((print-unreadable-function (lambda (&rest _args) "nil")))
      (should-error
       (anvil-offload (list 'identity (current-buffer)))))
    (let ((print-symbols-bare t))
      (should-error
       (anvil-offload
        (list 'identity
              (car (read-positioning-symbols "(wire-position)"))))))
    (should (= 0 (hash-table-count anvil-offload--pending)))
    (let ((future (anvil-offload '(+ 1 2))))
      (should (anvil-future-await future 5))
      (should (= 3 (anvil-future-value future))))))

(ert-deftest anvil-offload-ownership-normalizes-wire-reader-state ()
  "Reader shorthands, circle handling, and obarrays stay protocol-local."
  (anvil-offload-ownership-test--with-state
    (let ((poison
           (anvil-offload
            '(progn
               (setq read-circle nil
                     read-symbol-shorthands '(("wire-" . "poison-"))
                     obarray (make-vector 1511 0))
               'configured))))
      (should (anvil-future-await poison 5))
      (should (eq 'configured (anvil-future-value poison))))
    (let* ((circle (list 'wire-value))
           (_ (setcdr circle circle))
           (form (list 'car (list 'quote circle)))
           (future (anvil-offload form)))
      (should (anvil-future-await future 5))
      (should (eq 'wire-value (anvil-future-value future))))
    (let ((read-symbol-shorthands '(("wire-" . "poison-")))
          (obarray (make-vector 1511 0))
          (future (anvil-offload '(quote wire-value))))
      (should (anvil-future-await future 5))
      (should (eq 'wire-value (anvil-future-value future))))))

(ert-deftest anvil-offload-ownership-normalizes-noninteractive-reader-state ()
  "A child request cannot poison stdin handling for later pooled requests."
  (anvil-offload-ownership-test--with-state
    (let ((anvil-offload-max-frame-bytes 512)
          owner)
      (let ((poison
             (anvil-offload
              '(progn
                 (setq read-hide-char ?*)
                 'hide-char-poisoned))))
        (setq owner (anvil-offload--registered-process poison))
        (should (anvil-future-await poison 5))
        (should (eq 'hide-char-poisoned
                    (anvil-future-value poison))))
      (let ((large
             (anvil-offload
              (list 'progn (make-string 1000 ?x) 3))))
        (should (eq owner (anvil-offload--registered-process large)))
        (should (anvil-future-await large 5))
        (should (= 3 (anvil-future-value large))))
      (let ((poison
             (anvil-offload
              '(progn
                 (setq inhibit-interaction t)
                 (setq executing-kbd-macro t)
                 (setq noninteractive nil)
                 (setq minibuffer-local-map 17)
                 (sleep-for 0.2)
                 'interaction-poisoned))))
        (should (eq owner (anvil-offload--registered-process poison)))
        (should (anvil-future-await poison 5))
        (should (eq 'interaction-poisoned
                    (anvil-future-value poison))))
      (let ((next (anvil-offload '(+ 1 2))))
        (should (eq owner (anvil-offload--registered-process next)))
        (should (anvil-future-await next 5))
        (should (= 3 (anvil-future-value next)))))))

(ert-deftest anvil-offload-ownership-retirement-rebuilds-local-survivors ()
  "A cleanup callback cannot erase locally captured survivor custody."
  (anvil-offload-ownership-test--with-state
    (let* ((stubborn
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-local-survivor"))
           (old (vector stubborn))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2))
      (cl-letf (((symbol-function 'kill-process)
                 (lambda (_proc)
                   (setq anvil-offload--retired-pools nil)))
                ((symbol-function 'delete-process) #'ignore))
        (should-error (anvil-offload--ensure-pool-vector)))
      (should (eq old anvil-offload--pool))
      (should (memq old anvil-offload--retired-pools))
      (should anvil-offload--pool-retiring-p)
      (should (process-live-p stubborn)))))

(ert-deftest anvil-offload-ownership-zz-global-state-remains-clean ()
  "Focused and full runs must leave no hidden global ownership state."
  (should-not anvil-offload--pool)
  (should-not anvil-offload--pool-retiring-p)
  (should-not anvil-offload--pool-cleanup-active-p)
  (should-not anvil-offload--submission-active-p)
  (should-not anvil-offload--stop-retiring-p)
  (should-not anvil-offload--retired-pools)
  (dolist (table (anvil-offload--registered-ownership-tables))
    (should (= 0 (hash-table-count table))))
  (when (hash-table-p anvil-offload--pending)
    (should (= 0 (hash-table-count anvil-offload--pending)))))

(provide 'anvil-offload-ownership-test)
;;; anvil-offload-ownership-test.el ends here

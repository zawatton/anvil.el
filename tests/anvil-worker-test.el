;;; anvil-worker-test.el --- Phase 1 lane-split tests for anvil-worker -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises the data-model and dispatch logic introduced by
;; Doc 01 Phase 1 (lane split + :kind keyword).  No real worker
;; daemons are spawned: `anvil-worker--worker-alive-p' is stubbed
;; so dispatch can be verified without `emacs --fg-daemon'
;; processes (which would be expensive on Windows and rely on
;; emacsclient being on PATH).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-worker)

(defmacro anvil-worker-test--with-pool (sizes alive-set &rest body)
  "Run BODY with a fresh pool of SIZES and stubbed liveness.

SIZES is a plist `(:read N :write N :batch N)'.  ALIVE-SET is a
list of worker NAMES to treat as alive; everything else is dead.
Disable health timer + spawning so no real subprocesses start."
  (declare (indent 2))
  `(let* ((anvil-worker-read-pool-size  (or (plist-get ,sizes :read) 0))
          (anvil-worker-write-pool-size (or (plist-get ,sizes :write) 0))
          (anvil-worker-batch-pool-size (or (plist-get ,sizes :batch) 0))
          (anvil-worker--pool nil)
          (anvil-worker--dispatch-index (list :read 0 :write 0 :batch 0))
          (anvil-worker--alive-set ,alive-set))
     (cl-letf (((symbol-function 'anvil-worker--worker-alive-p)
                (lambda (worker)
                  (and worker
                       (member (plist-get worker :name)
                               anvil-worker--alive-set))))
               ((symbol-function 'anvil-worker--spawn-worker)
                (lambda (_worker) nil))
               ((symbol-function 'anvil-worker--log)
                (lambda (&rest _) nil)))
       (anvil-worker--init-pool)
       ,@body)))

;;;; --- pool init ---------------------------------------------------------

(ert-deftest anvil-worker-test-init-pool-shape ()
  "Pool plist holds a vector per lane, sized from the defcustoms."
  (anvil-worker-test--with-pool '(:read 2 :write 1 :batch 0) nil
    (should (= 2 (length (anvil-worker--lane-pool :read))))
    (should (= 1 (length (anvil-worker--lane-pool :write))))
    (should (= 0 (length (anvil-worker--lane-pool :batch))))))

(ert-deftest anvil-worker-test-name-includes-lane ()
  "Worker names embed the lane and a 1-based index."
  (anvil-worker-test--with-pool '(:read 2 :write 1) nil
    (should (equal "anvil-worker-read-1"
                   (plist-get (anvil-worker--worker :read 0) :name)))
    (should (equal "anvil-worker-read-2"
                   (plist-get (anvil-worker--worker :read 1) :name)))
    (should (equal "anvil-worker-write-1"
                   (plist-get (anvil-worker--worker :write 0) :name)))))

(ert-deftest anvil-worker-test-server-file-includes-lane ()
  "Server-file path mirrors the lane-aware worker name."
  (anvil-worker-test--with-pool '(:read 1 :write 1) nil
    (should (string-match-p "anvil-worker-read-1\\'"
                            (plist-get (anvil-worker--worker :read 0)
                                       :server-file)))
    (should (string-match-p "anvil-worker-write-1\\'"
                            (plist-get (anvil-worker--worker :write 0)
                                       :server-file)))))

;;;; --- pick-worker -------------------------------------------------------

(ert-deftest anvil-worker-test-pick-read-only-honours-lane ()
  ":kind :read picks from :read even when :write is also alive."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (let ((w (anvil-worker--pick-worker :read)))
      (should (eq :read (plist-get w :lane)))
      (should (equal "anvil-worker-read-1" (plist-get w :name))))))

(ert-deftest anvil-worker-test-pick-write-only-honours-lane ()
  ":kind :write must not return a :read worker even if :read alive."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (let ((w (anvil-worker--pick-worker :write)))
      (should (eq :write (plist-get w :lane))))))

(ert-deftest anvil-worker-test-pick-auto-prefers-read ()
  ":kind :auto walks lanes in `anvil-worker--lanes' order, read first."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (let ((w (anvil-worker--pick-worker :auto)))
      (should (eq :read (plist-get w :lane))))))

(ert-deftest anvil-worker-test-pick-auto-falls-through ()
  "When :read has no alive workers :auto descends to :write."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-write-1")
    (let ((w (anvil-worker--pick-worker :auto)))
      (should (eq :write (plist-get w :lane))))))

(ert-deftest anvil-worker-test-pick-round-robins-within-lane ()
  "Successive picks within a lane advance the round-robin cursor."
  (anvil-worker-test--with-pool
      '(:read 2)
      '("anvil-worker-read-1" "anvil-worker-read-2")
    (let ((first  (anvil-worker--pick-worker :read))
          (second (anvil-worker--pick-worker :read)))
      (should-not (equal (plist-get first  :name)
                         (plist-get second :name))))))

(ert-deftest anvil-worker-test-pick-skips-busy ()
  "A worker marked busy is skipped over while a non-busy peer exists."
  (anvil-worker-test--with-pool
      '(:read 2)
      '("anvil-worker-read-1" "anvil-worker-read-2")
    (plist-put (anvil-worker--worker :read 0) :busy t)
    (let ((w (anvil-worker--pick-worker :read)))
      (should (equal "anvil-worker-read-2" (plist-get w :name))))))

(ert-deftest anvil-worker-test-pick-busy-fallback ()
  "If every alive worker is busy, dispatch returns one anyway (fallback)."
  (anvil-worker-test--with-pool
      '(:read 2)
      '("anvil-worker-read-1" "anvil-worker-read-2")
    (plist-put (anvil-worker--worker :read 0) :busy t)
    (plist-put (anvil-worker--worker :read 1) :busy t)
    (let ((w (anvil-worker--pick-worker :read)))
      (should w)
      (should (eq :read (plist-get w :lane))))))

;;;; --- arg parsing -------------------------------------------------------

(ert-deftest anvil-worker-test-parse-call-args-empty ()
  "No args ⇒ :auto with default timeout."
  (let ((p (anvil-worker--parse-call-args nil)))
    (should (eq :auto (plist-get p :kind)))
    (should (null (plist-get p :timeout)))))

(ert-deftest anvil-worker-test-parse-call-args-positional-timeout ()
  "Legacy `(EXPR TIMEOUT)' form is accepted as :auto + that timeout."
  (let ((p (anvil-worker--parse-call-args '(120))))
    (should (eq :auto (plist-get p :kind)))
    (should (= 120 (plist-get p :timeout)))))

(ert-deftest anvil-worker-test-parse-call-args-positional-nil-timeout ()
  "A single nil argument is treated as the legacy form, not a keyword."
  (let ((p (anvil-worker--parse-call-args '(nil))))
    (should (eq :auto (plist-get p :kind)))
    (should (null  (plist-get p :timeout)))))

(ert-deftest anvil-worker-test-parse-call-args-keyword-form ()
  "Keyword form binds :kind and :timeout."
  (let ((p (anvil-worker--parse-call-args '(:kind :write :timeout 30))))
    (should (eq :write (plist-get p :kind)))
    (should (= 30     (plist-get p :timeout)))))

(ert-deftest anvil-worker-test-parse-call-args-rejects-unknown ()
  "Unknown keyword raises so typos surface immediately."
  (should-error (anvil-worker--parse-call-args '(:kind :read :nope 1))))

;;;; --- back-compat alias -------------------------------------------------

(ert-deftest anvil-worker-test-pool-size-alias ()
  "Old `anvil-worker-pool-size' setq propagates to read-pool-size."
  (let ((anvil-worker-read-pool-size 99))
    (should (= 99 anvil-worker-pool-size))
    (setq anvil-worker-pool-size 7)
    (should (= 7 anvil-worker-read-pool-size))))

;;; anvil-worker-test.el ends here

;;; anvil-server-ert-reuse-test.el --- ERT for with-server reuse -*- lexical-binding: t; -*-

;;; Commentary:

;; `anvil-server-ert-with-server' used to call `anvil-server-start'
;; unconditionally, so the first ERT of any run errored with "MCP
;; server is already running" whenever a live server was up — making
;; the suites impossible to run from a session that actually uses the
;; server.  The macro now consults `anvil-server-running-p': a running
;; server is reused and left running; a cold start is torn down inside
;; `unwind-protect' as before.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil)
(require 'anvil-server)
(require 'anvil-server-commands)
(require 'anvil-server-ert)

(defun anvil-server-ert-reuse-test--sentinel-tool ()
  "Sentinel handler used to advertise a tools capability during tests."
  "ok")

(defun anvil-server-ert-reuse-test--sentinel-resource ()
  "Sentinel handler used to advertise a resources capability during tests."
  "ok")

(defmacro anvil-server-ert-reuse-test--with-clean-server (&rest body)
  "Run BODY with a guaranteed-stopped server before and after.
Registers a sentinel tool/resource so the :tools/:resources initialize
asserts succeed in bare environments where no other module has
registered anything."
  (declare (indent 0) (debug t))
  `(progn
     (anvil-server-register-tool
      #'anvil-server-ert-reuse-test--sentinel-tool
      :id "ert-reuse-test-sentinel"
      :description "Sentinel tool for ERT setup"
      :server-id anvil-server-ert-server-id)
     (anvil-server-register-resource
      "ert-reuse-test://sentinel"
      #'anvil-server-ert-reuse-test--sentinel-resource
      :name "Sentinel"
      :description "Sentinel resource for ERT setup"
      :server-id anvil-server-ert-server-id)
     (unwind-protect
         (progn
           (when (anvil-server-running-p) (anvil-server-stop))
           ,@body)
       (when (anvil-server-running-p) (anvil-server-stop)))))

(ert-deftest anvil-server-ert-reuse-test-reuses-running-server ()
  "Reuse path: pre-started server is reused; start/stop are not called."
  (anvil-server-ert-reuse-test--with-clean-server
    (anvil-server-start)
    (should (anvil-server-running-p))
    (let ((start-calls 0)
          (stop-calls 0))
      (cl-letf
          (((symbol-function 'anvil-server-start)
            (lambda (&rest _) (cl-incf start-calls)))
           ((symbol-function 'anvil-server-stop)
            (lambda (&rest _) (cl-incf stop-calls))))
        (anvil-server-ert-with-server :tools t :resources t
          (should (anvil-server-running-p))))
      (should (= 0 start-calls))
      (should (= 0 stop-calls)))
    (should (anvil-server-running-p))))

(ert-deftest anvil-server-ert-reuse-test-cold-path-starts-and-stops ()
  "Cold path: no server running; macro starts then stops it."
  (anvil-server-ert-reuse-test--with-clean-server
    (should-not (anvil-server-running-p))
    (let ((start-calls 0)
          (stop-calls 0)
          (real-start (symbol-function 'anvil-server-start))
          (real-stop (symbol-function 'anvil-server-stop)))
      (cl-letf
          (((symbol-function 'anvil-server-start)
            (lambda (&rest args)
              (cl-incf start-calls)
              (apply real-start args)))
           ((symbol-function 'anvil-server-stop)
            (lambda (&rest args)
              (cl-incf stop-calls)
              (apply real-stop args))))
        (anvil-server-ert-with-server :tools t :resources t
          (should (anvil-server-running-p))))
      (should (= 1 start-calls))
      (should (= 1 stop-calls)))
    (should-not (anvil-server-running-p))))

(ert-deftest anvil-server-ert-reuse-test-cold-path-signal-safety ()
  "Cold path body signals: cleanup still stops the server."
  (anvil-server-ert-reuse-test--with-clean-server
    (should-not (anvil-server-running-p))
    (let ((signalled nil))
      (condition-case _err
          (anvil-server-ert-with-server :tools t :resources t
            (setq signalled t)
            (error "boom"))
        (error nil))
      (should signalled))
    (should-not (anvil-server-running-p))))

(provide 'anvil-server-ert-reuse-test)
;;; anvil-server-ert-reuse-test.el ends here

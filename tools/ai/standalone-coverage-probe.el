;;; standalone-coverage-probe.el --- probe one module on anvil-standalone  -*- lexical-binding: t; -*-
;; Loaded after tools/anvil-standalone-mcp.el with
;; ANVIL_STANDALONE_TEST_NO_SERVER=1.  The module name comes from
;; ANVIL_COVERAGE_MODULE.  Prints exactly one line starting with
;; "COVERAGE\t" so the driver can parse it.

(defun standalone-coverage--count-tools ()
  (let ((n 0))
    (maphash (lambda (_server tbl) (setq n (+ n (hash-table-count tbl))))
             anvil-server--tools)
    n))

(defun standalone-coverage--fmt (err)
  (replace-regexp-in-string "[\t\n]" " " (format "%S" err)))

(let* ((mod (intern (getenv "ANVIL_COVERAGE_MODULE")))
       (enable (intern (format "%s-enable" mod)))
       (before (standalone-coverage--count-tools))
       (load-status "pass") (load-err "")
       (reg-status "n/a") (reg-err "") (added 0))
  (condition-case err
      (require mod)
    (error (setq load-status "fail" load-err (standalone-coverage--fmt err))))
  (when (equal load-status "pass")
    (if (not (fboundp enable))
        (setq reg-status "no-enable"
              added (- (standalone-coverage--count-tools) before))
      (condition-case err
          (progn (funcall enable)
                 (setq added (- (standalone-coverage--count-tools) before)
                       reg-status (if (> added 0) "pass" "zero")))
        (error (setq reg-status "fail" reg-err (standalone-coverage--fmt err))))))
  (princ (format "COVERAGE\t%s\t%s\t%s\t%s\t%d\t%s\n"
                 mod load-status (substring load-err 0 (min 300 (length load-err)))
                 reg-status added
                 (substring reg-err 0 (min 300 (length reg-err))))))

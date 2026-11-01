;;; standalone-shell-drain-probe.el --- anvil-shell stdout completeness on standalone  -*- lexical-binding: t; -*-
;; Load after tools/anvil-standalone-mcp.el (ANVIL_STANDALONE_TEST_NO_SERVER=1).
;; Prints "DRAIN PASS" when `seq 1 20000' arrives complete, else "DRAIN FAIL ...".
;; The defvar/defuns stand in for names some NeLisp runtimes lack.
(defvar process-coding-system-alist nil)
(unless (fboundp 'process-list) (defun process-list () nil))
(unless (fboundp 'get-buffer-process) (defun get-buffer-process (_buffer) nil))
(require 'anvil-host)
(let* ((res (anvil-shell "seq 1 20000" '(:timeout 10 :max-output 1000000)))
       (out (plist-get res :stdout)))
  (princ (if (and (eql 0 (plist-get res :exit)) (= 108894 (length out)))
             "DRAIN PASS\n"
           (format "DRAIN FAIL exit=%S bytes=%d\n" (plist-get res :exit) (length out)))))

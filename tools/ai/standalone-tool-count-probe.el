;;; standalone-tool-count-probe.el --- print the standalone tool count -*- lexical-binding: t; -*-

;; Load after tools/anvil-standalone-mcp.el with
;; ANVIL_STANDALONE_TEST_NO_SERVER=1.  Prints `TOOLCOUNT N'.

;;; Code:

(princ (format "TOOLCOUNT %d\n"
               (hash-table-count (gethash "emacs-eval" anvil-server--tools))))

;;; standalone-tool-count-probe.el ends here

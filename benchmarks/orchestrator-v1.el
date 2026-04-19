;;; orchestrator-v1.el --- Benchmark harness for anvil-orchestrator v0.3.0 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; License: GPLv3

;;; Commentary:

;; Measures wallclock, cost, and output quality across three scenarios
;; and five dispatch conditions.  Writes a CSV to benchmarks/results/
;; and a summary plist back to the caller.

;; Scenarios:
;;   S1 fibonacci   — straightforward recursion
;;   S2 csv-parser  — edge cases, larger output
;;   S3 topo-sort   — algorithmic + error signaling

;; Conditions:
;;   C1 solo-haiku       : 1 claude-haiku task
;;   C2 serial-haiku     : 3 claude-haiku tasks, sequential dispatch
;;   C3 parallel-haiku   : 3 claude-haiku tasks, parallel dispatch
;;   C4 consensus-3way   : claude-haiku + codex + ollama fan-out (Phase 4)
;;   C5 consensus-judge  : C4 + meta-LLM judge synthesis (Phase 4b)

;; Usage:
;;   (load-file "benchmarks/orchestrator-v1.el")
;;   (anvil-bench-run-all)

;;; Code:

(require 'anvil-orchestrator)
(require 'cl-lib)

(defvar anvil-bench-reps 3
  "Replications per (scenario, condition) cell.")

(defvar anvil-bench-timeout-sec 120
  "Per-task wallclock cap passed to orchestrator-submit.")

(defvar anvil-bench-results-dir
  (let ((here (file-name-directory (or load-file-name buffer-file-name
                                       default-directory))))
    ;; If HERE already ends in benchmarks/, just append results/.
    (if (string-match-p "/benchmarks/\\'" here)
        (expand-file-name "results" here)
      (expand-file-name "benchmarks/results" here)))
  "Directory for CSV / JSON output.")

(defvar anvil-bench-claude-model "claude-haiku-4-5"
  "Pinned claude model.  Haiku for latency/cost baseline.")

(defvar anvil-bench-ollama-model "llama3.2:3b"
  "Pinned ollama model.  Small enough to stay snappy on a GTX 1060.")

;; ---------------------------------------------------------------------------
;; Scenarios

(defvar anvil-bench-scenarios
  '((:id s1-fib
        :prompt
        "Return ONLY this exact elisp source with no commentary, no prose, no markdown fences:

(defun anvil-bench-fib (n)
  (if (<= n 1) n
    (+ (anvil-bench-fib (- n 1)) (anvil-bench-fib (- n 2)))))

The response must be valid elisp that byte-compiles cleanly, nothing else."
        :entry anvil-bench-fib
        :tests ((anvil-bench-fib 0) 0
                (anvil-bench-fib 1) 1
                (anvil-bench-fib 2) 1
                (anvil-bench-fib 5) 5
                (anvil-bench-fib 10) 55))
    (:id s2-csv
        :prompt
        "Return ONLY elisp source code with no commentary, no prose, no markdown fences, implementing:

(defun anvil-bench-csv-parse (text) ...)

Given TEXT, a string of CSV where the first line is the header and subsequent
lines are rows.  Return a list of plists, one per data row, with header cells
as keys (interned with leading colon, e.g. :name).  Support double-quoted
fields where the value may contain commas.  Trim whitespace from cell values.
Ignore trailing blank lines.

The response must be a single top-level `defun' that byte-compiles, nothing else."
        :entry anvil-bench-csv-parse
        :tests ((anvil-bench-csv-parse "name,age\nAlice,30\nBob,25")
                ((:name "Alice" :age "30") (:name "Bob" :age "25"))
                (anvil-bench-csv-parse "a,b\n\"x,y\",z")
                ((:a "x,y" :b "z"))
                (anvil-bench-csv-parse "k,v\n  hello  ,  world  ")
                ((:k "hello" :v "world"))))
    (:id s3-topo
        :prompt
        "Return ONLY elisp source code with no commentary, no prose, no markdown fences, implementing:

(defun anvil-bench-topo-sort (graph) ...)

GRAPH is an alist ((NODE . (DEP ...)) ...) where NODE symbols depend on DEP
symbols.  Return a list of NODE symbols in topological order such that each
NODE appears AFTER all of its dependencies.  If GRAPH has a cycle, signal
`user-error' with message \"cycle\".

The response must be a single top-level `defun' that byte-compiles, nothing else."
        :entry anvil-bench-topo-sort
        :tests ((anvil-bench-topo-sort '((a . (b)) (b . (c)) (c . ())))
                (c b a)
                (anvil-bench-topo-sort '((x . ()) (y . (x)) (z . (y x))))
                (x y z))
        :cycle-test ((a . (b)) (b . (a)))))
  "List of (:id :prompt :entry :tests :cycle-test?) plists.")

;; ---------------------------------------------------------------------------
;; Validation

(defvar anvil-bench-sandbox-buffer "*anvil-bench-sandbox*")

(defun anvil-bench--extract-elisp (summary)
  "Strip common code-fence prefixes and return the core elisp form as a string."
  (let ((s (or summary "")))
    (when (string-match "```\\(?:elisp\\|emacs-lisp\\)?[ \t]*\n" s)
      (setq s (substring s (match-end 0))))
    (when (string-match "\n?```[ \t]*\\'" s)
      (setq s (substring s 0 (match-beginning 0))))
    (string-trim s)))

(defun anvil-bench--compile (code)
  "Byte-compile CODE (elisp source string).  Return t / nil."
  (condition-case _err
      (progn (byte-compile (car (read-from-string code))) t)
    (error nil)))

(defun anvil-bench--load (code)
  "Eval CODE (elisp source string).  Return t / nil."
  (condition-case _err
      (progn (eval (car (read-from-string code)) t) t)
    (error nil)))

(defun anvil-bench--check-tests (scenario)
  "Run (:tests ...) on the scenario's entry.  Return pass-count / total."
  (let* ((tests (plist-get scenario :tests))
         (pairs (cl-loop for (form expected) on tests by #'cddr
                         collect (cons form expected)))
         (pass 0)
         (total (length pairs)))
    (dolist (pair pairs)
      (condition-case _err
          (when (equal (eval (car pair) t) (cdr pair))
            (cl-incf pass))
        (error nil)))
    (when-let ((cycle (plist-get scenario :cycle-test)))
      (cl-incf total)
      (condition-case err
          (progn (funcall (plist-get scenario :entry) cycle)
                 ;; no error signaled — fail
                 nil)
        (user-error
         (when (equal (cadr err) "cycle") (cl-incf pass)))
        (error nil)))
    (cons pass total)))

(defun anvil-bench--validate (scenario summary)
  "Return (:compiles BOOL :loads BOOL :pass N :total N)."
  (let* ((code (anvil-bench--extract-elisp summary))
         (compiles (and (stringp code) (not (string-empty-p code))
                        (anvil-bench--compile code)))
         (loads (and compiles (anvil-bench--load code)))
         (tp (if loads (anvil-bench--check-tests scenario) (cons 0 0))))
    (list :compiles (and compiles t) :loads (and loads t)
          :pass (car tp) :total (cdr tp))))

;; ---------------------------------------------------------------------------
;; Task submission helpers

(defun anvil-bench--task (id) (anvil-orchestrator--task-get id))
(defun anvil-bench--batch-tids (id) (gethash id anvil-orchestrator--batches))

(defun anvil-bench--wait-one (task-id)
  "Block until TASK-ID reaches a terminal state.  Return the task plist."
  (let ((deadline (+ (float-time) (* 2 anvil-bench-timeout-sec))))
    (while (and (< (float-time) deadline)
                (memq (plist-get (anvil-bench--task task-id) :status)
                      '(queued running)))
      (sleep-for 1))
    (anvil-bench--task task-id)))

(defun anvil-bench--wait-tids (tids)
  "Block until every task id in TIDS reaches terminal state.  Return plists."
  (let ((deadline (+ (float-time) (* 3 anvil-bench-timeout-sec))))
    (while (and (< (float-time) deadline) tids
                (cl-some (lambda (tid)
                           (memq (plist-get (anvil-bench--task tid) :status)
                                 '(queued running)))
                         tids))
      (sleep-for 1))
    (mapcar #'anvil-bench--task tids)))

(defun anvil-bench--wait-batch (batch-id)
  (anvil-bench--wait-tids (anvil-bench--batch-tids batch-id)))

(defun anvil-bench--cell-row (scenario condition rep task-plists)
  "Compose one CSV row from TASK-PLISTS for (SCENARIO CONDITION REP)."
  (when (null task-plists)
    (error "anvil-bench--cell-row: empty task-plists for %s/%s/%d"
           (plist-get scenario :id) condition rep))
  (let* ((starts (delq nil (mapcar (lambda (tp) (plist-get tp :started-at)) task-plists)))
         (finishes (delq nil (mapcar (lambda (tp) (plist-get tp :finished-at)) task-plists)))
         (start (if starts (apply #'min starts) (float-time)))
         (finish (if finishes (apply #'max finishes) start))
         (wall-ms (round (* 1000 (- finish start))))
         (serial-sum-ms (apply #'+ (mapcar
                                    (lambda (tp) (or (plist-get tp :elapsed-ms) 0))
                                    task-plists)))
         (costs (delq nil (mapcar (lambda (tp) (plist-get tp :cost-usd)) task-plists)))
         (cost (apply #'+ costs))
         ;; For validation, pick the first non-empty summary (or join for consensus).
         (summary (cl-some (lambda (tp) (let ((s (plist-get tp :summary)))
                                          (unless (string-empty-p (or s "")) s)))
                           task-plists))
         (val (anvil-bench--validate scenario summary))
         (providers (mapconcat (lambda (tp) (symbol-name (plist-get tp :provider)))
                               task-plists "|")))
    (list :scenario (plist-get scenario :id)
          :condition condition
          :rep rep
          :wall-ms wall-ms
          :serial-sum-ms serial-sum-ms
          :cost-usd cost
          :providers providers
          :task-count (length task-plists)
          :compiles (plist-get val :compiles)
          :loads (plist-get val :loads)
          :pass (plist-get val :pass)
          :total (plist-get val :total)
          :summary-len (length (or summary "")))))

;; ---------------------------------------------------------------------------
;; Condition runners (return list of task plists)

(defun anvil-bench--run-c1 (scenario)
  (let ((tid (anvil-orchestrator-submit-one
              :provider 'claude
              :model anvil-bench-claude-model
              :prompt (plist-get scenario :prompt)
              :timeout-sec anvil-bench-timeout-sec)))
    (list (anvil-bench--wait-one tid))))

(defun anvil-bench--run-c2-serial (scenario)
  "Fire 3 claude tasks sequentially — each waits for previous to finish."
  (cl-loop for i below 3
           collect
           (let ((tid (anvil-orchestrator-submit-one
                       :provider 'claude
                       :model anvil-bench-claude-model
                       :prompt (plist-get scenario :prompt)
                       :timeout-sec anvil-bench-timeout-sec)))
             (anvil-bench--wait-one tid))))

(defun anvil-bench--run-c3-parallel (scenario)
  "Fire 3 claude tasks concurrently via a batch submit."
  (let* ((prompt (plist-get scenario :prompt))
         (tasks (cl-loop for i below 3
                         collect
                         `(:name ,(format "p%d" i)
                           :provider claude
                           :model ,anvil-bench-claude-model
                           :prompt ,prompt
                           :timeout-sec ,anvil-bench-timeout-sec)))
         (batch-id (anvil-orchestrator-submit tasks)))
    (anvil-bench--wait-batch batch-id)))

(defun anvil-bench--run-c4-consensus (scenario)
  "3-way fan-out: claude + codex + ollama.  Return (cid . task-plists)."
  (let* ((result (anvil-orchestrator-submit-consensus
                  :prompt (plist-get scenario :prompt)
                  :providers '(claude codex ollama)
                  :timeout-sec anvil-bench-timeout-sec
                  :overrides `((claude . (:model ,anvil-bench-claude-model))
                               (ollama . (:model ,anvil-bench-ollama-model)))))
         (cid (plist-get result :consensus-id))
         (batch-id (plist-get result :batch-id)))
    (anvil-bench--wait-batch batch-id)
    (cons cid (mapcar #'anvil-bench--task (plist-get result :task-ids)))))

(defun anvil-bench--run-c5-judge (scenario)
  "C4 + meta-LLM judge."
  (pcase-let* ((`(,cid . ,fanout) (anvil-bench--run-c4-consensus scenario))
               (judge-res (anvil-orchestrator-judge-consensus
                           cid
                           :judge 'claude
                           :judge-model anvil-bench-claude-model
                           :extra "Respond in at most 5 lines with ONLY the elisp code (single defun), no commentary, no fences."
                           :timeout-sec anvil-bench-timeout-sec
                           :wait t))
               (judge-task (anvil-bench--wait-one (plist-get judge-res :judge-task-id))))
    (append fanout (list judge-task))))

;; ---------------------------------------------------------------------------
;; CSV output

(defun anvil-bench--csv-escape (v)
  (let ((s (format "%s" v)))
    (if (string-match-p "[,\"\n]" s)
        (concat "\"" (replace-regexp-in-string "\"" "\"\"" s) "\"")
      s)))

(defvar anvil-bench-csv-columns
  '(:scenario :condition :rep :wall-ms :serial-sum-ms :cost-usd
              :providers :task-count :compiles :loads :pass :total :summary-len))

(defun anvil-bench--csv-header ()
  (mapconcat (lambda (k) (substring (symbol-name k) 1))
             anvil-bench-csv-columns ","))

(defun anvil-bench--csv-row (row)
  (mapconcat (lambda (k) (anvil-bench--csv-escape (plist-get row k)))
             anvil-bench-csv-columns ","))

(defun anvil-bench--write-csv (rows path)
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert (anvil-bench--csv-header) "\n")
    (dolist (r rows) (insert (anvil-bench--csv-row r) "\n"))))

;; ---------------------------------------------------------------------------
;; Main driver

(defun anvil-bench-run-cell (scenario condition rep)
  "Run one cell.  Return the row plist."
  (let ((tasks (pcase condition
                 ('c1 (anvil-bench--run-c1 scenario))
                 ('c2 (anvil-bench--run-c2-serial scenario))
                 ('c3 (anvil-bench--run-c3-parallel scenario))
                 ('c4 (cdr (anvil-bench--run-c4-consensus scenario)))
                 ('c5 (anvil-bench--run-c5-judge scenario))
                 (_ (user-error "Unknown condition %s" condition)))))
    (anvil-bench--cell-row scenario condition rep tasks)))

(defun anvil-bench-run-scenario (scenario &optional conditions)
  "Run SCENARIO across CONDITIONS × reps.  Return list of rows."
  (let ((conds (or conditions '(c1 c2 c3 c4 c5)))
        rows)
    (dolist (c conds)
      (dotimes (rep anvil-bench-reps)
        (let ((t0 (float-time)))
          (message "[bench] %s %s rep=%d starting"
                   (plist-get scenario :id) c (1+ rep))
          (push (anvil-bench-run-cell scenario c rep) rows)
          (message "[bench] %s %s rep=%d done in %.1fs"
                   (plist-get scenario :id) c (1+ rep)
                   (- (float-time) t0)))))
    (nreverse rows)))

(defun anvil-bench-run-all (&optional scenarios conditions)
  "Run full matrix.  Return all rows and write CSV."
  (let* ((scens (or scenarios anvil-bench-scenarios))
         (conds (or conditions '(c1 c2 c3 c4 c5)))
         (all-rows (cl-loop for s in scens
                            append (anvil-bench-run-scenario s conds)))
         (stamp (format-time-string "%Y%m%d-%H%M%S"))
         (path (expand-file-name (format "bench-%s.csv" stamp)
                                 anvil-bench-results-dir)))
    (anvil-bench--write-csv all-rows path)
    (message "[bench] wrote %s (%d rows)" path (length all-rows))
    (cons path all-rows)))

(defun anvil-bench-codex-ramp (&optional levels reps)
  "Measure codex wallclock at concurrency LEVELS (default '(1 2 3 4 6 8)).
Temporarily raises the global and per-provider codex caps so the
pool actually dispatches requested concurrency; caps are restored
on exit."
  (let* ((lvls (or levels '(1 2 3 4 6 8)))
         (nrep (or reps 2))
         (scenario (car anvil-bench-scenarios))
         (saved-global anvil-orchestrator-concurrency)
         (saved-per (copy-sequence anvil-orchestrator-per-provider-concurrency))
         (max-level (apply #'max lvls))
         rows)
    (setq anvil-orchestrator-concurrency (max saved-global (+ 2 max-level)))
    (let ((cell (assq 'codex anvil-orchestrator-per-provider-concurrency)))
      (if cell (setcdr cell max-level)
        (push (cons 'codex max-level) anvil-orchestrator-per-provider-concurrency)))
    (unwind-protect
        (anvil-bench--codex-ramp-core lvls nrep scenario)
      (setq anvil-orchestrator-concurrency saved-global)
      (setq anvil-orchestrator-per-provider-concurrency saved-per))))

(defun anvil-bench--codex-ramp-core (lvls nrep scenario)
  "Actual ramp loop, called inside an unwind-protect."
  (let (rows)
    (dolist (n lvls)
      (dotimes (rep nrep)
        (message "[bench] codex ramp n=%d rep=%d" n (1+ rep))
        (let* ((tasks (cl-loop for i below n
                               collect
                               `(:name ,(format "r%d-%d" n i)
                                 :provider codex
                                 :prompt ,(plist-get scenario :prompt)
                                 :timeout-sec ,anvil-bench-timeout-sec)))
               (batch-id (anvil-orchestrator-submit tasks))
               (plists (anvil-bench--wait-batch batch-id)))
          (push (list :level n :rep rep
                      :wall-ms (let* ((s (apply #'min (mapcar
                                                       (lambda (tp) (or (plist-get tp :started-at)
                                                                        (float-time)))
                                                       plists)))
                                      (f (apply #'max (mapcar
                                                       (lambda (tp) (or (plist-get tp :finished-at)
                                                                        (float-time)))
                                                       plists))))
                                 (round (* 1000 (- f s))))
                      :serial-sum-ms (apply #'+ (mapcar
                                                 (lambda (tp) (or (plist-get tp :elapsed-ms) 0))
                                                 plists))
                      :failed (cl-count-if (lambda (tp) (eq (plist-get tp :status) 'failed))
                                           plists)
                      :done (cl-count-if (lambda (tp) (eq (plist-get tp :status) 'done))
                                         plists))
                rows))))
    (let* ((stamp (format-time-string "%Y%m%d-%H%M%S"))
           (path (expand-file-name (format "codex-ramp-%s.csv" stamp)
                                   anvil-bench-results-dir)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (insert "level,rep,wall_ms,serial_sum_ms,failed,done\n")
        (dolist (r (nreverse rows))
          (insert (format "%d,%d,%d,%d,%d,%d\n"
                          (plist-get r :level) (plist-get r :rep)
                          (plist-get r :wall-ms) (plist-get r :serial-sum-ms)
                          (plist-get r :failed) (plist-get r :done)))))
      (message "[bench] wrote %s" path)
      path)))

(provide 'anvil-bench-orchestrator-v1)

;;; orchestrator-v1.el ends here

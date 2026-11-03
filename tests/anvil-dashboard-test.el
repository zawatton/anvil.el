;;; anvil-dashboard-test.el --- Tests for anvil-dashboard -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'anvil-dashboard)

;; `(defvar anvil-orchestrator--tasks)' in anvil-dashboard.el (no value form)
;; only suppresses a free-variable byte-compiler warning in THAT file; it
;; does not register the symbol as globally special (a no-value top-level
;; `defvar' never does, by design -- see the Elisp manual).  In real usage
;; anvil-orchestrator.el's own `(defvar anvil-orchestrator--tasks (make-hash-table ...))'
;; (a defvar WITH a value) does the real global registration.  To dynamically
;; shadow the variable from this (separately-compiled, lexical-binding)
;; test file without loading the 217 KB orchestrator module, redeclare it
;; here with an explicit value so `let' below binds it dynamically.
(defvar anvil-orchestrator--tasks nil
  "Test-only stand-in for anvil-orchestrator's task table.")

;;;; --- fixture helpers -----------------------------------------------------

(defun anvil-dashboard-test--temp-dir ()
  (make-temp-file "anvil-dashboard-test-" t))

(defun anvil-dashboard-test--write (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert content))
  path)

(defun anvil-dashboard-test--touch (path &optional seconds-ago)
  "Set PATH's mtime to SECONDS-AGO (default 0) seconds before now."
  (set-file-times path (time-subtract (current-time) (or seconds-ago 0)))
  path)

(defmacro anvil-dashboard-test--with-clean-cache (&rest body)
  "Run BODY with the snapshot cache reset before and after."
  (declare (indent 0))
  `(progn
     (setq anvil-dashboard--cache nil anvil-dashboard--cache-time 0)
     (unwind-protect (progn ,@body)
       (setq anvil-dashboard--cache nil anvil-dashboard--cache-time 0))))

;;;; --- small generic helpers ------------------------------------------------

(ert-deftest anvil-dashboard-test-panel-ok ()
  (let ((p (anvil-dashboard--panel (lambda () (list 1 2 3)))))
    (should (eq t (cdr (assq 'ok p))))
    (should (null (cdr (assq 'error p))))
    (should (equal '(1 2 3) (cdr (assq 'items p))))))

(ert-deftest anvil-dashboard-test-panel-error-does-not-propagate ()
  (let ((p (anvil-dashboard--panel (lambda () (error "boom")) '(fallback))))
    (should (eq :json-false (cdr (assq 'ok p))))
    (should (string-match-p "boom" (cdr (assq 'error p))))
    (should (equal '(fallback) (cdr (assq 'items p))))))

(ert-deftest anvil-dashboard-test-vec-empty-list-is-vector ()
  (let ((v (anvil-dashboard--vec nil)))
    (should (vectorp v))
    (should (= 0 (length v)))))

(ert-deftest anvil-dashboard-test-vec-preserves-elements ()
  (should (equal [1 2 3] (anvil-dashboard--vec '(1 2 3)))))

(ert-deftest anvil-dashboard-test-age-unknown ()
  (should (equal "unknown" (anvil-dashboard--age nil)))
  (should (equal "unknown" (anvil-dashboard--age "not-a-number"))))

(ert-deftest anvil-dashboard-test-age-recent ()
  (should (string-match-p "\\`[0-9]+s ago\\'"
                           (anvil-dashboard--age (- (float-time) 5)))))

(ert-deftest anvil-dashboard-test-shorten-home ()
  (let ((home (expand-file-name "~/")))
    (should (equal "~/foo/bar"
                    (anvil-dashboard--shorten (concat home "foo/bar"))))))

(ert-deftest anvil-dashboard-test-shorten-long-path ()
  (let ((long (concat "/very/deeply/nested/path/that/keeps/going/on/and/on/final/leaf")))
    (should (string-prefix-p ".../" (anvil-dashboard--shorten long)))))

(ert-deftest anvil-dashboard-test-shorten-nil-is-empty-string ()
  (should (equal "" (anvil-dashboard--shorten nil))))

(ert-deftest anvil-dashboard-test-stall-p-matches ()
  (should (anvil-dashboard--stall-p "I'll pause here -- standing by for your go-ahead."))
  (should (anvil-dashboard--stall-p "Let's hold here until you confirm.")))

(ert-deftest anvil-dashboard-test-stall-p-no-match ()
  (should-not (anvil-dashboard--stall-p "Implemented the feature and ran the tests; all green.")))

(ert-deftest anvil-dashboard-test-last-assistant-text-basic ()
  (let ((tail (concat
               "{\"type\":\"user\",\"message\":{\"content\":\"hi\"}}\n"
               "{\"type\":\"assistant\",\"message\":{\"content\":"
               "[{\"type\":\"text\",\"text\":\"done here\"}]}}\n")))
    (should (equal "done here" (anvil-dashboard--last-assistant-text tail)))))

(ert-deftest anvil-dashboard-test-last-assistant-text-tool-only-does-not-fall-back ()
  (let ((tail (concat
               "{\"type\":\"assistant\",\"message\":{\"content\":"
               "[{\"type\":\"text\",\"text\":\"earlier turn\"}]}}\n"
               "{\"type\":\"assistant\",\"message\":{\"content\":"
               "[{\"type\":\"tool_use\",\"name\":\"Bash\"}]}}\n")))
    (should (equal "" (anvil-dashboard--last-assistant-text tail)))))

;;;; --- Claude Code sessions -------------------------------------------------

(ert-deftest anvil-dashboard-test-claude-sessions ()
  (let ((home (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-dashboard--home)
                   (lambda (&rest parts)
                     (apply #'expand-file-name (mapconcat #'identity parts "/") (list home)))))
          (let* ((proj (expand-file-name ".claude/projects/-fake-proj" home))
                 (active (expand-file-name "active.jsonl" proj))
                 (old (expand-file-name "old.jsonl" proj))
                 (stalled (expand-file-name "stalled.jsonl" proj)))
            (anvil-dashboard-test--write
             active "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"working\"}]}}\n")
            (anvil-dashboard-test--touch active 5)
            (anvil-dashboard-test--write
             old "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"finished long ago\"}]}}\n")
            (anvil-dashboard-test--touch old 1800)
            (anvil-dashboard-test--write
             stalled "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"standing by for input\"}]}}\n")
            (anvil-dashboard-test--touch stalled 5)
            (let ((rows (anvil-dashboard--claude-sessions)))
              (should (= 3 (length rows)))
              (should (equal "running"
                             (cdr (assq 'status (cl-find-if
                                                  (lambda (r) (equal (cdr (assq 'name r)) "active"))
                                                  rows)))))
              (should (equal "stopped"
                             (cdr (assq 'status (cl-find-if
                                                  (lambda (r) (equal (cdr (assq 'name r)) "old"))
                                                  rows)))))
              (should (equal "stale"
                             (cdr (assq 'status (cl-find-if
                                                  (lambda (r) (equal (cdr (assq 'name r)) "stalled"))
                                                  rows)))))
              (should (cl-every (lambda (r) (equal (cdr (assq 'kind r)) "Claude session")) rows)))))
      (delete-directory home t))))

;;;; --- Claude Code subagents -------------------------------------------------

(ert-deftest anvil-dashboard-test-claude-subagents ()
  (let ((root (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-dashboard--claude-task-roots) (lambda () (list root))))
          (let* ((tasks-dir (expand-file-name "proj1/sess1/tasks" root))
                 (jsonl (expand-file-name "agentA.jsonl" tasks-dir))
                 (link (expand-file-name "agentA.output" tasks-dir)))
            (make-directory tasks-dir t)
            (anvil-dashboard-test--write
             jsonl "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"standing by\"}]}}\n")
            (anvil-dashboard-test--touch jsonl 5)
            (make-symbolic-link jsonl link)
            (let ((rows (anvil-dashboard--claude-subagents)))
              (should (= 1 (length rows)))
              (should (equal "Claude subagent" (cdr (assq 'kind (car rows)))))
              (should (equal "stale" (cdr (assq 'status (car rows))))))))
      (delete-directory root t))))

;;;; --- Codex sessions ---------------------------------------------------------

(ert-deftest anvil-dashboard-test-codex-sessions ()
  (let ((home (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-dashboard--home)
                   (lambda (&rest parts)
                     (apply #'expand-file-name (mapconcat #'identity parts "/") (list home)))))
          (let* ((day (format-time-string "%Y/%m/%d" nil))
                 (f (expand-file-name (format ".codex/sessions/%s/rollout.jsonl" day) home)))
            (anvil-dashboard-test--write
             f (concat "{\"type\":\"session_meta\",\"payload\":{\"cwd\":\"/x/y\","
                       "\"model\":\"gpt-6-luna\",\"originator\":\"codex_exec\"}}\n"))
            (let ((rows (anvil-dashboard--codex-sessions)))
              (should (= 1 (length rows)))
              (should (equal "Codex exec" (cdr (assq 'kind (car rows)))))
              (should (equal "gpt-6-luna" (cdr (assq 'model (car rows))))))))
      (delete-directory home t))))

;;;; --- dispatch workers: CLI path, files fallback, negative control ---------

(ert-deftest anvil-dashboard-test-dispatch-cli-classification ()
  (cl-letf (((symbol-function 'anvil-dashboard--dispatch-script) (lambda () "/bin/true"))
            ((symbol-function 'anvil-dashboard--call-bounded)
             (lambda (&rest _args)
               (cons 0 (json-encode
                        (list
                         (list (cons 'run "r1") (cons 'run_dir "/nonexistent/r1")
                               (cons 'name "r1") (cons 'model "gpt-6-luna")
                               (cons 'status "done") (cons 'pid 1)
                               (cons 'started_at "2026-01-01T00:00:00Z")
                               (cons 'last_line "all good")))))))
            ((symbol-function 'anvil-dashboard--legacy-run-workers) (lambda () nil)))
    (let ((rows (anvil-dashboard--dispatch-workers)))
      (should (= 1 (length rows)))
      (should (equal "done" (cdr (assq 'status (car rows)))))
      (should (equal "all good" (cdr (assq 'message (car rows))))))))

(ert-deftest anvil-dashboard-test-dispatch-files-fallback-self-heals-running ()
  "The known wrapper bug: meta.json still says \"running\" after the
process exited.  codex.log's trailing EXIT= line is the evidence that
must override it -- this is the additions-#1 self-heal requirement."
  (let ((runs-root (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-dashboard--dispatch-script) (lambda () nil))
                  ((symbol-function 'anvil-dashboard--dispatch-runs-root) (lambda () runs-root))
                  ((symbol-function 'anvil-dashboard--legacy-run-workers) (lambda () nil)))
          (let ((dir (expand-file-name "buggy-run" runs-root)))
            (anvil-dashboard-test--write
             (expand-file-name "meta.json" dir)
             (json-encode (list (cons 'run "buggy-run") (cons 'name "buggy-run")
                                 (cons 'model "gpt-6-luna") (cons 'status "running")
                                 (cons 'pid 999999999) (cons 'workdir "/tmp"))))
            (anvil-dashboard-test--write
             (expand-file-name "codex.log" dir)
             "model: gpt-6-luna\nworkdir: /tmp\n...\nEXIT=0\n")
            (let ((rows (anvil-dashboard--dispatch-workers)))
              (should (= 1 (length rows)))
              (should (equal "done" (cdr (assq 'status (car rows))))))))
      (delete-directory runs-root t))))

(ert-deftest anvil-dashboard-test-dispatch-corrupted-meta-not-running ()
  "Negative control: a corrupted meta.json must never be reported as
a running (or any) worker."
  (let ((runs-root (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-dashboard--dispatch-script) (lambda () nil))
                  ((symbol-function 'anvil-dashboard--dispatch-runs-root) (lambda () runs-root))
                  ((symbol-function 'anvil-dashboard--legacy-run-workers) (lambda () nil)))
          (anvil-dashboard-test--write
           (expand-file-name "corrupt-run/meta.json" runs-root)
           "{not valid json at all")
          (let ((rows (anvil-dashboard--dispatch-workers)))
            (should (= 0 (length rows)))
            (should-not (cl-some (lambda (r) (equal (cdr (assq 'status r)) "running")) rows))))
      (delete-directory runs-root t))))

(ert-deftest anvil-dashboard-test-legacy-run-worker ()
  (let ((home (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-dashboard--home)
                   (lambda (&rest parts)
                     (apply #'expand-file-name (mapconcat #'identity parts "/") (list home)))))
          (let ((dir (expand-file-name ".cache/tmp/mytask-run" home)))
            (anvil-dashboard-test--write (expand-file-name "brief.md" dir) "do the thing")
            (anvil-dashboard-test--write
             (expand-file-name "codex.log" dir)
             "model: gpt-6-luna\nworkdir: /home/x/work\nsome output\nEXIT=1\n")
            (let ((rows (anvil-dashboard--legacy-run-workers)))
              (should (= 1 (length rows)))
              (should (equal "failed" (cdr (assq 'status (car rows)))))
              (should (equal "gpt-6-luna" (cdr (assq 'model (car rows))))))))
      (delete-directory home t))))

;;;; --- orchestrator ------------------------------------------------------------

(ert-deftest anvil-dashboard-test-orchestrator-unavailable ()
  (should-not (hash-table-p anvil-orchestrator--tasks))
  (should-error (anvil-dashboard--orchestrator-workers)))

(ert-deftest anvil-dashboard-test-orchestrator-loaded ()
  (let ((anvil-orchestrator--tasks (make-hash-table :test 'equal)))
    (puthash "t1"
             (list :id "t1" :name "build-x" :model "gpt-6-luna" :status 'running
                   :cwd "/home/x/work" :started-at (float-time))
             anvil-orchestrator--tasks)
    (let ((rows (anvil-dashboard--orchestrator-workers)))
      (should (= 1 (length rows)))
      (should (equal "Orchestrator task" (cdr (assq 'kind (car rows)))))
      (should (equal "running" (cdr (assq 'status (car rows))))))))

;;;; --- combined workers: one broken source must not break the rest ----------

(ert-deftest anvil-dashboard-test-workers-tolerates-one-broken-source ()
  (cl-letf (((symbol-function 'anvil-dashboard--claude-sessions)
             (lambda () (error "simulated failure")))
            ((symbol-function 'anvil-dashboard--claude-subagents) (lambda () nil))
            ((symbol-function 'anvil-dashboard--codex-sessions) (lambda () nil))
            ((symbol-function 'anvil-dashboard--dispatch-workers)
             (lambda () (list '((kind . "Codex exec") (status . "running")))))
            ((symbol-function 'anvil-dashboard--orchestrator-workers) (lambda () (error "no orchestrator"))))
    (let ((rows (anvil-dashboard--workers)))
      (should (= 1 (length rows)))
      (should (equal "Codex exec" (cdr (assq 'kind (car rows))))))))

(defun anvil-dashboard-test--fake-worker (kind status age-seconds name)
  `((kind . ,kind) (model . "") (name . ,name) (workdir . "")
    (status . ,status) (started . nil)
    (last_activity . ,(anvil-dashboard--age (- (float-time) age-seconds)))
    (last_activity_seconds . ,age-seconds)
    (message . "")))

(ert-deftest anvil-dashboard-test-workers-codex-not-starved-by-many-old-claude-sessions ()
  "Regression: a single global cap applied to the append order used
to let ~80 old Claude session/subagent rows (always appended first)
silently push every Codex row out of the list before the cap ever
saw them.  Ranking + a per-kind cap must guarantee at least one
Codex row survives even with 100 old Claude sessions present."
  (cl-letf (((symbol-function 'anvil-dashboard--claude-sessions)
             (lambda ()
               (cl-loop for i from 1 to 100
                        collect (anvil-dashboard-test--fake-worker
                                 "Claude session" "stopped" 90000 (format "old-%d" i)))))
            ((symbol-function 'anvil-dashboard--claude-subagents) (lambda () nil))
            ((symbol-function 'anvil-dashboard--codex-sessions) (lambda () nil))
            ((symbol-function 'anvil-dashboard--dispatch-workers)
             (lambda () (list (anvil-dashboard-test--fake-worker "Codex exec" "done" 300 "luna-run"))))
            ((symbol-function 'anvil-dashboard--orchestrator-workers) (lambda () (error "unavailable"))))
    (let ((rows (anvil-dashboard--workers)))
      (should (cl-some (lambda (r) (equal (cdr (assq 'kind r)) "Codex exec")) rows))
      ;; per-kind cap: 100 old Claude sessions must not all survive
      (should (= anvil-dashboard--per-kind-cap
                 (cl-count-if (lambda (r) (equal (cdr (assq 'kind r)) "Claude session")) rows))))))

(ert-deftest anvil-dashboard-test-workers-ranking-order ()
  "Running first, then stale/failed, then done/stopped; most-recent
activity first within a tier."
  (cl-letf (((symbol-function 'anvil-dashboard--claude-sessions)
             (lambda () (list (anvil-dashboard-test--fake-worker "Claude session" "stopped" 100 "old")
                              (anvil-dashboard-test--fake-worker "Claude session" "running" 5 "fresh"))))
            ((symbol-function 'anvil-dashboard--claude-subagents) (lambda () nil))
            ((symbol-function 'anvil-dashboard--codex-sessions) (lambda () nil))
            ((symbol-function 'anvil-dashboard--dispatch-workers)
             (lambda () (list (anvil-dashboard-test--fake-worker "Codex exec" "failed" 20 "broke"))))
            ((symbol-function 'anvil-dashboard--orchestrator-workers) (lambda () (error "unavailable"))))
    (let ((rows (anvil-dashboard--workers)))
      (should (equal '("fresh" "broke" "old") (mapcar (lambda (r) (cdr (assq 'name r))) rows))))))

;;;; --- NeLisp health -----------------------------------------------------------

(ert-deftest anvil-dashboard-test-nelisp-unconfigured ()
  (let ((anvil-dashboard-notes-root nil))
    (should-error (anvil-dashboard--nelisp))))

(ert-deftest anvil-dashboard-test-nelisp-gates-and-status ()
  (let ((notes (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (let ((anvil-dashboard-notes-root notes))
          (anvil-dashboard-test--write
           (expand-file-name "dev/nelisp/target/gates/gate-a.json" notes)
           "{\"schema\":\"nelisp-gate/1\",\"name\":\"gate-a\",\"status\":\"pass\"}")
          (anvil-dashboard-test--write
           (expand-file-name "dev/nelisp/target/gates/gate-b.json" notes)
           "{\"schema\":\"nelisp-gate/1\",\"name\":\"gate-b\",\"status\":\"fail\"}")
          (anvil-dashboard-test--write
           (expand-file-name "dev/nelisp/target/ai/STATUS.json" notes)
           "{\"repo\":{\"branch\":\"main\",\"head\":\"abc123\",\"modified_tracked\":2,\"untracked\":3,\"other_worktrees\":4}}")
          (let* ((res (anvil-dashboard--nelisp))
                 (gates (append (cdr (assq 'gates res)) nil))
                 (status (cdr (assq 'status res))))
            (should (= 2 (length gates)))
            (should (= 1 (cl-count-if (lambda (g) (equal (cdr (assq 'status g)) "fail")) gates)))
            (should (equal "main" (cdr (assq 'branch status))))
            (should (= 4 (cdr (assq 'other_worktrees status))))))
      (delete-directory notes t))))

;;;; --- worklog --------------------------------------------------------------

(ert-deftest anvil-dashboard-test-worklog-not-loaded-degrades ()
  (unless (fboundp 'anvil-worklog-list)
    (should-error (anvil-dashboard--worklog))
    (should (= 0 (anvil-dashboard--worklog-today-count)))))

(ert-deftest anvil-dashboard-test-worklog-available ()
  (let ((today (format-time-string "%Y-%m-%d")))
    (cl-letf (((symbol-function 'anvil-worklog-list)
               (lambda (&rest _args)
                 (list (list :date today :title "did a thing" :machine "debian")
                       (list :date "2020-01-01" :title "old entry" :machine "debian")))))
      (should (= 2 (length (append (anvil-dashboard--worklog) nil))))
      (should (= 1 (anvil-dashboard--worklog-today-count))))))

;;;; --- handoff --------------------------------------------------------------

(ert-deftest anvil-dashboard-test-handoff ()
  (let ((notes (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (let ((anvil-dashboard-notes-root notes))
          (let ((older (anvil-dashboard-test--write (expand-file-name "handoff/older.md" notes) "x"))
                (newer (anvil-dashboard-test--write (expand-file-name "handoff/newer.md" notes) "y")))
            (anvil-dashboard-test--touch older 500)
            (anvil-dashboard-test--touch newer 5)
            (let ((items (append (anvil-dashboard--handoff) nil)))
              (should (= 2 (length items)))
              (should (equal "newer.md" (cdr (assq 'name (car items))))))))
      (delete-directory notes t))))

;;;; --- daemon health ----------------------------------------------------------

(ert-deftest anvil-dashboard-test-daemon-hung-emacsclients ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/bin/ps"))
            ((symbol-function 'call-process)
             (lambda (_prog _infile _dest _display &rest _args)
               (insert "111 5 /bin/bash\n")
               (insert "222 90 emacsclient -e (foo)\n")
               (insert "333 200 emacsclient --eval bar\n")
               0)))
    (let ((res (anvil-dashboard--daemon)))
      (should (= 2 (cdr (assq 'count res))))
      (should (= 200 (cdr (assq 'oldest_age_seconds res))))
      (should (eq t (cdr (assq 'red res)))))))

(ert-deftest anvil-dashboard-test-daemon-clean-is-not-red ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/bin/ps"))
            ((symbol-function 'call-process)
             (lambda (_prog _infile _dest _display &rest _args)
               (insert "111 5 /bin/bash\n222 3 emacsclient -e (foo)\n")
               0)))
    (let ((res (anvil-dashboard--daemon)))
      (should (= 0 (cdr (assq 'count res))))
      (should (eq :json-false (cdr (assq 'red res)))))))

;;;; --- project-progress ledgers ----------------------------------------------

(defun anvil-dashboard-test--real-ledger-json (ledger project percent criteria-defined)
  "Build a JSON string matching bin/progress-meter.sh --json's real
schema: {schema_version,ledger,project,generated_at,overall:{...},
stages:[{...,criteria:[...]}]}."
  (json-encode
   (list (cons 'schema_version 1)
         (cons 'ledger ledger)
         (cons 'project project)
         (cons 'generated_at "2026-09-27T07:37:53Z")
         (cons 'overall
               (list (cons 'criteria_defined criteria-defined)
                     (cons 'weight_total criteria-defined)
                     (cons 'weight_pass (round (* criteria-defined (/ percent 100.0))))
                     (cons 'percent percent)
                     (cons 'counts (list (cons 'pass 1) (cons 'fail 1) (cons 'skip 0)
                                          (cons 'pending 0) (cons 'unknown 0)))))
         (cons 'stages
               (list (list (cons 'id "S1") (cons 'title "Stage one")
                            (cons 'criteria_defined criteria-defined)
                            (cons 'weight_total criteria-defined)
                            (cons 'weight_pass 1) (cons 'percent percent)
                            (cons 'counts (list (cons 'pass 1) (cons 'fail 1)))
                            (cons 'criteria
                                  (list (list (cons 'id "S1.1") (cons 'statement "It passes")
                                              (cons 'kind "cmd") (cons 'weight 1)
                                              (cons 'status "PASS") (cons 'detail "exit 0"))
                                        (list (cons 'id "S1.2") (cons 'statement "It should also pass")
                                              (cons 'kind "cmd") (cons 'weight 1)
                                              (cons 'status "FAIL") (cons 'detail "exit 1"))))))))))

(defun anvil-dashboard-test--cache-json ()
  "progress-meter.sh's own per-criterion status CACHE shape: a flat
{id: {status,timestamp}} map, with none of the real ledger's
top-level keys.  Must never be surfaced as a phantom ledger."
  (json-encode (list (cons 'S1.1 (list (cons 'status "PASS") (cons 'timestamp "2026-09-27T07:37:00Z"))))))

(ert-deftest anvil-dashboard-test-progress-none-yet ()
  (let ((notes (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (let ((anvil-dashboard-notes-root notes))
          (should (= 0 (length (anvil-dashboard--progress)))))
      (delete-directory notes t))))

(ert-deftest anvil-dashboard-test-progress-real-schema ()
  (let ((notes (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (let ((anvil-dashboard-notes-root notes)
              (ledger-org (expand-file-name "dev/nelisp/tools/ai/eln-progress.org" notes)))
          (anvil-dashboard-test--write
           (expand-file-name "dev/nelisp/target/progress/eln-progress.json" notes)
           (anvil-dashboard-test--real-ledger-json ledger-org "NeLisp eln ledger" 48.2 56))
          ;; the sibling per-criterion cache file must not become a
          ;; second, bogus "percent: None" item
          (anvil-dashboard-test--write
           (expand-file-name "dev/nelisp/target/progress/eln-progress.cache.json" notes)
           (anvil-dashboard-test--cache-json))
          ;; another project's ledger, unrelated, must appear too
          (anvil-dashboard-test--write
           (expand-file-name "dev/projx/target/progress/x.json" notes)
           (anvil-dashboard-test--real-ledger-json
            (expand-file-name "dev/projx/x.org" notes) "Project X" 10 2))
          ;; malformed ledger: must be skipped, not surfaced as an error
          (anvil-dashboard-test--write
           (expand-file-name "dev/projy/target/progress/bad.json" notes)
           "not json")
          (let ((items (append (anvil-dashboard--progress) nil)))
            (should (= 2 (length items)))
            (let ((eln (cl-find-if (lambda (i) (equal (cdr (assq 'project i)) "NeLisp eln ledger")) items)))
              (should eln)
              (should (= 48.2 (cdr (assq 'percent eln))))
              (should (= 56 (cdr (assq 'criteria_defined eln))))
              (should (= 1 (length (append (cdr (assq 'stages eln)) nil))))
              (let ((failing (append (cdr (assq 'failing_criteria eln)) nil)))
                (should (= 1 (length failing)))
                (should (equal "S1.2" (cdr (assq 'id (car failing)))))
                (should (equal "It should also pass" (cdr (assq 'statement (car failing)))))))))
      (delete-directory notes t))))

(ert-deftest anvil-dashboard-test-progress-dedupe-by-ledger-truename ()
  "Two progress JSON files describing the SAME underlying ledger org
file (e.g. reached through two aliased notes-root paths) must
collapse to a single panel item."
  (let ((notes (anvil-dashboard-test--temp-dir)))
    (unwind-protect
        (let* ((anvil-dashboard-notes-root notes)
               (ledger-org (expand-file-name "dev/nelisp/tools/ai/eln-progress.org" notes)))
          (make-directory (file-name-directory ledger-org) t)
          (with-temp-file ledger-org (insert "* ledger\n"))
          (anvil-dashboard-test--write
           (expand-file-name "target/progress/a.json" notes)
           (anvil-dashboard-test--real-ledger-json ledger-org "Same ledger" 40 5))
          (anvil-dashboard-test--write
           (expand-file-name "dev/otherproj/target/progress/b.json" notes)
           (anvil-dashboard-test--real-ledger-json ledger-org "Same ledger" 40 5))
          (should (= 1 (length (anvil-dashboard--progress)))))
      (delete-directory notes t))))

;;;; --- stats aggregation -------------------------------------------------------

(ert-deftest anvil-dashboard-test-stats-aggregation ()
  (let* ((workers (list '((kind . "Claude session") (status . "running"))
                        '((kind . "Claude subagent") (status . "running"))
                        '((kind . "Codex exec") (status . "running"))
                        '((kind . "Codex exec") (status . "failed"))
                        '((kind . "Claude session") (status . "stale"))))
         (nelisp `((gates . ,(anvil-dashboard--vec
                              (list '((name . "a") (status . "pass"))
                                    '((name . "b") (status . "fail")))))))
         (stats (anvil-dashboard--stats workers nelisp 3 '((count . 1)))))
    (should (= 1 (cdr (assq 'active_claude_sessions stats))))
    (should (= 1 (cdr (assq 'running_subagents stats))))
    (should (= 1 (cdr (assq 'running_codex stats))))
    (should (= 1 (cdr (assq 'stale_workers stats))))
    (should (= 1 (cdr (assq 'failed_24h stats))))
    (should (= 1 (cdr (assq 'nelisp_gates_red stats))))
    (should (= 3 (cdr (assq 'worklog_today stats))))
    (should (= 1 (cdr (assq 'emacsclient_hung stats))))))

;;;; --- full snapshot integration ----------------------------------------------

(ert-deftest anvil-dashboard-test-snapshot-has-all-panels ()
  (anvil-dashboard-test--with-clean-cache
   (let ((notes (anvil-dashboard-test--temp-dir))
         (anvil-dashboard-notes-root nil))
     (unwind-protect
         (progn
           (setq anvil-dashboard-notes-root notes)
           (let ((snap (anvil-dashboard-snapshot)))
             (should (stringp (cdr (assq 'generated_at snap))))
             (let ((panels (cdr (assq 'panels snap))))
               (dolist (name '(stats workers progress nelisp worklog handoff daemon))
                 (let ((panel (cdr (assq name panels))))
                   (should panel)
                   (should (memq (cdr (assq 'ok panel)) '(t :json-false)))
                   (should (assq 'items panel)))))))
       (delete-directory notes t)))))

(ert-deftest anvil-dashboard-test-snapshot-is-cached ()
  (anvil-dashboard-test--with-clean-cache
   (let ((s1 (anvil-dashboard-snapshot))
         (s2 (anvil-dashboard-snapshot)))
     (should (eq s1 s2)))))

;;;; --- HTTP server ------------------------------------------------------------

(defun anvil-dashboard-test--http-get (port path)
  "Open a TCP client to 127.0.0.1:PORT, GET PATH, return a parsed plist.
Plist keys: :status (int), :headers (alist, lower-cased names), :body (str)."
  (let* ((buf "")
         (done nil)
         (proc (make-network-process
                :name "anvil-dashboard-http-test-client"
                :host "127.0.0.1"
                :service port
                :family 'ipv4
                :coding 'binary
                :noquery t
                :filter (lambda (_p s) (setq buf (concat buf s)))
                :sentinel (lambda (p _e)
                            (when (memq (process-status p) '(closed failed exit signal))
                              (setq done t))))))
    (unwind-protect
        (progn
          (process-send-string
           proc (format "GET %s HTTP/1.0\r\nHost: 127.0.0.1\r\n\r\n" path))
          (with-timeout (3 nil)
            (while (not done)
              (accept-process-output proc 0.1))))
      (when (process-live-p proc) (delete-process proc)))
    (if (string-match "\\`HTTP/[0-9.]+ \\([0-9]+\\) " buf)
        (let* ((status (string-to-number (match-string 1 buf)))
               (split (string-match "\r\n\r\n" buf))
               (head (substring buf 0 (or split (length buf))))
               (body (if split (substring buf (+ split 4)) ""))
               (headers
                (let (acc)
                  (dolist (line (cdr (split-string head "\r\n" t)))
                    (when (string-match "\\([^:]+\\):\\s-*\\(.*\\)" line)
                      (push (cons (downcase (match-string 1 line)) (match-string 2 line)) acc)))
                  (nreverse acc))))
          (list :status status :headers headers :body body))
      (list :status -1 :headers nil :body buf))))

(defmacro anvil-dashboard-test--with-server (&rest body)
  "Run BODY with a dashboard server bound to an ephemeral port.
Binds `port' (int)."
  (declare (indent 0))
  `(let* ((info (anvil-dashboard-serve-start :host "127.0.0.1" :port t))
          (port (plist-get info :port)))
     (unwind-protect (progn ,@body)
       (anvil-dashboard-serve-stop))))

(ert-deftest anvil-dashboard-test-serve-root-page ()
  (anvil-dashboard-test--with-clean-cache
   (anvil-dashboard-test--with-server
    (let* ((res (anvil-dashboard-test--http-get port "/"))
           ;; the raw TCP client reads :coding 'binary (raw bytes); decode
           ;; before matching multibyte (Japanese) patterns against it.
           (body (decode-coding-string (plist-get res :body) 'utf-8)))
      (should (= 200 (plist-get res :status)))
      (should (string-match-p "text/html" (cdr (assoc "content-type" (plist-get res :headers)))))
      (should (string-match-p "Anvil dashboard" body))
      (should (string-match-p "Anvil ダッシュボード" body))))))

(ert-deftest anvil-dashboard-test-serve-snapshot-json ()
  (anvil-dashboard-test--with-clean-cache
   (anvil-dashboard-test--with-server
    (let ((res (anvil-dashboard-test--http-get port "/api/snapshot")))
      (should (= 200 (plist-get res :status)))
      (should (string-match-p "application/json"
                              (cdr (assoc "content-type" (plist-get res :headers)))))
      (let ((parsed (json-read-from-string (plist-get res :body))))
        (should (assq 'panels parsed)))))))

(ert-deftest anvil-dashboard-test-serve-404 ()
  (anvil-dashboard-test--with-clean-cache
   (anvil-dashboard-test--with-server
    (let ((res (anvil-dashboard-test--http-get port "/nope")))
      (should (= 404 (plist-get res :status)))))))

(ert-deftest anvil-dashboard-test-serve-start-stop-idempotent ()
  (let* ((info (anvil-dashboard-serve-start :host "127.0.0.1" :port t)))
    (should (plist-get info :port))
    (should-error (anvil-dashboard-serve-start :host "127.0.0.1" :port t))
    (should (integerp (anvil-dashboard-serve-stop)))
    (should (null (anvil-dashboard-serve-stop)))))

(ert-deftest anvil-dashboard-test-tool-serve-start-result-is-json-safe ()
  "The MCP start tool must not return the (unencodable) process object."
  (unwind-protect
      (let ((r (anvil-dashboard--tool-serve-start t)))
        (should (equal "127.0.0.1" (plist-get r :host)))
        (should (integerp (plist-get r :port)))
        (should-not (plist-member r :process))
        (should (stringp (json-encode r))))
    (anvil-dashboard-serve-stop)))

(provide 'anvil-dashboard-test)
;;; anvil-dashboard-test.el ends here

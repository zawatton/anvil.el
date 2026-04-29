;;; anvil-claude-watchdog-test.el --- ERT tests for anvil-claude-watchdog -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure unit tests for the heuristic.  No real /proc reads — all
;; samples are constructed plists.  This keeps the suite fast and
;; portable across machines that may or may not currently have a
;; running claude process.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (file-name-directory
             (or load-file-name buffer-file-name default-directory))))
  (add-to-list 'load-path (expand-file-name ".." root)))

(require 'anvil-claude-watchdog)

(cl-defun anvil-claude-watchdog-test--sample
    (&key sampled-at rchar wchar state jsonl-mtime stagnant-since)
  "Construct a test sample plist with sensible defaults."
  (list :pid 12345
        :sampled-at (or sampled-at 1000.0)
        :rchar (or rchar 0)
        :wchar (or wchar 0)
        :syscw 0
        :state (or state "S")
        :cwd "/home/x/proj"
        :jsonl-path "/tmp/x.jsonl"
        :jsonl-mtime jsonl-mtime
        :wchar-stagnant-since stagnant-since))

(ert-deftest anvil-claude-watchdog-test/healthy-no-prev ()
  "First sample is always healthy."
  (let ((now (anvil-claude-watchdog-test--sample)))
    (should (eq :healthy
                (anvil-claude-watchdog--classify
                 :prev nil :now now
                 :stagnation-threshold 60
                 :jsonl-staleness 120)))))

(ert-deftest anvil-claude-watchdog-test/healthy-output-advances ()
  "wchar advancing → healthy regardless of state."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :wchar 100 :state "R"))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1030.0 :wchar 200 :state "R"
                :jsonl-mtime 1029.5)))
    (should (eq :healthy
                (anvil-claude-watchdog--classify
                 :prev prev :now now
                 :stagnation-threshold 60
                 :jsonl-staleness 120)))))

(ert-deftest anvil-claude-watchdog-test/healthy-idle-state-S ()
  "State S (sleeping in epoll) with no rchar arrival → healthy idle."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :rchar 50 :wchar 100 :state "S"))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1030.0 :rchar 50 :wchar 100 :state "S"
                :jsonl-mtime 1029.5)))
    (should (eq :healthy
                (anvil-claude-watchdog--classify
                 :prev prev :now now
                 :stagnation-threshold 60
                 :jsonl-staleness 120)))))

(ert-deftest anvil-claude-watchdog-test/suspicious-short-stagnation ()
  "State R + wchar=0 + recent stagnation < threshold → suspicious."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :rchar 50 :wchar 100 :state "R"
                :stagnant-since 970.0))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1010.0 :rchar 75 :wchar 100 :state "R"
                :stagnant-since 970.0
                :jsonl-mtime 1009.0)))
    (should (eq :suspicious
                (anvil-claude-watchdog--classify
                 :prev prev :now now
                 :stagnation-threshold 60
                 :jsonl-staleness 120)))))

(ert-deftest anvil-claude-watchdog-test/hang-rchar-up-wchar-stale ()
  "State R + wchar static for >60s + rchar increasing → hang.

Mirrors the 2026-04-29 production hang signature."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :rchar 100 :wchar 200 :state "R"
                :stagnant-since 900.0))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1010.0 :rchar 200 :wchar 200 :state "R"
                :stagnant-since 900.0
                :jsonl-mtime 1009.0)))
    (should (eq :hang
                (anvil-claude-watchdog--classify
                 :prev prev :now now
                 :stagnation-threshold 60
                 :jsonl-staleness 120)))))

(ert-deftest anvil-claude-watchdog-test/hang-jsonl-stale-no-rchar ()
  "State R + wchar static + jsonl mtime old → hang even without rchar."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :rchar 100 :wchar 200 :state "R"
                :stagnant-since 800.0))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1010.0 :rchar 100 :wchar 200 :state "R"
                :stagnant-since 800.0
                ;; mtime 200s old → > 120s staleness
                :jsonl-mtime 810.0)))
    (should (eq :hang
                (anvil-claude-watchdog--classify
                 :prev prev :now now
                 :stagnation-threshold 60
                 :jsonl-staleness 120)))))

(ert-deftest anvil-claude-watchdog-test/healthy-state-not-R ()
  "Even with stagnant wchar, State S means healthy idle (no CPU spent)."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :rchar 100 :wchar 200 :state "S"
                :stagnant-since 800.0))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1010.0 :rchar 200 :wchar 200 :state "S"
                :stagnant-since 800.0
                :jsonl-mtime 810.0)))
    (should (eq :healthy
                (anvil-claude-watchdog--classify
                 :prev prev :now now
                 :stagnation-threshold 60
                 :jsonl-staleness 120)))))

(ert-deftest anvil-claude-watchdog-test/update-stagnation-resets-on-output ()
  "When wchar advances, :wchar-stagnant-since resets to nil."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :wchar 100 :stagnant-since 950.0))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1010.0 :wchar 200)))
    (anvil-claude-watchdog--update-stagnation prev now)
    (should (null (plist-get now :wchar-stagnant-since)))))

(ert-deftest anvil-claude-watchdog-test/update-stagnation-carries-forward ()
  "When wchar stays static, prior :wchar-stagnant-since carries over."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :wchar 100 :stagnant-since 950.0))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1010.0 :wchar 100)))
    (anvil-claude-watchdog--update-stagnation prev now)
    (should (= 950.0 (plist-get now :wchar-stagnant-since)))))

(ert-deftest anvil-claude-watchdog-test/update-stagnation-starts-tracking ()
  "When prev had no stagnation tracker but wchar is static, anchor to prev sample time."
  (let* ((prev (anvil-claude-watchdog-test--sample
                :sampled-at 1000.0 :wchar 100))
         (now  (anvil-claude-watchdog-test--sample
                :sampled-at 1010.0 :wchar 100)))
    (anvil-claude-watchdog--update-stagnation prev now)
    (should (= 1000.0 (plist-get now :wchar-stagnant-since)))))

(ert-deftest anvil-claude-watchdog-test/cwd-to-slug ()
  "Project-dir slug mirrors the leading-dash + slash-replaced format."
  (should (equal "-home-x-proj"
                 (anvil-claude-watchdog--cwd-to-project-slug "/home/x/proj")))
  (should (null (anvil-claude-watchdog--cwd-to-project-slug nil))))

(ert-deftest anvil-claude-watchdog-test/proc-io-parse ()
  "Synthetic /proc-style content parses into the (rchar wchar syscw) tuple."
  (cl-letf (((symbol-function 'anvil-claude-watchdog--read-file)
             (lambda (_p)
               "rchar: 12345\nwchar: 67890\nsyscw: 42\n")))
    (let ((io (anvil-claude-watchdog--proc-io 1)))
      (should (equal '(12345 67890 42) io)))))

(ert-deftest anvil-claude-watchdog-test/proc-state-parse ()
  "/proc/PID/status State line yields just the letter."
  (cl-letf (((symbol-function 'anvil-claude-watchdog--read-file)
             (lambda (_p) "Name: x\nState: R (running)\nThreads: 1\n")))
    (should (equal "R" (anvil-claude-watchdog--proc-state 1)))))

(ert-deftest anvil-claude-watchdog-test/dispatch-runs-hook-on-hang ()
  "On :hang verdict the hook is invoked with the sample plist."
  (let (captured)
    (cl-letf* (((symbol-function 'anvil-claude-watchdog--notify) #'ignore)
               ((symbol-function 'anvil-claude-watchdog--record-event)
                (lambda (_n _v) "k"))
               (anvil-claude-watchdog-on-hang-functions
                (list (lambda (info) (push info captured)))))
      (let ((now (anvil-claude-watchdog-test--sample :state "R")))
        (anvil-claude-watchdog--dispatch now :hang))
      (should (= 1 (length captured)))
      (should (eq :hang (plist-get (car captured) :verdict))))))

(ert-deftest anvil-claude-watchdog-test/dispatch-skips-on-healthy ()
  "Healthy verdicts do not trigger the hook."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-claude-watchdog--notify) #'ignore)
              ((symbol-function 'anvil-claude-watchdog--record-event)
               (lambda (_n _v) (push :should-not-run captured))))
      (let ((anvil-claude-watchdog-on-hang-functions
             (list (lambda (_) (push :hook captured)))))
        (anvil-claude-watchdog--dispatch
         (anvil-claude-watchdog-test--sample) :healthy))
      (should (null captured)))))

(provide 'anvil-claude-watchdog-test)
;;; anvil-claude-watchdog-test.el ends here

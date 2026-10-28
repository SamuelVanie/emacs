;;; smoke-test.el --- Network-free starter checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Run in a fresh Emacs (no presets, API credentials, or live calls needed):
;; emacs -Q --batch -L /path/to/gptel-runner -l /path/to/skill/scripts/smoke-test.el \
;;   -f ert-run-tests-batch-and-exit
;; The temporary workspace is empty and removed after each test.

;;; Code:

(require 'ert)
(require 'gptel-runner)

(load (expand-file-name
       "../assets/intent-preserving-pipeline.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defconst grp-test/brief
  "Purpose: recover interrupted imports. A1: completed items stay completed. C1: preserve CLI.")
(defconst grp-test/plan
  "Plan P1: checkpoint stable item IDs; atomic updates protect A1. Inspect importer.el.")
(defconst grp-test/pass
  "{\"verdict\":\"pass\",\"summary\":\"A1 verified; C1 and D1 preserved\",\"issues\":[]}")
(defconst grp-test/revise
  "{\"verdict\":\"revise\",\"summary\":\"A1 incomplete\",\"issues\":[{\"severity\":\"error\",\"message\":\"I1: completed items were imported twice\"}]}")

(defmacro grp-test/with-run (&rest body)
  "Run BODY with a fake driver and temporary workspace."
  (declare (indent 0))
  `(let ((driver (gptel-runner-fake-driver-create))
         (workspace (make-temp-file "grp-skill-smoke-" t))
         (gptel-runner--runs (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_id run)
                  (when (eq (gptel-runner-run-state run) 'running)
                    (gptel-runner-abort-run run)))
                gptel-runner--runs)
       (delete-directory workspace t))))

(defun grp-test/start (driver workspace)
  "Start the sample on DRIVER in WORKSPACE with an explicit shared brief."
  (gptel-runner-start
   'grp-intent-preserving :driver driver :workspace workspace
   :goal "Add resumable import" :context grp-test/brief :allow-writes t))

(defun grp-test/prompts (driver node-id)
  "Get dispatched prompts for NODE-ID from DRIVER."
  (mapcar #'gptel-runner-call-prompt
          (seq-filter (lambda (call)
                        (eq (gptel-runner-node-id (gptel-runner-call-node call))
                            node-id))
                      (gptel-runner-fake-driver-starts driver))))

(ert-deftest grp-smoke-intent-decisions-and-revision-reach-consumers ()
  (grp-test/with-run
    (gptel-runner-fake-queue driver 'grp-planner
                             (list :value grp-test/plan :manual t))
    (gptel-runner-fake-queue driver 'grp-implementer
                             '(:value "First change; test A1 fails")
                             '(:value "Fixed I1; test A1 passes"))
    (gptel-runner-fake-queue driver 'grp-reviewer
                             (list :value grp-test/revise)
                             (list :value grp-test/pass))
    (let* ((run (grp-test/start driver workspace))
           (planner (car (gptel-runner-fake-driver-starts driver))))
      ;; Record a human choice before the dependent calls are prepared.
      (gptel-runner-record-decision
       run "D1: use existing SQLite store" "Atomic updates without another service")
      (gptel-runner-fake-release driver planner)
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= 2 (gptel-runner-iteration run 'grp-cycle)))
      (dolist (call (gptel-runner-fake-driver-starts driver))
        (let ((prompt (gptel-runner-call-prompt call)))
          (should (string-match-p "Add resumable import" prompt))
          (should (string-match-p (regexp-quote grp-test/brief) prompt))
          (should (string-match-p (regexp-quote workspace) prompt))))
      (dolist (prompt (append (grp-test/prompts driver 'grp-implement)
                              (grp-test/prompts driver 'grp-review)))
        (should (string-match-p (regexp-quote grp-test/plan) prompt))
        (should (string-match-p "D1: use existing SQLite store" prompt))
        (should (string-match-p "Atomic updates without another service" prompt)))
      (let ((prompts (grp-test/prompts driver 'grp-implement)))
        (should-not (string-match-p "I1: completed items" (car prompts)))
        (should (string-match-p "Revision round: 2" (cadr prompts)))
        (should (string-match-p "I1: completed items" (cadr prompts))))
      (should (equal '(1 2)
                     (mapcar (lambda (entry) (plist-get entry :iteration))
                             (gptel-runner-get run 'grp-history)))))))

(ert-deftest grp-smoke-malformed-review-cannot-pass ()
  (grp-test/with-run
    (gptel-runner-fake-queue driver 'grp-planner (list :value grp-test/plan))
    (gptel-runner-fake-queue driver 'grp-implementer '(:value "Implementation"))
    (gptel-runner-fake-queue driver 'grp-reviewer
                             '(:value "Probably pass") '(:value "Still not JSON"))
    (let ((run (grp-test/start driver workspace)))
      (should (eq (gptel-runner-run-state run) 'failed))
      (should-not (gptel-runner-get run 'grp-review))
      (should (= 2 (length (grp-test/prompts driver 'grp-review))))
      (should (= 1 (length (grp-test/prompts driver 'grp-implement)))))))

(ert-deftest grp-smoke-pass-with-issues-is-rejected ()
  (grp-test/with-run
    (gptel-runner-fake-queue driver 'grp-planner (list :value grp-test/plan))
    (gptel-runner-fake-queue driver 'grp-implementer '(:value "Implementation"))
    (let ((bad (replace-regexp-in-string "\"revise\"" "\"pass\"" grp-test/revise)))
      (gptel-runner-fake-queue driver 'grp-reviewer
                               (list :value bad) (list :value bad)))
    (let ((run (grp-test/start driver workspace)))
      (should (eq (gptel-runner-run-state run) 'failed))
      (should-not (gptel-runner-get run 'grp-review)))))

(ert-deftest grp-smoke-blocked-review-stops-revision ()
  (grp-test/with-run
    (gptel-runner-fake-queue driver 'grp-planner (list :value grp-test/plan))
    (gptel-runner-fake-queue driver 'grp-implementer '(:value "Need credentials"))
    (gptel-runner-fake-queue
     driver 'grp-reviewer
     '(:value "{\"verdict\":\"blocked\",\"summary\":\"Need test credentials\",\"issues\":[]}"))
    (let ((run (grp-test/start driver workspace)))
      (should (eq (gptel-runner-run-state run) 'blocked))
      (should (= 1 (gptel-runner-iteration run 'grp-cycle)))
      (should (= 1 (length (grp-test/prompts driver 'grp-implement)))))))

(ert-deftest grp-smoke-identical-progress-stalls ()
  (grp-test/with-run
    (gptel-runner-fake-queue driver 'grp-planner (list :value grp-test/plan))
    (gptel-runner-fake-queue driver 'grp-implementer
                             '(:value "Attempt one") '(:value "Attempt two"))
    (gptel-runner-fake-queue driver 'grp-reviewer
                             (list :value grp-test/revise)
                             (list :value grp-test/revise))
    (let ((run (grp-test/start driver workspace)))
      (should (eq (gptel-runner-run-state run) 'stalled))
      (should (= 2 (gptel-runner-iteration run 'grp-cycle))))))

(ert-deftest grp-smoke-missing-plan-stops-before-dispatch ()
  (grp-test/with-run
    (let ((run
           (gptel-runner-start
            (gptel-runner-agent-step
             :id 'requires-plan :agent 'grp-implementer
             :context #'grp/plan-context :prompt "Implement")
            :goal "Add resumable import" :workspace workspace
            :driver driver :allow-writes t)))
      (should (eq (gptel-runner-run-state run) 'failed))
      (should-not (gptel-runner-fake-driver-starts driver)))))

;;; smoke-test.el ends here

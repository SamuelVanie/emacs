;;; smoke-test.el --- Network-free starter checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Run in a fresh Emacs (no presets, API credentials, or live calls needed):
;; emacs -Q --batch -L /path/to/gptel-runner -l /path/to/skill/scripts/smoke-test.el \
;;   -f ert-run-tests-batch-and-exit
;; Temporary workspaces and local context fixtures are removed after each test.
;; A fake success (:value ...) stands for a submitted handoff in these workflows.
;; Use :status discussion for ordinary prose or :status follow-up for a question.

;;; Code:

(require 'ert)
(require 'gptel-runner)

(load (expand-file-name
       "../assets/intent-preserving-pipeline.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil t)

(load (expand-file-name
       "../assets/minimal-pipeline.el"
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

(ert-deftest grp-smoke-reviewer-asks-for-evidence-before-submitting-json ()
  (grp-test/with-run
    (gptel-runner-fake-queue driver 'grp-planner (list :value grp-test/plan))
    (gptel-runner-fake-queue driver 'grp-implementer
                             '(:value "A1 passes")
                             '(:value "See import-test.el, case A1."))
    (gptel-runner-fake-queue driver 'grp-reviewer
                             '(:status follow-up :value "Which test covers A1?")
                             (list :value grp-test/pass))
    (let* ((run (grp-test/start driver workspace))
           (implementation-turns (grp-test/prompts driver 'grp-implement))
           (review-turns (grp-test/prompts driver 'grp-review)))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (string-search "Handoff from grp-implement:\nA1 passes" (car review-turns)))
      (should (string-search "Which test covers A1?" (cadr implementation-turns)))
      (should (string-search "See import-test.el, case A1." (cadr review-turns)))
      (should (equal (gptel-runner-get run 'grp-implementation) "A1 passes"))
      (should (eq (plist-get (gptel-runner-get run 'grp-review) :verdict) 'pass))
      (should (= 1 (gptel-runner-iteration run 'grp-cycle))))))

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

(ert-deftest grp-smoke-workspaces-preserve-intent-and-select-local-evidence ()
  (grp-test/with-run
    (let* ((directory (file-name-as-directory (file-truename workspace)))
           (shared-brief (expand-file-name "brief.md" directory))
           (repo-a (expand-file-name "a/" directory))
           (repo-b (expand-file-name "b/" directory)))
      (with-temp-file shared-brief (insert grp-test/brief))
      (dolist (repo (list repo-a repo-b))
        (make-directory repo)
        (with-temp-file (expand-file-name "local-notes.md" repo)
          (insert (if (equal repo repo-a) "Evidence for repo A" "Evidence for repo B"))))
      (gptel-runner-fake-queue driver 'grp-planner
                               (list :value grp-test/plan :manual t))
      (gptel-runner-fake-queue driver 'grp-implementer
                               '(:value "A report" :manual t)
                               '(:value "B report" :manual t))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-sequence
                :id 'multi-root :context #'grp/shared-context
                (gptel-runner-agent-step
                 :id 'plan :agent 'grp-planner
                 :prompt #'grp/plan-prompt :save-as 'grp-plan)
                (gptel-runner-parallel
                 :id 'repositories
                 :context (list #'grp/plan-context
                                (gptel-runner-context-file "local-notes.md"))
                 (gptel-runner-agent-step
                  :id 'a :agent 'grp-implementer :workspace "a"
                  :prompt "Implement A's contribution" :save-as 'a-report)
                 (gptel-runner-agent-step
                  :id 'b :agent 'grp-implementer :workspace repo-b
                  :prompt "Implement B's contribution" :save-as 'b-report)))
               :workspace workspace :goal "Add resumable import"
               :context (gptel-runner-context-file shared-brief)
               :driver driver :allow-writes t :max-concurrency 2))
             (planner (car (gptel-runner-fake-driver-starts driver))))
        (should (equal (gptel-runner-call-workspace planner) directory))
        (gptel-runner-record-decision
         run "D1: retain the common import contract" "Keep both repositories compatible")
        (gptel-runner-fake-release driver planner)
        (should (= 2 (gptel-runner-fake-driver-active driver)))
        (cl-mapc
         (lambda (call repo local-evidence other-evidence)
           (let ((prompt (gptel-runner-call-prompt call)))
             (should (equal (gptel-runner-call-workspace call) repo))
             (dolist (expected (list (concat "Workspace: " repo)
                                    "Add resumable import" grp-test/brief grp-test/plan
                                    "D1: retain the common import contract"
                                    "Keep both repositories compatible" local-evidence))
               (should (string-match-p (regexp-quote expected) prompt)))
             (should-not (string-match-p (regexp-quote other-evidence) prompt)))
           (gptel-runner-fake-release driver call))
         (cdr (gptel-runner-fake-driver-starts driver))
         (list repo-a repo-b)
         '("Evidence for repo A" "Evidence for repo B")
         '("Evidence for repo B" "Evidence for repo A"))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-run-workspace run) directory))
        (should (equal (gptel-runner-get run 'a-report) "A report"))
        (should (equal (gptel-runner-get run 'b-report) "B report"))))))

(ert-deftest grp-smoke-writer-aliases-serialize-while-other-repos-run ()
  (grp-test/with-run
    (make-directory (expand-file-name "a" workspace))
    (make-directory (expand-file-name "b" workspace))
    (make-symbolic-link (expand-file-name "a" workspace)
                        (expand-file-name "alias" workspace))
    (dotimes (_ 3)
      (gptel-runner-fake-queue driver 'grp-implementer '(:value "Done" :manual t)))
    (let* ((run
            (gptel-runner-start
             (gptel-runner-parallel
              :id 'writers :context #'grp/shared-context
              (gptel-runner-agent-step
               :id 'a :agent 'grp-implementer :workspace "a" :prompt "Work on A")
              (gptel-runner-agent-step
               :id 'alias :agent 'grp-implementer :workspace "alias" :prompt "Revisit A")
              (gptel-runner-agent-step
               :id 'b :agent 'grp-implementer :workspace "b" :prompt "Work on B"))
             :workspace workspace :goal "Implement independent changes"
             :driver driver :allow-writes t :max-concurrency 2))
           (calls (gptel-runner-run-calls run)))
      (should (equal (gptel-runner-fake-driver-starts driver)
                     (list (nth 0 calls) (nth 2 calls))))
      (should (equal (gptel-runner-call-workspace (nth 0 calls))
                     (gptel-runner-call-workspace (nth 1 calls))))
      (gptel-runner-fake-release driver (nth 0 calls))
      (should (equal (gptel-runner-fake-driver-starts driver)
                     (list (nth 0 calls) (nth 2 calls) (nth 1 calls))))
      (gptel-runner-fake-release driver (nth 1 calls))
      (gptel-runner-fake-release driver (nth 2 calls))
      (should (eq (gptel-runner-run-state run) 'succeeded)))))

(ert-deftest grp-smoke-missing-workspace-prevents-all-dispatch ()
  (grp-test/with-run
    (should-error
     (gptel-runner-start
      (gptel-runner-sequence
       :id 'preflight :context #'grp/shared-context
       (gptel-runner-agent-step
        :id 'first :agent 'grp-planner :prompt "Plan")
       (gptel-runner-agent-step
        :id 'missing :agent 'grp-planner :workspace "not-created" :prompt "Research"))
      :workspace workspace :driver driver :goal "Inspect repositories")
     :type 'user-error)
    (should-not (gptel-runner-fake-driver-starts driver))))

(ert-deftest grp-smoke-minimal-pipeline-waits-for-acceptance-before-publishing ()
  (grp-test/with-run
    (let ((gptel-runner-output-directory (expand-file-name "outputs/" workspace)))
      (gptel-runner-fake-queue driver 'grp-minimal-reader
                               '(:status discussion :value "Evidence: importer.el"))
      (let* ((run (gptel-runner-start 'grp-minimal :driver driver
                                      :goal "Inspect imports" :workspace workspace))
             (call (car (gptel-runner-run-calls run))))
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (should-not (gptel-runner-get run 'analysis))
        (should-not (gptel-runner-get run 'report))
        (gptel-runner-complete-call-from-buffer nil call)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= 1 (length (gptel-runner-fake-driver-starts driver))))
        (let ((artifact (gptel-runner-get run 'report)))
          (should (equal (gptel-runner-node-result run 'publish) artifact))
          (should (equal (gptel-runner-artifacts run) (list artifact)))
          (should (equal (gptel-runner-read-artifact run artifact) "Evidence: importer.el"))
          (should (file-readable-p (gptel-runner-artifact-file run artifact)))
          (should (equal (alist-get 'analysis (gptel-runner-outputs run))
                         "Evidence: importer.el")))))))

(ert-deftest grp-smoke-produced-artifact-context-reaches-agent ()
  (grp-test/with-run
    (let ((gptel-runner-output-directory (expand-file-name "outputs/" workspace)))
      (gptel-runner-fake-queue driver 'grp-minimal-reader '(:value "Verified"))
      (let ((run
             (gptel-runner-start
              (gptel-runner-sequence
               (gptel-runner-elisp-step
                :id 'publish :save-as 'report
                :function (lambda (run _node)
                            (gptel-runner-write-artifact run "report.txt" "Evidence A1")))
               (gptel-runner-agent-step
                :id 'consume :agent 'grp-minimal-reader :prompt "Check evidence"
                :context (gptel-runner-context-artifact 'report)))
              :driver driver :workspace workspace)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (string-match-p "Evidence A1"
                                (gptel-runner-call-prompt
                                 (car (gptel-runner-fake-driver-starts driver)))))))))

;;; smoke-test.el ends here

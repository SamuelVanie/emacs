;;; operations-smoke-test.el --- Network-free lifecycle checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs the existing nine starter checks plus operational checks below:
;; emacs -Q --batch -L /path/to/gptel-runner \
;;   -l /path/to/skill/scripts/operations-smoke-test.el \
;;   -f ert-run-tests-batch-and-exit
;; Private registry/store bindings isolate tests, not production workflow code.
;; All snapshots and simulated worker buffers are temporary; no providers run.

;;; Code:

(load (expand-file-name "smoke-test.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro grp-ops/with-environment (&rest body)
  "Run BODY with isolated registries and temporary snapshot storage."
  (declare (indent 0))
  `(grp-test/with-run
     (let ((gptel-runner--workflows (copy-hash-table gptel-runner--workflows))
           (gptel-runner-store--coordinators (make-hash-table :test #'eq))
           (gptel-runner-snapshot-directory
            (expand-file-name "snapshots" workspace)))
       (unwind-protect (progn ,@body)
         (dolist (run (gptel-runner-list-runs))
           (unless (memq (gptel-runner-run-state run)
                         '(succeeded failed blocked stalled cancelled skipped))
             (gptel-runner-abort-run run))
           (gptel-runner-store-cancel-save run)
           (dolist (call (gptel-runner-run-calls run))
             (when (buffer-live-p (gptel-runner-call-buffer call))
               (kill-buffer (gptel-runner-call-buffer call)))))))))

(defun grp-ops/wait-for-save (run)
  "Yield to snapshot timers for at most five seconds, then check RUN's save."
  (let ((deadline (+ (float-time) 5)))
    (while (and (< (float-time) deadline)
                (memq (gptel-runner-store-save-status run) '(pending writing)))
      (sleep-for 0.01)))
  (should (eq (gptel-runner-store-save-status run) 'clean))
  (should (file-readable-p (gptel-runner-run-snapshot-file run))))

(ert-deftest grp-ops-gate-keeps-result-unpublished-until-buffer-acceptance ()
  (grp-ops/with-environment
    (gptel-runner-defworkflow grp-ops-gated (:max-duration 60)
      (gptel-runner-sequence
       :id 'root :context #'grp/shared-context
       (gptel-runner-agent-step :id 'plan :agent 'grp-planner
                                :prompt "Plan" :save-as 'grp-plan :pause-after t)
       (gptel-runner-agent-step :id 'implement :agent 'grp-implementer
                                :context #'grp/plan-context :prompt "Implement")))
    (gptel-runner-fake-queue driver 'grp-planner '(:value "Unaccepted plan"))
    (gptel-runner-fake-queue driver 'grp-implementer '(:value "Done"))
    (let* ((run (gptel-runner-start 'grp-ops-gated :workspace workspace
                                   :goal "Preserve import" :driver driver
                                   :allow-writes t))
           (call (car (gptel-runner-run-calls run)))
           (buffer (generate-new-buffer " *grp-ops-feedback*")))
      (should (eq (gptel-runner-call-state call) 'waiting-feedback))
      (should (eq (gptel-runner-run-state run) 'running))
      (should (timerp (gptel-runner-run-duration-timer run)))
      (should-not (gptel-runner-get run 'grp-plan))
      (should (= 1 (length (gptel-runner-fake-driver-starts driver))))
      (setf (gptel-runner-call-buffer call) buffer)
      (with-current-buffer buffer
        (insert (propertize "Accepted corrected plan" 'gptel 'response)))
      (gptel-runner-record-decision run "D2: preserve CLI" "User correction")
      (gptel-runner-pause-run run)
      (should (eq (gptel-runner-run-state run) 'paused))
      (should-not (gptel-runner-run-duration-timer run))
      (should (gptel-runner-call-on-complete call))
      (gptel-runner-complete-call-from-buffer nil call)
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (equal (gptel-runner-get run 'grp-plan) "Accepted corrected plan"))
      (let ((prompt (cadr (gptel-runner-fake-driver-starts driver))))
        (should (string-match-p "Accepted corrected plan" (gptel-runner-call-prompt prompt)))
        (should (string-match-p "D2: preserve CLI" (gptel-runner-call-prompt prompt)))))))

(ert-deftest grp-ops-snapshot-restore-keeps-context-not-live-calls ()
  (grp-ops/with-environment
    (make-directory (expand-file-name "repo" workspace))
    (gptel-runner-defworkflow grp-ops-durable (:max-calls 8 :max-duration 60)
      (gptel-runner-sequence
       :id 'root :context #'grp/shared-context
       (gptel-runner-agent-step :id 'plan :agent 'grp-planner
                                :prompt "Plan" :save-as 'grp-plan)
       (gptel-runner-agent-step :id 'work :agent 'grp-implementer :workspace "repo"
                                :context #'grp/plan-context :prompt "Implement")
       (gptel-runner-agent-step :id 'report :agent 'grp-planner :prompt "Report")))
    (gptel-runner-fake-queue driver 'grp-planner (list :value grp-test/plan))
    (gptel-runner-fake-queue driver 'grp-implementer '(:value "Pending" :manual t))
    (let* ((original-callbacks 0)
           (restored-callbacks 0)
           (run (gptel-runner-start
                 'grp-ops-durable :workspace workspace :driver driver
                 :goal "Preserve import" :context grp-test/brief :allow-writes t
                 :callback (lambda (_) (cl-incf original-callbacks)))))
      (gptel-runner-record-decision run "D1: use SQLite" "Atomic updates")
      (gptel-runner-pause-run run)
      (should (eq (gptel-runner-store-save-status run) 'pending))
      (should-not (file-exists-p (gptel-runner-run-snapshot-file run)))
      (grp-ops/wait-for-save run)
      (let ((file (gptel-runner-run-snapshot-file run))
            (id (gptel-runner-run-id run))
            (restored-driver (gptel-runner-fake-driver-create)))
        (should (= 0 original-callbacks))
        (gptel-runner-forget-run run)
        (should (file-exists-p file))
        (should-not (gptel-runner-find-run id))
        (setq run (gptel-runner-load-run
                   file (lambda (_) (cl-incf restored-callbacks)) restored-driver))
        (should (eq (gptel-runner-run-state run) 'paused))
        (should-not (gptel-runner-run-calls run))
        (should (= 2 (gptel-runner-budget-calls (gptel-runner-run-budget run))))
        (should (equal (gptel-runner-get run 'grp-plan) grp-test/plan))
        (should (= 1 (length (gptel-runner-decisions run))))
        (should (equal (mapcar #'gptel-runner-event-type (gptel-runner-run-events run))
                       '(snapshot-loaded)))
        (gptel-runner-fake-queue restored-driver 'grp-implementer '(:value "Done"))
        (gptel-runner-fake-queue restored-driver 'grp-planner '(:value "Report"))
        (gptel-runner-resume-run run "Recheck restored artifacts")
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= 1 restored-callbacks))
        (should (= 0 original-callbacks))
        (let* ((calls (gptel-runner-fake-driver-starts restored-driver))
               (work (car calls)) (report (cadr calls)))
          (should (equal (gptel-runner-call-workspace work)
                         (file-name-as-directory
                          (file-truename (expand-file-name "repo" workspace)))))
          (dolist (call calls)
            (should (string-match-p "D1: use SQLite" (gptel-runner-call-prompt call)))
            (should (string-match-p (regexp-quote grp-test/brief)
                                    (gptel-runner-call-prompt call))))
          (should (string-match-p "Recheck restored artifacts" (gptel-runner-call-prompt work)))
          (should-not (string-match-p "Recheck restored artifacts" (gptel-runner-call-prompt report))))))))

(ert-deftest grp-ops-retry-review-keeps-plan-work-and-history ()
  (grp-ops/with-environment
    (gptel-runner-fake-queue driver 'grp-planner (list :value grp-test/plan))
    (gptel-runner-fake-queue driver 'grp-implementer '(:value "Implementation"))
    (gptel-runner-fake-queue
     driver 'grp-reviewer
     '(:value "{\"verdict\":\"blocked\",\"summary\":\"Missing tests\",\"issues\":[]}")
     (list :value grp-test/pass :manual t))
    (let ((run (grp-test/start driver workspace)) (callbacks 0))
      (should (eq (gptel-runner-run-state run) 'blocked))
      (gptel-runner-record-decision run "D2: preserve CLI" "User correction")
      (gptel-runner-retry (gptel-runner-run-id run) :from-node 'grp-review
                          :feedback "Test dependency installed"
                          :callback (lambda (_) (cl-incf callbacks)))
      (should-not (gptel-runner-get run 'grp-review))
      (should (equal (gptel-runner-get run 'grp-plan) grp-test/plan))
      (should (equal (gptel-runner-get run 'grp-implementation) "Implementation"))
      (should (= 1 (length (gptel-runner-get run 'grp-history))))
      (let ((call (car (last (gptel-runner-fake-driver-starts driver)))))
        (should (string-match-p "Test dependency installed" (gptel-runner-call-prompt call)))
        (should (string-match-p "D2: preserve CLI" (gptel-runner-call-prompt call)))
        (gptel-runner-fake-release driver call))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= 1 callbacks))
      (should (= 1 (length (grp-test/prompts driver 'grp-plan))))
      (should (= 1 (length (grp-test/prompts driver 'grp-implement))))
      (should (equal '(1 2) (mapcar (lambda (entry) (plist-get entry :iteration))
                                   (gptel-runner-get run 'grp-history)))))))

(ert-deftest grp-ops-continue-success-starts-clean-cycle-and-archives-results ()
  (grp-ops/with-environment
    (gptel-runner-fake-queue driver 'grp-planner
                            (list :value grp-test/plan) '(:value "New plan" :manual t))
    (gptel-runner-fake-queue driver 'grp-implementer '(:value "Implementation") '(:value "Updated"))
    (gptel-runner-fake-queue driver 'grp-reviewer (list :value grp-test/pass) (list :value grp-test/pass))
    (let ((callbacks 0) run)
      (setq run (gptel-runner-start
                 'grp-intent-preserving :workspace workspace :driver driver
                 :goal "Old goal" :context grp-test/brief :allow-writes t
                 :callback (lambda (_) (cl-incf callbacks))))
      (gptel-runner-put run 'manual-context "Keep me")
      (gptel-runner-record-decision run "D1: preserve CLI" "Compatibility")
      (gptel-runner-continue run "New goal with original constraints")
      (should (= 1 callbacks))
      (should (= 0 (gptel-runner-iteration run 'grp-cycle)))
      (dolist (key '(grp-plan grp-implementation grp-review grp-history))
        (should-not (gptel-runner-get run key)))
      (should (equal (gptel-runner-get run 'manual-context) "Keep me"))
      (let ((archive (car (gptel-runner-continuations run))))
        (should (eq (plist-get archive :restart-mode) 'new-cycle))
        (should (equal (plist-get archive :previous-goal) "Old goal"))
        (should (assoc 'grp-history (plist-get archive :results))))
      (gptel-runner-fake-release driver (car (last (gptel-runner-fake-driver-starts driver))))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= 1 callbacks)) ; The previously invoked callback is not reused.
      (should (= 6 (gptel-runner-budget-calls (gptel-runner-run-budget run))))
      (dolist (call (nthcdr 3 (gptel-runner-fake-driver-starts driver)))
        (dolist (text '("Old goal" "New goal with original constraints" "D1: preserve CLI"))
          (should (string-match-p text (gptel-runner-call-prompt call))))))))

(ert-deftest grp-ops-continue-failure-retains-earlier-plan ()
  (grp-ops/with-environment
    (gptel-runner-fake-queue driver 'grp-planner (list :value grp-test/plan))
    (gptel-runner-fake-queue driver 'grp-implementer
                            '(:status error :value "Environment failure" :metadata (:http-status 400))
                            '(:value "Revised"))
    (gptel-runner-fake-queue driver 'grp-reviewer (list :value grp-test/pass))
    (let ((run (grp-test/start driver workspace)))
      (should (eq (gptel-runner-run-state run) 'failed))
      (gptel-runner-continue run "Changed goal requiring plan review")
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (equal (gptel-runner-get run 'grp-plan) grp-test/plan))
      (should (= 1 (length (grp-test/prompts driver 'grp-plan))))
      (should (eq (plist-get (car (gptel-runner-continuations run)) :restart-mode)
                  'checkpoint)))))

(ert-deftest grp-ops-budget-extension-and-retry-reapplication-are-cumulative ()
  (grp-ops/with-environment
    (gptel-runner-fake-queue driver 'grp-planner '(:value "Plan"))
    (gptel-runner-fake-queue driver 'grp-implementer
                            '(:status error :value "Environment failure" :metadata (:http-status 400))
                            '(:value "Done"))
    (let* ((run (gptel-runner-start
                 (gptel-runner-sequence
                  :id 'root
                  (gptel-runner-agent-step :id 'plan :agent 'grp-planner
                                           :prompt "Plan" :save-as 'plan)
                  (gptel-runner-agent-step :id 'work :agent 'grp-implementer :prompt "Work"))
                 :driver driver :workspace workspace :allow-writes t
                 :goal "Same goal" :max-calls 1 :max-requests 1))
           (budget (gptel-runner-run-budget run)))
      (should (eq (gptel-runner-run-state run) 'failed))
      (should-error (gptel-runner-retry run) :type 'user-error)
      (should-error (gptel-runner-extend run :additional-calls 1) :type 'user-error)
      (should (= 1 (gptel-runner-budget-max-calls budget)))
      (gptel-runner-extend run :additional-calls 1 :additional-requests 1)
      (should (eq (gptel-runner-run-state run) 'failed))
      (gptel-runner-retry run)
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= 3 (gptel-runner-budget-calls budget)))
      (should (= 3 (gptel-runner-budget-max-calls budget)))
      (should (= 3 (gptel-runner-budget-requests budget)))
      (should (= 3 (gptel-runner-budget-max-requests budget)))
      (should (equal (gptel-runner-get run 'plan) "Plan"))
      (should (equal (gptel-runner-run-goal run) "Same goal"))
      (should (seq-some
               (lambda (event)
                 (and (eq (gptel-runner-event-type event) 'run-retried)
                      (plist-get (gptel-runner-event-data event) :reapplied-extension)))
               (gptel-runner-run-events run))))))

(ert-deftest grp-ops-repeat-extension-appends-history-without-changing-definition ()
  (grp-ops/with-environment
    (gptel-runner-defworkflow grp-ops-repeat (:max-calls 4)
      (gptel-runner-repeat-until
       :id 'cycle :max 1 :until (lambda (run) (equal (gptel-runner-get run 'review) "pass"))
       :collect-keys '(review) :save-history-as 'history
       :body (gptel-runner-agent-step :id 'review :agent 'grp-planner
                                      :prompt "Review" :save-as 'review)))
    (gptel-runner-fake-queue driver 'grp-planner '(:value "revise") '(:value "pass"))
    (let ((run (gptel-runner-start 'grp-ops-repeat :driver driver :workspace workspace)))
      (should (eq (gptel-runner-run-state run) 'failed))
      (should-error (gptel-runner-retry run) :type 'user-error)
      (gptel-runner-extend-repeat run 'cycle 1)
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= 2 (gptel-runner-iteration run 'cycle)))
      (should (= 2 (gptel-runner-budget-calls (gptel-runner-run-budget run))))
      (should (= 1 (plist-get (gptel-runner-node-properties
                               (gptel-runner-workflow-root
                                (gptel-runner-run-workflow run))) :max)))
      (should (equal '(1 2) (mapcar (lambda (entry) (plist-get entry :iteration))
                                   (gptel-runner-get run 'history)))))))

;;; operations-smoke-test.el ends here

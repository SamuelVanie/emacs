(gptel-runner-register-agent
 'plan :preset 'gptel-plan :workspace-mode 'read)

(gptel-runner-register-agent
 'dev :preset 'gptel-agent :workspace-mode 'write)

(gptel-runner-register-agent
 'review :preset 'mayuri-reviewer :workspace-mode 'read
 :schema gptel-runner-review-schema :parser #'gptel-runner-parse-review
 :validator #'gptel-runner-valid-review-p)

(gptel-runner-register-agent
 'summarizer :preset 'mayuri-task-summarizer :workspace-mode 'read)

(defun myproject/plan-prompt (run _node)
  "Build a planning prompt for RUN."
  (let ((goal (gptel-runner-run-goal run))
        (workspace (gptel-runner-run-workspace run))
        (iteration (gptel-runner-iteration run 'review-cycle))
        (review (gptel-runner-get run 'review)))
    (if (null review)
        (format
         (concat "You are the techlead for this project.\n"
                 "Goal (ticket):\n%s\n\nWorkspace: %s\nPlan iteration: %d\n"
                 "No review feedback yet. Plan the implementation steps required to achieve this goal. "
                 "Divide the task into actionable steps suitable for the developer. "
                 "Consider dependencies and the overall approach.")
         goal workspace iteration)
      (format
       (concat "You are the techlead for this project.\n"
               "Goal (ticket):\n%s\n\nWorkspace: %s\nPlan iteration: %d\n"
               "Reviewer's feedback: %S\n"
               "Update the plan as necessary. Plan revised implementation or corrective steps to address all reviewer comments, maintaining a clear actionable breakdown for the developer")
       goal workspace iteration review))))

(defun myproject/implementation-prompt (run _node)
  "Build an implementation prompt for RUN."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\nRevision iteration: %d\n"
           "Implementation plan: %S\n"
           "Follow the plan provided. Inspect actual files, make the planned changes, run tests, and return a concise report. Don't forget to commit at appropriate points of time")
   (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
   (gptel-runner-iteration run 'review-cycle)
   (gptel-runner-get run 'plan)))

(defun myproject/review-prompt (run _node)
  "Build an independent review prompt for RUN."
  (format
   (concat "Review the current project's workspace for this goal:\n%s\n\nWorkspace: %s\n"
           "Implementation report: %S\n"
           "Return only the required review JSON. Do not modify files.")
   (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
   (gptel-runner-get run 'implementation)))

(defun myproject/summarizer-prompt (run _node)
  "Build an independent review prompt for RUN."
  (format
   (concat "Here is the full list of the work done by the Techlead, the developer and the reviewer in the current project's workspace for this goal:\n%s\n\nWorkspace: %s\n\n"
           "Summary: %S\n\n"
           "Return the summary document please")
   (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
   (gptel-runner-get run 'history)))

(gptel-runner-defworkflow plan-implement-review
    (:max-requests 30 :max-calls 16 :max-concurrency 2 :max-duration 3600)
  (gptel-runner-repeat-until
   :id 'review-cycle :max 5
   :until (lambda (run)
            (eq (plist-get (gptel-runner-get run 'review) :verdict) 'pass))
   :stop-when (lambda (run)
                (eq (plist-get (gptel-runner-get run 'review) :verdict)
                    'blocked))
   :progress-key #'gptel-runner-review-progress-key
   :collect-keys '(plan implementation review)
   :save-history-as 'history
   :body
   (gptel-runner-sequence
    (gptel-runner-agent-step
     :id 'plan :agent 'plan
     :prompt #'myproject/plan-prompt :save-as 'plan)
    (gptel-runner-agent-step
     :id 'implement :agent 'dev
     :prompt #'myproject/implementation-prompt :save-as 'implementation)
    (gptel-runner-agent-step
     :id 'review :agent 'review
     :prompt #'myproject/review-prompt
     :save-as 'review :repair-invalid t)))
  (gptel-runner-agent-step
   :id 'summarize
   :agent 'summarizer
   :prompt #'myproject/summarizer-prompt
   :save-as 'final-report))


;; (gptel-runner-start 'plan-implement-review :goal (read-string "What's the goal: ") :workspace "~/projects/dailybanking-mobile-bff/"
;;                     :allow-writes t :allow-unconfirmed-tools t)

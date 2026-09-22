(gptel-runner-register-agent
 'plan :preset 'gptel-plan :workspace-mode 'read)

(defun myproject/plan-prompt (run _node)
  "Build a planning prompt for RUN."
  (let ((goal (gptel-runner-run-goal run))
        (workspace (gptel-runner-run-workspace run))
        (history (gptel-runner-get run 'history))
        )
    (if (null review)
        (format
         (concat "You are an architect, you have to find the most suitable solution to implement the feature. You should not repeat any solution that already exists"
                 "Goal (ticket):\n%s\n\nWorkspace: %s\nPropositions list: %d\n"
                 "No proposition yet. Make sure that the project's convention are always followed. Propose a clear solution permitting to achieve this goal. ")
         goal workspace history)
      (format
       (concat "You are an architect, you have to find the most suitable solution to implement the feature. You should not repeat any solution that already exists\n"
               "Goal (ticket):\n%s\n\nWorkspace: %s\nPropositions list: %d\n"
               "Add your solution to proposition list but don't copy the ones that already exists. You should ask to yourself, is there something better than those propositions ? If there something that could be done better in this ? Make sure that the project's convention are always followed.")
       goal workspace history))))

(defun myproject/implementation-prompt (run _node)
  "Build an implementation prompt for RUN."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\n"
           "Implementation plan: %S\n"
           "Follow the plan provided. Use TDD whenever possible. Use context7 when the tool is available (flowable, nestjs, java packages, etc). Inspect actual files, make the planned changes, run tests, and return a concise report. Don't forget to commit at appropriate points of time")
   (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
   (gptel-runner-iteration run 'review-cycle)
   (gptel-runner-get run 'plan)))

(gptel-runner-defworkflow plan-implement-review
    (:max-requests 40 :max-calls 26 :max-concurrency 2 :max-duration 5400)
  (gptel-runner-sequence
   :id 'plan-implement-review-save-seq
   (gptel-runner-repeat-until
    :id 'review-cycle :max 8
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
     :id 'review-cycle-inner-seq
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
  )


;; (gptel-runner-start 'plan-implement-review :goal (read-string "What's the goal: ") :workspace "~/projects/dailybanking-mobile-bff/"
;;                     :allow-writes t :allow-unconfirmed-tools t)


;; An example of step prompt to perform before launching a pipeline
;; Do not write any plan for now we have to make the request statement as clear as possible to begin with. What we want to achieve is to delete the file from the untrusted bucket when the scan finishes wether or not it was declared clean or infected. If any part of that is not properly stated or there're missing information that couldn't be gathered from the codebase, ask me. We will make the task as clear as possible before planning it

;; adding an AGENTS.md and telling at the end of the goal "follow the AGENTS.md" could be interesting

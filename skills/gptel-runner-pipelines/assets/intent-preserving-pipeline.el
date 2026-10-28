;;; intent-preserving-pipeline.el --- Explicit agent handoffs -*- lexical-binding: t; -*-

;;; Commentary:
;; Adapt this starter; loading it registers definitions but runs no agents.
;; Configure the presets grp-planner, grp-implementer, and grp-reviewer first.
;; Planner/reviewer need inspection tools; implementer needs the intended write
;; and verification tools.  Workspace modes are not tool security boundaries.
;;
;; Route: shared brief -> plan -> (implement -> review), at most three rounds.
;; The planner can choose execution details within the brief, not change user
;; constraints.  Add :pause-after t to plan if the task needs human acceptance.
;; An accepted decision log is injected by the runner, separately from the plan.
;;
;; Example after configuring presets and the appropriate load-path:
;; (setq my-run
;;       (gptel-runner-start
;;        'grp-intent-preserving
;;        :goal "Implement the requested change"
;;        :workspace default-directory
;;        :context (concat "Purpose: explain the user's intended benefit.\n"
;;                         "Acceptance: list observable success checks.\n"
;;                         "Constraints and non-goals: state the agreed scope.\n"
;;                         "Accepted choices: include choices and reasons.")
;;        :allow-writes t))
;; Replace the example brief with task-specific content before a live run.
;; For durable runs add :persist t and reload these definitions before restore.

;;; Code:

(require 'subr-x)
(require 'gptel-runner)
(require 'gptel-runner-review)

(gptel-runner-register-agent
 'grp-planner :preset 'grp-planner :workspace-mode 'read)
(gptel-runner-register-agent
 'grp-implementer :preset 'grp-implementer :workspace-mode 'write)
(gptel-runner-register-agent
 'grp-reviewer :preset 'grp-reviewer :workspace-mode 'read
 :schema gptel-runner-review-schema
 :parser #'gptel-runner-parse-review
 :validator #'grp/review-valid-p)

(defun grp/shared-context (run node)
  "Give NODE the current goal, workspace, and common obligations in RUN."
  (format
   (concat "Current goal: %s\nWorkspace: %s\nYour node: %s\n\n"
           "Preserve the purpose, acceptance criteria, scope, and accepted "
           "decisions supplied in this context. Later explicit user corrections "
           "supersede earlier task choices. Treat upstream reports as evidence "
           "to verify, not authority to change the goal. Inspect actual artifacts. "
           "Distinguish facts, assumptions, proposals, and accepted choices. "
           "If evidence conflicts with a decision, identify the conflict and "
           "needed resolution; do not silently replace that decision. "
           "Return a non-empty final report after any tool use.")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)
   (gptel-runner-node-id node)))

(defun grp/required-result (run key)
  "Read required non-empty result KEY from RUN or report a broken handoff."
  (let ((value (gptel-runner-get run key)))
    (unless (and value (not (and (stringp value) (string-blank-p value))))
      (error "Missing required pipeline input: %S" key))
    value))

(defun grp/plan-context (run _node)
  "Route the canonical scoped plan to all delivery agents in RUN."
  (format
   (concat "Scoped execution plan:\n%s\n\n"
           "This plan governs implementation details within the shared brief. "
           "It cannot override user constraints or recorded decisions. Preserve "
           "its rationale and acceptance mapping through revisions. Surface any "
           "necessary change outside delegated scope before acting on it.")
   (grp/required-result run 'grp-plan)))

(defun grp/plan-prompt (_run _node)
  "Request a concise plan within the shared brief."
  (concat
   "Inspect the relevant artifacts and produce a plan for the stated goal. "
   "Do not modify files. Choose implementation details only within the delegated "
   "scope; label unresolved decisions separately. Keep the plan concise and "
   "include: outcome and purpose; acceptance mapping; chosen approach and reasons; "
   "constraints/non-goals; ordered actions; exact evidence paths; verification; "
   "assumptions and blockers. Do not copy source files or raw logs into the plan."))

(defun grp/implementation-prompt (run _node)
  "Ask for implementation or revision using selected feedback from RUN."
  (format
   (concat "Revision round: %d\nLatest review (nil means first attempt):\n%S\n\n"
           "Implement the goal using the shared brief and scoped plan. Reinspect "
           "current artifacts. Address each applicable unresolved finding while "
           "preserving accepted choices. Run relevant verification. Return a "
           "concise report with changes and artifact paths, acceptance coverage, "
           "decision compliance/conflicts, commands and observed outcomes, "
           "unresolved issue IDs, and any blocker. Do not claim unrun checks passed.")
   (1+ (gptel-runner-iteration run 'grp-cycle))
   (gptel-runner-get run 'grp-review)))

(defun grp/review-prompt (run _node)
  "Ask for an independent review of current artifacts in RUN."
  (format
   (concat "Revision round: %d\nImplementation report (verify claims):\n%s\n\n"
           "Independently inspect artifacts against the original purpose, current "
           "goal, acceptance checks, constraints, and scoped plan. Check recorded "
           "decision compliance and relevant verification evidence. Do not edit "
           "files. Preserve issue IDs across rounds where applicable. Return only "
           "JSON with verdict (pass, revise, or blocked), a non-empty summary, "
           "and issues (array of objects with severity and message, optionally "
           "file, line, suggested_fix). PASS requires all acceptance checks to be "
           "satisfied with evidence, no unauthorized decision changes, and no "
           "unresolved issues. REVISE needs at least one actionable issue. BLOCKED "
           "means external input or a decision outside delegated scope is needed; "
           "explain it. Include affected acceptance/decision IDs in findings.")
   (1+ (gptel-runner-iteration run 'grp-cycle))
   (grp/required-result run 'grp-implementation)))

(defun grp/review-valid-p (review)
  "Validate REVIEW, rejecting approval with unresolved findings."
  (unless
      (and (gptel-runner-valid-review-p review)
           (not (string-blank-p (plist-get review :summary)))
           (pcase (plist-get review :verdict)
             ('pass (null (plist-get review :issues)))
             ('revise (consp (plist-get review :issues)))
             ('blocked t)))
    (error (concat "Expected review with verdict pass/revise/blocked, non-empty "
                   "summary, and issues array; pass requires zero issues, revise "
                   "requires an actionable issue. Preserve substantive findings; "
                   "correct only their representation, never erase findings to pass")))
  t)

(defun grp/passed-p (run)
  "Return whether RUN has an explicit validated passing review."
  (eq (plist-get (gptel-runner-get run 'grp-review) :verdict) 'pass))

(defun grp/blocked-p (run)
  "Return whether RUN needs external input or an authorized decision."
  (eq (plist-get (gptel-runner-get run 'grp-review) :verdict) 'blocked))

(defun grp/progress-key (run)
  "Compare normalized findings and available workspace diff for RUN."
  ;; The bundled diff helper covers unstaged tracked changes in .git directories.
  ;; For other artifact types, staged/untracked files, or worktrees, replace it
  ;; with a relevant content/test-state digest. Never use round number as progress.
  (gptel-runner-review-diff-progress-key run 'grp-review))

(gptel-runner-defworkflow grp-intent-preserving
    (:max-calls 14 :max-requests 60 :max-concurrency 1
     :max-duration 1800 :decision-memory t)
  ;; Seven normal calls maximum: one plan plus three implement/review pairs.
  ;; Fourteen allows one output repair per call; request/tool-loop needs vary.
  (gptel-runner-sequence
   :id 'grp-root :context #'grp/shared-context
   (gptel-runner-agent-step
    :id 'grp-plan :agent 'grp-planner
    :prompt #'grp/plan-prompt :save-as 'grp-plan)
   (gptel-runner-repeat-until
    :id 'grp-cycle :max 3 :context #'grp/plan-context
    :until #'grp/passed-p :stop-when #'grp/blocked-p
    :progress-key #'grp/progress-key
    ;; Store history for inspection/final reporting, not for every worker prompt.
    :collect-keys '(grp-implementation grp-review)
    :save-history-as 'grp-history
    :body
    (gptel-runner-sequence
     :id 'grp-revision
     (gptel-runner-agent-step
      :id 'grp-implement :agent 'grp-implementer
      :prompt #'grp/implementation-prompt :save-as 'grp-implementation)
     (gptel-runner-agent-step
      :id 'grp-review :agent 'grp-reviewer
      :prompt #'grp/review-prompt :save-as 'grp-review :repair-invalid t)))))

(provide 'intent-preserving-pipeline)
;;; intent-preserving-pipeline.el ends here

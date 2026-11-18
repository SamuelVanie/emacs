;;; minimal-pipeline.el --- Inspect and retain a report -*- lexical-binding: t; -*-

;;; Commentary:
;; Discover a compatible preset and replace the placeholder below before a live
;; run.  Loading registers definitions only.  Add stages only for actual work.
;; (gptel-runner-start 'grp-minimal :goal "Explain the importer"
;;                     :workspace "/path/to/project/" :allow-unconfirmed-tools t)
;; The reader uses RunnerHandoff to submit the text stored under 'analysis.
;; The Elisp publisher reads that value and writes the report.
;; An ordinary reply leaves the reader's chat open for feedback or acceptance.
;; Add :persist t to the start options for automatic runner-owned checkpoints.
;; The same definition can be loaded by a job init.
;; Read 'report with gptel-runner-get, then gptel-runner-read-artifact.

;;; Code:

(require 'gptel-runner)

(gptel-runner-register-agent
 'grp-minimal-reader :preset 'grp-minimal-reader :workspace-mode 'read)

(gptel-runner-defworkflow grp-minimal
    (:max-calls 2 :max-requests 6 :max-duration 300)
  (gptel-runner-sequence
   :id 'pipeline
   (gptel-runner-agent-step
    :id 'inspect :agent 'grp-minimal-reader :save-as 'analysis
    :prompt (lambda (run node)
              (format (concat "Goal: %s\nWorkspace: %s\nInspect the project. "
                              "Submit a concise report with evidence using RunnerHandoff.")
                      (gptel-runner-run-goal run)
                      (gptel-runner-node-workspace run node))))
   (gptel-runner-elisp-step
    :id 'publish :save-as 'report
    :function (lambda (run _node)
                (gptel-runner-write-artifact
                 run "report.md" (gptel-runner-get run 'analysis) "text/markdown")))))

(provide 'minimal-pipeline)
;;; minimal-pipeline.el ends here

;;; minimal-pipeline.el --- Inspect and retain a report -*- lexical-binding: t; -*-

;;; Commentary:
;; Discover a compatible preset and replace the placeholder below before a live
;; run.  Loading registers definitions only.  Add stages only for actual work.
;; (gptel-runner-start 'grp-minimal :goal "Explain the importer"
;;                     :workspace "/path/to/project/" :allow-unconfirmed-tools t)
;; The same definition can be loaded by a job init; no output adapter is needed.
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
              (format "Goal: %s\nWorkspace: %s\nInspect and return a concise report with evidence."
                      (gptel-runner-run-goal run)
                      (gptel-runner-node-workspace run node))))
   (gptel-runner-elisp-step
    :id 'publish :save-as 'report
    :function (lambda (run _node)
                (gptel-runner-write-artifact
                 run "report.md" (gptel-runner-get run 'analysis) "text/markdown")))))

(provide 'minimal-pipeline)
;;; minimal-pipeline.el ends here

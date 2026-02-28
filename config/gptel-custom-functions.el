(defun gptel--hide-tool-results-in-region (beg end)
  "Helper: Hide tool results in range by backing up their property and setting to ignore."
  (let ((prop))
    (save-excursion
      (goto-char beg)
      (while (and (<= (point) end)
                  ;; Search for gptel property where value is a list starting with 'tool
                  (setq prop (text-property-search-forward
    			      'gptel 'tool
    			      (lambda (val actual) (eq val (car-safe actual))))))
        (let* ((start (prop-match-beginning prop))
    	       (finish (prop-match-end prop))
    	       (original-val (prop-match-value prop)))
          ;; 1. Save the original 'tool' value to a backup property
          (put-text-property start finish 'gptel-tool-backup original-val)
          ;; 2. Set the main property to 'ignore so gptel skips it
          (put-text-property start finish 'gptel 'ignore))))))

(defun gptel--restore-tool-results-in-region (beg end)
  "Helper: Restore tool results in range from the backup property."
  (let ((prop))
    (save-excursion
      (goto-char beg)
      (while (and (<= (point) end)
                  ;; Search for gptel property explicitly set to 'ignore
                  (setq prop (text-property-search-forward
    			      'gptel 'ignore #'eq)))
        (let* ((start (prop-match-beginning prop))
    	       (finish (prop-match-end prop))
    	       ;; Check if we have a backup for this region
    	       (backup (get-text-property start 'gptel-tool-backup)))
          (when backup
            ;; 1. Restore the original value
            (put-text-property start finish 'gptel backup)
            ;; 2. Remove the backup property to clean up
            (remove-text-properties start finish '(gptel-tool-backup nil))))))))

(defun gptel-auto-hide-tool-results (beg end)
  "Hook function: automatically hide tool results in new responses."
  (gptel--hide-tool-results-in-region beg end))

(define-minor-mode gptel-no-tool-history-mode
  "Toggle excluding tool results from the conversation history context.

    When ENABLED:
    1. Existing tool results in the buffer are marked 'ignore'.
    2. New tool results (via hook) are marked 'ignore'.

    When DISABLED:
    1. The hook is removed.
    2. All ignored tool results are restored to their original state."
  :global nil
  :lighter " NoTool"
  (if gptel-no-tool-history-mode
      (progn
        ;; 1. Add hook locally for future responses
        (add-hook 'gptel-post-response-functions #'gptel-auto-hide-tool-results 0 t)
        ;; 2. Process the whole buffer immediately to hide existing ones
        (gptel--hide-tool-results-in-region (point-min) (point-max)))
    
    ;; ELSE (Turning off)
    (progn
      ;; 1. Remove the hook locally
      (remove-hook 'gptel-post-response-functions #'gptel-auto-hide-tool-results t)
      ;; 2. Restore all hidden items in the buffer
      (gptel--restore-tool-results-in-region (point-min) (point-max)))))


;; loads presets
(load-file (format "%s%s/%s%s" user-emacs-directory "presets" "command_line" ".el"))



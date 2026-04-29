(defvar smv/gptel-intercept-next-tool nil
  "When non-nil, intercept the next tool call, prompt for a reason, then disable.")

(defun smv/gptel-intercept-tool-call (plist)
  (when smv/gptel-intercept-next-tool
    (setq smv/gptel-intercept-next-tool nil)
    (let* ((name (plist-get plist :name))
           (args (plist-get plist :args))
           (reason (read-string
                    (format "Block tool '%s' %S — reason: " name args))))
      (list :block (if (string-empty-p reason)
                       (format "Tool '%s' was blocked by the user." name)
                     (format "THE USER HAS CANCELLED THE TOOL CALL WITH THE FOLLOWING REASON.\nAJUST ACCORDINGLY.\nREASON:\n%s" reason))))))

(add-hook 'gptel-post-tool-call-functions #'smv/gptel-intercept-tool-call)

(defun smv/gptel-intercept-next-tool ()
  (interactive)
  (setq smv/gptel-intercept-next-tool t)
  (message "gptel: next tool call will be intercepted."))

(keymap-global-set "C-c g i" #'smv/gptel-intercept-next-tool)

(defun fetch-web-page-content (url)
  "Fetch web page content from URL using org-eww-copy-for-org-mode.
Converts the org-formatted content to plain text suitable for LLM processing.
Returns clean UTF-8 text without org markup."
  (let ((original-buffer (current-buffer))
        (original-kill-ring-length (length kill-ring))
        org-content plain-content)
    (save-window-excursion
      (save-excursion
        ;; Create and switch to a temporary buffer for eww
        (let ((temp-eww-buffer (generate-new-buffer "*eww-temp*")))
          (unwind-protect
              (progn
                (switch-to-buffer temp-eww-buffer)
                ;; Fetch the page with eww in the temp buffer
                (eww url)
                ;; Wait for the page to actually load
                (let ((max-wait 30) ; Maximum 30 seconds
                      (wait-time 0))
                  (while (and (< wait-time max-wait)
                              (or (not (bound-and-true-p eww-data))
                                  (string-match-p "Loading" (buffer-string))
                                  (< (buffer-size) 100))) ; Very small buffer suggests still loading
                    (sit-for 0.5)
                    (setq wait-time (+ wait-time 0.5))))
                
                ;; Check if we have actual content
                (if (and (bound-and-true-p eww-data)
                         (> (buffer-size) 100)
                         (not (string-match-p "Loading" (buffer-string))))
                    (progn
                      ;; Use org-eww-copy-for-org-mode to copy content to kill ring
                      (when (fboundp 'org-eww-copy-for-org-mode)
                        (org-eww-copy-for-org-mode)
                        ;; Get the org content from kill ring if something new was added
                        (when (> (length kill-ring) original-kill-ring-length)
                          (setq org-content (car kill-ring)))))
                  (error "Failed to load webpage or timeout reached")))
            ;; Clean up the eww temporary buffer
            (when (buffer-live-p temp-eww-buffer)
              (kill-buffer temp-eww-buffer)))))
      
      ;; Convert org content to plain text using org export
      (when org-content
        (let ((temp-org-buffer (generate-new-buffer "*org-temp*")))
          (unwind-protect
              (with-current-buffer temp-org-buffer
                ;; Insert the org content
                (insert org-content)
                ;; Enable org-mode
                (org-mode)
                ;; Export to ASCII/plain text
                (let ((exported-content (org-export-as 'ascii nil nil t)))
                  (setq plain-content exported-content)))
            ;; Clean up the org temporary buffer  
            (when (buffer-live-p temp-org-buffer)
              (kill-buffer temp-org-buffer))))))
    
    ;; Clean up the plain text content for LLM processing
    (when plain-content
      (with-temp-buffer
        (insert plain-content)
        ;; Remove excessive whitespace and clean up formatting
        (goto-char (point-min))
        ;; Remove ASCII export headers/footers if any
        (while (re-search-forward "^\\([=~]\\)\\1+$" nil t)
          (replace-match ""))
        ;; Clean up multiple newlines
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (replace-match "\n\n"))
        ;; Clean up excessive spaces
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+" nil t)
          (replace-match " "))
        ;; Return clean content
        (string-trim (buffer-string))))))

(defun fetch-web-page-content (url)
  "Fetch web page content from URL using org-eww-copy-for-org-mode.
Converts the org-formatted content to plain text suitable for LLM processing.
Returns clean UTF-8 text without org markup."
  (let ((original-buffer (current-buffer))
        (original-kill-ring-length (length kill-ring))
        org-content plain-content)
    (save-window-excursion
      (save-excursion
        ;; Create and switch to a temporary buffer for eww
        (let ((temp-eww-buffer (generate-new-buffer "*eww-temp*")))
          (unwind-protect
              (progn
                (switch-to-buffer temp-eww-buffer)
                ;; Fetch the page with eww in the temp buffer
                (eww url)
                ;; Wait for the page to actually load
                (let ((max-wait 30) ; Maximum 30 seconds
                      (wait-time 0))
                  (while (and (< wait-time max-wait)
                              (or (not (bound-and-true-p eww-data))
                                  (string-match-p "Loading" (buffer-string))
                                  (< (buffer-size) 100))) ; Very small buffer suggests still loading
                    (sit-for 0.5)
                    (setq wait-time (+ wait-time 0.5))))
                
                ;; Check if we have actual content
                (if (and (bound-and-true-p eww-data)
                         (> (buffer-size) 100)
                         (not (string-match-p "Loading" (buffer-string))))
                    (progn
                      ;; Use org-eww-copy-for-org-mode to copy content to kill ring
                      (when (fboundp 'org-eww-copy-for-org-mode)
                        (org-eww-copy-for-org-mode)
                        ;; Get the org content from kill ring if something new was added
                        (when (> (length kill-ring) original-kill-ring-length)
                          (setq org-content (car kill-ring)))))
                  (error "Failed to load webpage or timeout reached")))
            ;; Clean up the eww temporary buffer
            (when (buffer-live-p temp-eww-buffer)
              (kill-buffer temp-eww-buffer)))))
      
      ;; Convert org content to plain text using org export
      (when org-content
        (let ((temp-org-buffer (generate-new-buffer "*org-temp*")))
          (unwind-protect
              (with-current-buffer temp-org-buffer
                ;; Insert the org content
                (insert org-content)
                ;; Enable org-mode
                (org-mode)
                ;; Export to ASCII/plain text
                (let ((exported-content (org-export-as 'ascii nil nil t)))
                  (setq plain-content exported-content)))
            ;; Clean up the org temporary buffer  
            (when (buffer-live-p temp-org-buffer)
              (kill-buffer temp-org-buffer))))))
    plain-content))

(defun smv/fetch-content (url)
  "Robust web page fetcher that tries eww+org-export first, then falls back to direct HTTP."
  (or (ignore-errors (fetch-web-page-content url))
      (error "Failed to fetch content from %s" url)))

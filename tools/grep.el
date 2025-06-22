(defun smv-tool/grep (pattern &optional directory)
  "Grep for PATTERN in DIRECTORY (defaults to current project root or default-directory).
Uses ripgrep if available, otherwise falls back to grep.
Returns the grep results as a string."
  (interactive 
   (list (read-string "Pattern to search: " 
                      (when (use-region-p) 
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (when current-prefix-arg
           (read-directory-name "Directory to search: "))))
  
  (let* ((search-dir (or directory
                         (and (fboundp 'project-current)
                              (project-current)
                              (fboundp 'project-root)
                              (project-root (project-current)))
                         (and (fboundp 'projectile-project-root)
                              (projectile-project-root))
                         default-directory))
         (search-dir-safe (or search-dir default-directory))
         (default-directory search-dir-safe)
         (cmd (if (executable-find "rg")
                  (format "rg --color=never --line-number --no-heading --smart-case %s %s"
                          (shell-quote-argument pattern)
                          (shell-quote-argument "."))
                (format "grep -rn --color=never %s %s"
                        (shell-quote-argument pattern)
                        (shell-quote-argument ".")))))
    
    (message "Searching for '%s' in %s..." pattern search-dir-safe)
    (let ((result (shell-command-to-string cmd)))
      (if (string-empty-p (string-trim result))
          (progn
            (message "No matches found for '%s'" pattern)
            nil)
        (progn
          (message "Found matches for '%s'" pattern)
          result)))))

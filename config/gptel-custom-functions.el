;; loads presets
(load-file (format "%s%s/%s%s" user-emacs-directory "presets" "command_line" ".el"))
(load-file (format "%s%s/%s%s" user-emacs-directory "presets" "repomap" ".el"))
(load-file (format "%s%s/%s%s" user-emacs-directory "presets" "reviewer" ".el"))
(load-file (format "%s%s/%s%s" user-emacs-directory "presets" "task_summarizer" ".el"))

;; load functions
(load-file (format "%s%s/%s%s" user-emacs-directory "config" "gptel-refusal-posthook" ".el"))
(load-file (format "%s%s/%s%s" user-emacs-directory "config" "gptel-notool-minor-mode" ".el"))
(load-file (format "%s%s/%s%s" user-emacs-directory "config" "gptel-ediff-prehook" ".el"))
(load-file (format "%s%s/%s%s" user-emacs-directory "config" "gptel-retry" ".el"))



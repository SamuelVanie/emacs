;;; sl4j-format.el --- Default SLF4J output-format rule -*- lexical-binding: t; -*-

;; Local entry point for the generic rule engine in
;; `ghostel-output-format.el'.  All symbols owned by this configuration use
;; the `smv/' namespace; the filenames remain unchanged so existing
;; `load-file' calls keep working.

(declare-function smv/ghostel-output-format-mode
                  "ghostel-output-format" (&optional argument))

;; Detach either previous, unnamespaced implementation before loading the
;; namespaced engine.  This matters when evaluating this file in an Emacs
;; session where an older version was already active.
(remove-hook 'compilation-start-hook 'sl4j--maybe-wrap-ghostel-process)
(remove-hook 'compilation-start-hook
             'ghostel-output-format--maybe-wrap-process)

(dolist (process (process-list))
  (cond
   ((or (eq (process-filter process)
            'ghostel-output-format--process-filter)
        (eq (process-sentinel process)
            'ghostel-output-format--process-sentinel))
    (when (fboundp 'ghostel-output-format--restore-process)
      (condition-case nil
          (funcall 'ghostel-output-format--restore-process process t)
        (error nil))))
   ((or (eq (process-filter process) 'sl4j--ghostel-process-filter)
        (eq (process-sentinel process) 'sl4j--ghostel-process-sentinel))
    (when (fboundp 'sl4j--restore-process-handlers)
      (condition-case nil
          (funcall 'sl4j--restore-process-handlers process t)
        (error nil))))))

;; Remove the public names introduced by earlier versions.  They are not kept
;; as aliases because doing so would continue to occupy package-like global
;; names outside the `smv/' namespace.
(dolist (symbol '(ghostel-output-format-mode
                  ghostel-output-format-render-json
                  ghostel-output-format-render-line
                  ghostel-output-format-render-template
                  ghostel-output-format-toggle-slf4j-style
                  sl4j-ghostel-color-mode
                  sl4j-ghostel-toggle-display-style))
  (when (fboundp symbol)
    (fmakunbound symbol)))

(dolist (symbol '(ghostel-output-format-mode
                  ghostel-output-format-rules
                  ghostel-output-format-styles
                  sl4j-ghostel-color-mode
                  sl4j-ghostel-display-style))
  (when (boundp symbol)
    (makunbound symbol)))

(setq features (delq 'ghostel-output-format features))
(setq features (delq 'sl4j-format features))

(let ((directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (require 'smv/ghostel-output-format
           (expand-file-name "ghostel-output-format" directory)))

(smv/ghostel-output-format-mode 1)

(provide 'smv/sl4j-format)

;;; sl4j-format.el ends here

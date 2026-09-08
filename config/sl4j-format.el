;;; sl4j-format.el --- Default SLF4J output-format rule -*- lexical-binding: t; -*-

;; Local entry point for the generic rule engine in
;; `ghostel-output-format.el'.  All symbols owned by this configuration use
;; the `smv/' namespace; the filenames remain unchanged so existing
;; `load-file' calls keep working.

(declare-function smv/ghostel-output-format-mode
                  "ghostel-output-format" (&optional argument))

(let ((directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (require 'smv/ghostel-output-format
           (expand-file-name "ghostel-output-format" directory)))

(smv/ghostel-output-format-mode 1)

(provide 'smv/sl4j-format)

;;; sl4j-format.el ends here

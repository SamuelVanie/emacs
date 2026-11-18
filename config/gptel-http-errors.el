;;; gptel-http-errors.el --- Preserve HTTP errors behind proxies -*- lexical-binding: t; -*-

;; Some gptel versions keep the first HTTP status emitted by Curl.  With an
;; HTTPS proxy that can be "200 Connection established", followed by an API
;; error.  Correct the status before gptel decides whether the stream succeeded.
;; This also covers informational responses and redirects, and is harmless
;; when gptel already selects the final response status itself.

(require 'gptel)

(defun smv/gptel-curl--final-http-status (info)
  "Return the final HTTP status and status line in Curl's current buffer.
Use INFO's unique Curl footer to bound the headers.  The header size is
in bytes, so convert it to a buffer position before searching."
  (save-excursion
    (goto-char (point-max))
    (when-let* ((uuid (plist-get info :uuid))
                ((stringp uuid))
                ((re-search-backward
                  (concat "(" (regexp-quote uuid) " \\. \\([0-9]+\\))[ \t\r\n]*\\'")
                  nil t))
                (footer-start (match-beginning 0))
                (header-size (string-to-number (match-string 1)))
                (body-start (byte-to-position (1+ header-size)))
                ((<= body-start footer-start)))
      (goto-char (point-min))
      (let (status)
        (while (re-search-forward
                "^HTTP/[.0-9]+ +\\([0-9][0-9][0-9]\\)[^\r\n]*" body-start t)
          (setq status (list (match-string-no-properties 1)
                             (string-trim (match-string-no-properties 0)))))
        status))))

(defun smv/gptel-curl--restore-final-http-status (process &rest _)
  "Correct PROCESS's final HTTP status before gptel completes its stream."
  (when-let* ((fsm (car (alist-get process gptel--request-alist)))
              (info (gptel-fsm-info fsm))
              (buffer (process-buffer process))
              ((buffer-live-p buffer))
              (status (with-current-buffer buffer
                        (smv/gptel-curl--final-http-status info))))
    (setf (gptel-fsm-info fsm)
          (plist-put (plist-put info :http-status (car status))
                     :status (cadr status)))))

;; Named advice remains idempotent when this configuration is reloaded.
(advice-add 'gptel-curl--stream-cleanup :before
            #'smv/gptel-curl--restore-final-http-status)

(provide 'gptel-http-errors)
;;; gptel-http-errors.el ends here

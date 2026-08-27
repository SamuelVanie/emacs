;;; structured-slf4j-log-mode.el --- Pretty structured SLF4J logs -*- lexical-binding: t; -*-

(require 'compile)
(require 'json)
(require 'seq)
(require 'subr-x)


;;;; Faces

(defface smv/slf4j-error-face
  '((t :inherit error :weight bold))
  "Face for ERROR/FATAL log levels.")

(defface smv/slf4j-warning-face
  '((t :inherit warning :weight bold))
  "Face for WARN log levels.")

(defface smv/slf4j-info-face
  '((t :inherit success :weight bold))
  "Face for INFO log levels.")

(defface smv/slf4j-debug-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for DEBUG log levels.")

(defface smv/slf4j-trace-face
  '((t :inherit shadow))
  "Face for TRACE log levels.")

(defface smv/slf4j-time-face
  '((t :inherit shadow))
  "Face for timestamps.")

(defface smv/slf4j-logger-face
  '((t :inherit font-lock-type-face))
  "Face for logger names.")

(defface smv/slf4j-thread-face
  '((t :inherit font-lock-variable-name-face))
  "Face for thread names.")

(defface smv/slf4j-key-face
  '((t :inherit font-lock-keyword-face))
  "Face for structured log keys.")

(defface smv/slf4j-string-face
  '((t :inherit font-lock-string-face))
  "Face for string values.")

(defface smv/slf4j-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for numeric, boolean and null values.")


;;;; Face helpers

(defun smv/slf4j-log--propertize (string face)
  "Return STRING displayed with FACE.

Both `face' and `font-lock-face' are supplied deliberately.
`font-lock-face' survives Compilation mode fontification, while
`face' also works in buffers where Font Lock is not active."
  (propertize
   string
   'face face
   'font-lock-face face))


(defun smv/slf4j-log--add-face (string beg end face)
  "Apply FACE to BEG..END in STRING."
  (add-text-properties
   beg end
   (list
    'face face
    'font-lock-face face)
   string))


;;;; Basic helpers

(defun smv/slf4j-log--severity-face (severity)
  "Return face appropriate for SEVERITY."
  (pcase (upcase (format "%s" severity))
    ((or "FATAL" "ERROR")
     'smv/slf4j-error-face)

    ((or "WARN" "WARNING")
     'smv/slf4j-warning-face)

    ("INFO"
     'smv/slf4j-info-face)

    ("DEBUG"
     'smv/slf4j-debug-face)

    ("TRACE"
     'smv/slf4j-trace-face)

    (_
     'font-lock-keyword-face)))


(defun smv/slf4j-log--key-name (key)
  "Convert JSON object KEY to a string."
  (cond
   ((symbolp key)
    (symbol-name key))

   ((stringp key)
    key)

   (t
    (format "%s" key))))


(defun smv/slf4j-log--reserved-key-p (key)
  "Return non-nil when KEY is formatted specially."
  (member
   (smv/slf4j-log--key-name key)
   '("time"
     "timestamp"
     "severity"
     "level"
     "logger"
     "thread"
     "message")))


;;;; JSON values

(defun smv/slf4j-log--json-value (value)
  "Convert parsed JSON VALUE to compact readable JSON."
  (cond
   ((stringp value)
    (prin1-to-string value))

   ((numberp value)
    (number-to-string value))

   ((eq value t)
    "true")

   ((eq value :false)
    "false")

   ((eq value :null)
    "null")

   ;; json-parse-string :array-type 'array produces vectors.
   ((vectorp value)
    (concat
     "["
     (mapconcat
      #'smv/slf4j-log--json-value
      (append value nil)
      ", ")
     "]"))

   ;; Nested JSON objects.
   ((listp value)
    (concat
     "{"
     (mapconcat
      (lambda (entry)
        (format
         "\"%s\": %s"
         (smv/slf4j-log--key-name (car entry))
         (smv/slf4j-log--json-value (cdr entry))))
      value
      ", ")
     "}"))

   (t
    (format "%s" value))))


(defun smv/slf4j-log--field-value (value)
  "Convert VALUE to a compact key=value representation."
  (cond
   ((stringp value)
    ;; Quote strings containing whitespace.
    (if (string-match-p "[[:space:]]" value)
        (prin1-to-string value)
      value))

   ((or (vectorp value)
        (listp value))
    (smv/slf4j-log--json-value value))

   (t
    (smv/slf4j-log--json-value value))))


(defun smv/slf4j-log--value-face (value)
  "Return appropriate face for VALUE."
  (cond
   ((or (numberp value)
        (eq value t)
        (eq value :false)
        (eq value :null))
    'smv/slf4j-constant-face)

   (t
    'smv/slf4j-string-face)))


;;;; Individual structured fields

(defun smv/slf4j-log--format-field (key value)
  "Return colored KEY=VALUE."
  (concat
   (smv/slf4j-log--propertize
    (concat (smv/slf4j-log--key-name key) "=")
    'smv/slf4j-key-face)

   (smv/slf4j-log--propertize
    (smv/slf4j-log--field-value value)
    (smv/slf4j-log--value-face value))))


;;;; Message formatting

(defun smv/slf4j-log--format-message (message)
  "Format MESSAGE and highlight embedded key=value expressions."
  (let ((text
         (copy-sequence
          (if (stringp message)
              message
            (smv/slf4j-log--field-value message))))
        (pos 0))

    ;; Examples:
    ;;
    ;; route=/orders
    ;; method=POST
    ;; status=201
    ;; requestId=abc123
    ;;
    (while
        (string-match
         "\\_<\\([[:alnum:]_.-]+\\)=\\([^ ,\t\n]*\\)"
         text
         pos)

      ;; key
      (smv/slf4j-log--add-face
       text
       (match-beginning 1)
       (match-end 1)
       'smv/slf4j-key-face)

      ;; value
      (when (< (match-beginning 2)
               (match-end 2))
        (smv/slf4j-log--add-face
         text
         (match-beginning 2)
         (match-end 2)
         'smv/slf4j-string-face))

      (setq pos (match-end 0)))

    text))


;;;; Complete log record

(defun smv/slf4j-log--format-object (object)
  "Turn structured SLF4J OBJECT into a readable colored log line."
  (let* ((severity
          (or (alist-get 'severity object)
              (alist-get 'level object)))

         (time
          (or (alist-get 'time object)
              (alist-get 'timestamp object)))

         (logger
          (alist-get 'logger object))

         (thread
          (alist-get 'thread object))

         (message
          (alist-get 'message object))

         ;; Everything else is preserved in original JSON order.
         (extra-fields
          (seq-remove
           (lambda (entry)
             (smv/slf4j-log--reserved-key-p
              (car entry)))
           object))

         parts)

    ;; [INFO]
    (when severity
      (push
       (smv/slf4j-log--propertize
        (format
         "[%s]"
         (upcase (format "%s" severity)))
        (smv/slf4j-log--severity-face severity))
       parts))

    ;; timestamp
    (when time
      (push
       (smv/slf4j-log--propertize
        (smv/slf4j-log--field-value time)
        'smv/slf4j-time-face)
       parts))

    ;; logger=...
    (when logger
      (push
       (concat
        (smv/slf4j-log--propertize
         "logger="
         'smv/slf4j-key-face)

        (smv/slf4j-log--propertize
         (smv/slf4j-log--field-value logger)
         'smv/slf4j-logger-face))
       parts))

    ;; thread=...
    (when thread
      (push
       (concat
        (smv/slf4j-log--propertize
         "thread="
         'smv/slf4j-key-face)

        (smv/slf4j-log--propertize
         (smv/slf4j-log--field-value thread)
         'smv/slf4j-thread-face))
       parts))

    ;; message itself
    (when message
      (let ((text
             (if (stringp message)
                 message
               (smv/slf4j-log--field-value message))))

        (unless (string-empty-p text)
          (push
           (smv/slf4j-log--format-message message)
           parts))))

    ;; ALL remaining top-level JSON fields.
    (dolist (entry extra-fields)
      (push
       (smv/slf4j-log--format-field
        (car entry)
        (cdr entry))
       parts))

    (string-join
     (nreverse parts)
     " ")))


;;;; Parsing

(defun smv/slf4j-log--structured-log-p (object)
  "Return non-nil when OBJECT resembles our SLF4J JSON record."
  (and
   (listp object)

   (or (alist-get 'severity object)
       (alist-get 'level object))

   (or (alist-get 'time object)
       (alist-get 'timestamp object))

   (alist-get 'logger object)))


(defun smv/slf4j-log--parse-line (raw)
  "Parse RAW as a structured SLF4J record.

Return the parsed alist or nil."
  (let ((text (string-trim raw)))

    (when
        (and
         (not (string-empty-p text))
         (eq (aref text 0) ?{))

      (condition-case nil
          (let ((object
                 (json-parse-string
                  text
                  :object-type 'alist
                  :array-type 'array
                  :null-object :null
                  :false-object :false)))

            (when
                (smv/slf4j-log--structured-log-p object)
              object))

        (json-parse-error nil)))))


;;;; Rewriting

(defun smv/slf4j-log--replace-line (beg end)
  "Replace structured JSON between BEG and END with formatted text."
  (when (< beg end)

    ;; Don't process lines we have already converted.
    (unless
        (get-text-property
         beg
         'smv/slf4j-log-original)

      (let* ((raw
              (buffer-substring-no-properties
               beg end))

             (object
              (smv/slf4j-log--parse-line raw)))

        (when object

          (let ((rendered
                 (smv/slf4j-log--format-object object))

                (inhibit-read-only t))

            ;; Remember the exact original JSON so disabling the
            ;; minor mode can restore it.
            (when (> (length rendered) 0)
              (put-text-property
               0 1
               'smv/slf4j-log-original
               raw
               rendered))

            (save-excursion
              (goto-char beg)

              (with-silent-modifications
                (delete-region beg end)
                (insert rendered)))

            t))))))


(defun smv/slf4j-log--format-buffer ()
  "Format all structured log lines currently in this buffer."
  (save-restriction
    (widen)

    (save-excursion
      (goto-char (point-min))

      (while (< (point) (point-max))

        (smv/slf4j-log--replace-line
         (line-beginning-position)
         (line-end-position))

        (forward-line 1)))))


;;;; Normal compilation-mode: live streaming

(defun smv/slf4j-log--compilation-filter ()
  "Format newly inserted Compilation mode output."
  (when smv/structured-slf4j-log-mode

    (let ((limit
           (copy-marker (point) t)))

      (unwind-protect
          (save-excursion

            ;; Process output can arrive halfway through a JSON line.
            (goto-char compilation-filter-start)
            (beginning-of-line)

            (let ((line-beg (point)))

              ;; Only process lines terminated by newline.
              (while
                  (search-forward "\n" limit t)

                (smv/slf4j-log--replace-line
                 line-beg
                 (1- (point)))

                (setq line-beg
                      (point)))))

        (set-marker limit nil)))))


;;;; Ghostel support

(defun smv/slf4j-log--running-ghostel-compile-p ()
  "Return non-nil for a live `ghostel-compile' buffer."
  (and
   (eq major-mode 'ghostel-mode)

   (boundp 'ghostel-compile--command)

   (local-variable-p
    'ghostel-compile--command)

   ghostel-compile--command

   (not
    (and
     (boundp 'ghostel-compile--finalized)
     ghostel-compile--finalized))))


(defun smv/slf4j-log--ghostel-finished (buffer _status)
  "Format Ghostel compilation BUFFER after terminal rendering stops."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer

      (when smv/structured-slf4j-log-mode
        (smv/slf4j-log--format-buffer)))))


;;;; Restoring JSON

(defun smv/slf4j-log--restore-buffer ()
  "Restore the original JSON records in the current buffer."
  (let ((inhibit-read-only t))

    (save-restriction
      (widen)

      (save-excursion
        (goto-char (point-min))

        (while (< (point) (point-max))

          (let* ((beg
                  (line-beginning-position))

                 (end
                  (line-end-position))

                 (original
                  (get-text-property
                   beg
                   'smv/slf4j-log-original)))

            (if original

                (progn
                  (goto-char beg)

                  (with-silent-modifications
                    (delete-region beg end)
                    (insert original))

                  (forward-line 1))

              (forward-line 1))))))))


;;;; Backend setup

(defun smv/slf4j-log--remove-backends ()
  "Remove buffer-local hooks installed by this minor mode."
  (remove-hook
   'compilation-filter-hook
   #'smv/slf4j-log--compilation-filter
   t)

  (remove-hook
   'compilation-finish-functions
   #'smv/slf4j-log--ghostel-finished
   t))


(defun smv/slf4j-log--setup-backend (&optional quiet)
  "Set up the correct backend for the current major mode.

When QUIET is non-nil, don't signal an error for unsupported modes."
  (smv/slf4j-log--remove-backends)

  (cond

   ;; Normal compilation-mode, including a finished
   ;; ghostel-compile-view-mode.
   ((derived-mode-p 'compilation-mode)

    ;; Format anything that is already there.
    (smv/slf4j-log--format-buffer)

    ;; Standard compilation buffers can be transformed live.
    (add-hook
     'compilation-filter-hook
     #'smv/slf4j-log--compilation-filter
     90
     t))


   ;; Live ghostel compilation.
   ;;
   ;; Do NOT edit this buffer while Ghostel's VT renderer owns it.
   ;; Ghostel explicitly treats its live buffer as renderer-owned.
   ;;
   ;; Instead install a finish callback.  Ghostel invokes local
   ;; compilation-finish-functions after tearing down the renderer.
   ((smv/slf4j-log--running-ghostel-compile-p)

    (add-hook
     'compilation-finish-functions
     #'smv/slf4j-log--ghostel-finished
     90
     t)

    (unless quiet
      (message
       "SLF4J formatting enabled; Ghostel output will be formatted when this run finishes")))


   (quiet
    nil)


   (t
    (user-error
     "Not a compilation-mode or live ghostel-compile buffer"))))


;;;; Survive Ghostel's mode switch

(defun smv/slf4j-log--after-major-mode-change ()
  "Reconfigure backend after the current buffer changes major mode."
  (when smv/structured-slf4j-log-mode
    (smv/slf4j-log--setup-backend t)))


;; Preserve this hook when Ghostel switches from ghostel-mode to
;; ghostel-compile-view-mode.
(put
 'smv/slf4j-log--after-major-mode-change
 'permanent-local-hook
 t)


;;;; Commands

(defun smv/structured-slf4j-log-refresh ()
  "Reformat structured logs in the current buffer."
  (interactive)

  (unless smv/structured-slf4j-log-mode
    (user-error
     "`smv/structured-slf4j-log-mode' is not enabled"))

  (unless (smv/slf4j-log--running-ghostel-compile-p)
    (smv/slf4j-log--format-buffer)))


(define-minor-mode smv/structured-slf4j-log-mode
  "Pretty structured SLF4J JSON logs.

Enable manually with:

    M-x smv/structured-slf4j-log-mode

In ordinary `compilation-mode', output is transformed live.

In a live `ghostel-compile' buffer, the Ghostel renderer owns the
buffer while the command runs, so transformation happens
automatically when the command finishes.

The mode never enables itself globally."
  :init-value nil
  :lighter " JLog"

  (if smv/structured-slf4j-log-mode

      (progn
        ;; Keep our backend across Ghostel's major-mode transition.
        (add-hook
         'after-change-major-mode-hook
         #'smv/slf4j-log--after-major-mode-change
         nil
         t)

        (smv/slf4j-log--setup-backend))

    ;; Disable.
    (smv/slf4j-log--remove-backends)

    (remove-hook
     'after-change-major-mode-hook
     #'smv/slf4j-log--after-major-mode-change
     t)

    (smv/slf4j-log--restore-buffer)

    (message
     "Structured SLF4J formatting disabled")))


;; `ghostel-compile' switches major mode when it finishes.  Keep the
;; minor-mode state across that switch.
(put
 'smv/structured-slf4j-log-mode
 'permanent-local
 t)


(provide 'structured-slf4j-log-mode)

;;; structured-slf4j-log-mode.el ends here

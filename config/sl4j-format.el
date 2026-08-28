;;; sl4j-format.el --- Format structured SLF4J logs in Ghostel -*- lexical-binding: t; -*-

;; This file transforms matching records immediately before Ghostel's VT parser
;; sees them.  Pretty mode renders a compact log line; JSON mode keeps the
;; original text.  Both styles inject ANSI SGR sequences that Ghostel consumes
;; as faces rather than displaying as text.

(require 'compile)
(require 'json)
(require 'subr-x)

(defvar sl4j-ghostel-color-mode)
(defvar ghostel-compile--command)

(declare-function ghostel--filter "ghostel" (process output))

(defgroup sl4j-format nil
  "Live syntax coloring for structured SLF4J output."
  :group 'tools)

(defcustom sl4j-ghostel-display-style 'pretty
  "How live structured logs are rendered in Ghostel compilations.

`pretty' renders a compact line beginning with severity and timestamp.
`json' preserves the original JSON text and only adds syntax coloring."
  :type '(choice (const :tag "Compact pretty output" pretty)
                 (const :tag "Original colored JSON" json))
  :group 'sl4j-format)

(defconst sl4j--missing (make-symbol "sl4j--missing"))

(defconst sl4j--process-filter-property 'sl4j--original-process-filter)
(defconst sl4j--process-sentinel-property 'sl4j--original-process-sentinel)
(defconst sl4j--process-pending-property 'sl4j--pending-output)
(defconst sl4j--process-line-start-property 'sl4j--at-line-start)

(defun sl4j--sgr (parameters)
  "Return a unibyte ANSI SGR sequence using PARAMETERS."
  (encode-coding-string (concat "\e[" parameters "m") 'us-ascii t))

(defun sl4j--paint (text start finish)
  "Surround TEXT with ANSI SGR START and FINISH sequences."
  (concat (sl4j--sgr start) text (sl4j--sgr finish)))

(defun sl4j--dim (text)
  "Return TEXT with reduced intensity."
  (sl4j--paint text "2" "22"))

(defun sl4j--severity-style (severity)
  "Return the ANSI start sequence parameters for SEVERITY."
  (pcase (upcase (format "%s" severity))
    ((or "FATAL" "ERROR") "1;31")
    ((or "WARN" "WARNING") "1;33")
    ("INFO" "1;32")
    ("DEBUG" "36")
    ("TRACE" "2")
    (_ "1;36")))

(defun sl4j--style-finish (style)
  "Return the selective ANSI reset sequence parameters for STYLE."
  (cond
   ((string-match-p "\\`[12];" style) "22;39")
   ((string= style "2") "22")
   (t "39")))

(defun sl4j--structured-log-object (line)
  "Parse unibyte LINE when it resembles a structured SLF4J log.

Return its JSON object, or nil for ordinary output and unrelated JSON."
  (condition-case nil
      (let* ((text (string-trim
                    (decode-coding-string line 'utf-8-unix t)))
             (object (and (string-prefix-p "{" text)
                          (json-parse-string text)))
             (severity (and (hash-table-p object)
                            (or (gethash "severity" object)
                                (gethash "level" object))))
             (time (and (hash-table-p object)
                        (or (gethash "time" object)
                            (gethash "timestamp" object))))
             (message (and (hash-table-p object)
                           (gethash "message" object sl4j--missing)))
             (logger (and (hash-table-p object)
                          (gethash "logger" object sl4j--missing))))
        ;; Requiring these identifying fields avoids coloring arbitrary JSON
        ;; printed by the application or its build tooling.
        (when (and (hash-table-p object)
                   (stringp severity)
                   (stringp time)
                   (or (not (eq message sl4j--missing))
                       (not (eq logger sl4j--missing))))
          object))
    (error nil)))

(defun sl4j--json-string-end (text start)
  "Return the position after the JSON string in TEXT at START.

Return nil for an unterminated string.  TEXT is deliberately scanned as bytes
so adding color never decodes and re-encodes the original process output."
  (let ((position (1+ start))
        (length (length text))
        done)
    (while (and (< position length) (not done))
      (pcase (aref text position)
        (?\\
         ;; Skip the escaped byte.  For a \uXXXX escape the four hex digits
         ;; contain no quote and can safely be scanned normally afterwards.
         (setq position (+ position 2)))
        (?\"
         (setq position (1+ position)
               done t))
        (_
         (setq position (1+ position)))))
    (and done position)))

(defun sl4j--json-whitespace-p (character)
  "Return non-nil when CHARACTER is JSON whitespace."
  (memq character '(?\s ?\t ?\n ?\r)))

(defun sl4j--json-delimiter-p (character)
  "Return non-nil when CHARACTER terminates an unquoted JSON value."
  (or (sl4j--json-whitespace-p character)
      (memq character '(?, ?: ?\{ ?\} ?\[ ?\]))))

(defun sl4j--style-message-content (content)
  "Color key=value fragments in raw JSON string CONTENT."
  (let ((position 0)
        pieces)
    (while (string-match
            "\\([[:alnum:]_.-]+\\)=\\([^ ,\t]*\\)"
            content position)
      (push (substring content position (match-beginning 0)) pieces)
      (push (sl4j--paint (match-string 1 content) "1;36" "22;39")
            pieces)
      (push (sl4j--dim "=") pieces)
      (when (< (match-beginning 2) (match-end 2))
        (push (sl4j--paint (match-string 2 content) "32" "39") pieces))
      (setq position (match-end 0)))
    (push (substring content position) pieces)
    (apply #'concat (nreverse pieces))))

(defun sl4j--style-quoted-string (token style &optional content-function)
  "Color quoted JSON string TOKEN.

STYLE is the SGR start sequence for its content.  When CONTENT-FUNCTION is
non-nil, call it to produce the styled content instead."
  (let* ((content (substring token 1 -1))
         (styled-content
          (cond
           (content-function (funcall content-function content))
           (style (sl4j--paint content style (sl4j--style-finish style)))
           (t content))))
    (concat (sl4j--dim "\"") styled-content (sl4j--dim "\""))))

(defun sl4j--style-string-value (token key severity)
  "Color JSON string TOKEN according to KEY and SEVERITY."
  (pcase key
    ((or "severity" "level")
     (sl4j--style-quoted-string token (sl4j--severity-style severity)))
    ((or "time" "timestamp")
     (sl4j--style-quoted-string token "2"))
    ("logger"
     (sl4j--style-quoted-string token "35"))
    ("thread"
     (sl4j--style-quoted-string token "34"))
    ("message"
     (sl4j--style-quoted-string token nil #'sl4j--style-message-content))
    (_
     (sl4j--style-quoted-string token "32"))))

(defun sl4j--color-json (line severity)
  "Add ANSI syntax coloring to JSON bytes in LINE.

SEVERITY controls the color of a severity or level value.  Removing the ANSI
sequences from the result always recovers LINE byte-for-byte."
  (let ((position 0)
        (length (length line))
        current-key
        pieces)
    (while (< position length)
      (let ((character (aref line position)))
        (cond
         ((eq character ?\")
          (let ((end (sl4j--json-string-end line position)))
            (if (not end)
                (progn
                  (push (substring line position) pieces)
                  (setq position length))
              (let ((after end))
                (while (and (< after length)
                            (sl4j--json-whitespace-p (aref line after)))
                  (setq after (1+ after)))
                (let* ((token (substring line position end))
                       (key-p (and (< after length)
                                   (eq (aref line after) ?:))))
                  (if key-p
                      (progn
                        ;; SLF4J's standard keys are ASCII.  Keep escaped or
                        ;; nonstandard keys readable as generic JSON keys.
                        (setq current-key (substring token 1 -1))
                        (push (sl4j--style-quoted-string token "1;36") pieces))
                    (push (sl4j--style-string-value
                           token current-key severity)
                          pieces)
                    (setq current-key nil)))
                (setq position end)))))

         ((memq character '(?\{ ?\} ?\[ ?\] ?, ?:))
          (push (sl4j--dim (substring line position (1+ position))) pieces)
          (when (memq character '(?\{ ?\[ ?, ?\} ?\]))
            (setq current-key nil))
          (setq position (1+ position)))

         ((sl4j--json-whitespace-p character)
          (let ((end (1+ position)))
            (while (and (< end length)
                        (sl4j--json-whitespace-p (aref line end)))
              (setq end (1+ end)))
            (push (substring line position end) pieces)
            (setq position end)))

         (t
          (let ((end (1+ position)))
            (while (and (< end length)
                        (not (sl4j--json-delimiter-p (aref line end))))
              (setq end (1+ end)))
            (push (sl4j--paint (substring line position end) "33" "39")
                  pieces)
            (setq current-key nil
                  position end))))))
    (apply #'concat (nreverse pieces))))

(defun sl4j--json-value (value)
  "Serialize parsed JSON VALUE compactly."
  (condition-case nil
      (json-serialize value :null-object :null :false-object :false)
    (error (format "%s" value))))

(defun sl4j--single-line-string (value)
  "Return string VALUE with JSON control characters escaped, without quotes."
  (let ((serialized (sl4j--json-value value)))
    (if (and (>= (length serialized) 2)
             (eq (aref serialized 0) ?\")
             (eq (aref serialized (1- (length serialized))) ?\"))
        (substring serialized 1 -1)
      serialized)))

(defun sl4j--pretty-value (value)
  "Return parsed JSON VALUE in a compact, unambiguous field representation."
  (if (stringp value)
      (let ((escaped (sl4j--single-line-string value)))
        (if (or (string-empty-p value)
                (string-match-p "[[:space:]]" value))
            (concat "\"" escaped "\"")
          escaped))
    (sl4j--json-value value)))

(defun sl4j--constant-value-p (value)
  "Return non-nil when VALUE is a JSON number, boolean, or null."
  (or (numberp value)
      (eq value t)
      (eq value :false)
      (eq value :null)))

(defun sl4j--pretty-field (key value &optional value-style)
  "Return a colored KEY=VALUE field.

VALUE-STYLE, when non-nil, overrides the inferred ANSI color."
  (concat
   (sl4j--paint (concat key "=") "1;36" "22;39")
   (let ((style (or value-style
                    (if (sl4j--constant-value-p value) "33" "32"))))
     (sl4j--paint (sl4j--pretty-value value)
                  style (sl4j--style-finish style)))))

(defun sl4j--reserved-field-p (key)
  "Return non-nil when JSON object KEY has a fixed position in pretty output."
  (member key '("severity" "level" "time" "timestamp"
                "logger" "thread" "message")))

(defun sl4j--pretty-record (object)
  "Render parsed SLF4J JSON OBJECT as one colored compact terminal line."
  (let* ((severity (or (gethash "severity" object)
                       (gethash "level" object)))
         (time (or (gethash "time" object)
                   (gethash "timestamp" object)))
         (logger (gethash "logger" object sl4j--missing))
         (thread (gethash "thread" object sl4j--missing))
         (message (gethash "message" object sl4j--missing))
         parts
         extras)
    (let ((style (sl4j--severity-style severity)))
      (push (sl4j--paint (format "[%s]" (upcase severity))
                         style (sl4j--style-finish style))
            parts))
    (push (sl4j--paint (sl4j--pretty-value time) "2" "22") parts)
    (unless (eq logger sl4j--missing)
      (push (sl4j--pretty-field "logger" logger "35") parts))
    (unless (eq thread sl4j--missing)
      (push (sl4j--pretty-field "thread" thread "34") parts))
    (unless (eq message sl4j--missing)
      (let ((text (if (stringp message)
                      (sl4j--single-line-string message)
                    (sl4j--pretty-value message))))
        (unless (string-empty-p text)
          (push (if (stringp message)
                    (sl4j--style-message-content text)
                  text)
                parts))))
    ;; Preserve additional structured fields after the standard log content.
    (maphash (lambda (key value)
               (unless (sl4j--reserved-field-p key)
                 (push (sl4j--pretty-field key value) extras)))
             object)
    (setq parts (nconc (nreverse parts) (nreverse extras)))
    ;; Parsed JSON strings are multibyte Emacs strings.  Ghostel's process
    ;; filter receives binary data, so encode the generated line explicitly.
    (encode-coding-string (string-join parts " ") 'utf-8-unix t)))

(defun sl4j--render-record (line)
  "Render structured log LINE according to `sl4j-ghostel-display-style'."
  (let ((object (sl4j--structured-log-object line)))
    (if (not object)
        line
      (pcase sl4j-ghostel-display-style
        ('pretty (sl4j--pretty-record object))
        (_ (sl4j--color-json
            line
            (or (gethash "severity" object)
                (gethash "level" object))))))))

(defun sl4j--possible-json-prefix-p (text)
  "Return non-nil when unterminated TEXT may begin a JSON record."
  (let ((trimmed (string-trim-left text)))
    (or (string-empty-p trimmed)
        (eq (aref trimmed 0) ?\{))))

(defun sl4j--transform-process-output (process output)
  "Color complete log lines in PROCESS OUTPUT.

Process output can split a JSON record at any byte.  A possible record is held
until LF, CRLF, or CR arrives; ordinary partial output is forwarded without
waiting for a newline."
  (let* ((pending (or (process-get process sl4j--process-pending-property) ""))
         (at-line-start
          (if (string-empty-p pending)
              (process-get process sl4j--process-line-start-property)
            t))
         (data (concat pending output))
         (position 0)
         pieces)
    (process-put process sl4j--process-pending-property "")
    (while (string-match "\r\n\\|\n\\|\r" data position)
      ;; Save both bounds before coloring the line: the JSON and message
      ;; scanners legitimately replace the global regexp match data.
      (let* ((separator-start (match-beginning 0))
             (separator-end (match-end 0))
             (line (substring data position separator-start))
             (separator (substring data separator-start separator-end)))
        (push (if at-line-start (sl4j--render-record line) line) pieces)
        (push separator pieces)
        (setq at-line-start t
              position separator-end)))
    (let ((tail (substring data position)))
      (cond
       ((string-empty-p tail))
       ((and at-line-start (sl4j--possible-json-prefix-p tail))
        (process-put process sl4j--process-pending-property tail))
       (t
        (push tail pieces)
        (setq at-line-start nil))))
    (process-put process sl4j--process-line-start-property at-line-start)
    (apply #'concat (nreverse pieces))))

(defun sl4j--ghostel-process-filter (process output)
  "Color structured logs from PROCESS, then delegate OUTPUT to Ghostel."
  (let* ((original (process-get process sl4j--process-filter-property))
         (pending (or (process-get process sl4j--process-pending-property) ""))
         (transformed
          (condition-case error-data
              (sl4j--transform-process-output process output)
            (error
             ;; A colorizer failure must never swallow or duplicate build
             ;; output.  The pending bytes have not yet reached Ghostel.
             (process-put process sl4j--process-pending-property "")
             (process-put process sl4j--process-line-start-property nil)
             (message "SLF4J colorizer skipped malformed output: %s"
                      (error-message-string error-data))
             (concat pending output)))))
    (when (and original (not (string-empty-p transformed)))
      (funcall original process transformed))))

(defun sl4j--flush-pending (process &optional color)
  "Send PROCESS's pending bytes to its original filter.

When COLOR is non-nil, color a final log record that had no line terminator."
  (let ((pending (or (process-get process sl4j--process-pending-property) ""))
        (original (process-get process sl4j--process-filter-property)))
    (process-put process sl4j--process-pending-property "")
    (when (and original (not (string-empty-p pending)))
      (funcall original process (if color (sl4j--render-record pending) pending)))))

(defun sl4j--restore-process-handlers (process &optional flush)
  "Restore Ghostel handlers previously wrapped on PROCESS.

If FLUSH is non-nil, forward pending output without adding colors first."
  (let ((original-filter
         (process-get process sl4j--process-filter-property))
        (original-sentinel
         (process-get process sl4j--process-sentinel-property)))
    (when flush
      (condition-case nil
          (sl4j--flush-pending process)
        (error nil)))
    (when (and original-filter
               (eq (process-filter process) #'sl4j--ghostel-process-filter))
      (set-process-filter process original-filter))
    (when (and original-sentinel
               (eq (process-sentinel process) #'sl4j--ghostel-process-sentinel))
      (set-process-sentinel process original-sentinel))
    (process-put process sl4j--process-filter-property nil)
    (process-put process sl4j--process-sentinel-property nil)
    (process-put process sl4j--process-pending-property nil)
    (process-put process sl4j--process-line-start-property nil)))

(defun sl4j--ghostel-process-sentinel (process event)
  "Flush PROCESS and delegate EVENT to Ghostel's original sentinel."
  (let ((original (process-get process sl4j--process-sentinel-property)))
    (if (not (memq (process-status process) '(exit signal)))
        (when original (funcall original process event))
      (unwind-protect
          (condition-case nil
              (sl4j--flush-pending process t)
            (error nil))
        (sl4j--restore-process-handlers process)
        (when original
          (funcall original process event))))))

(defun sl4j--ghostel-compilation-process-p (process)
  "Return non-nil when PROCESS belongs to a live Ghostel compilation."
  (let ((buffer (and (processp process) (process-buffer process))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (derived-mode-p 'ghostel-mode)
                (local-variable-p 'ghostel-compile--command)
                ghostel-compile--command)))))

(defun sl4j--maybe-wrap-ghostel-process (process)
  "Install live SLF4J coloring on a Ghostel compilation PROCESS."
  (when (and sl4j-ghostel-color-mode
             (sl4j--ghostel-compilation-process-p process)
             (eq (process-filter process) #'ghostel--filter))
    (process-put process sl4j--process-filter-property
                 (process-filter process))
    (process-put process sl4j--process-sentinel-property
                 (process-sentinel process))
    (process-put process sl4j--process-pending-property "")
    (process-put process sl4j--process-line-start-property t)
    (set-process-filter process #'sl4j--ghostel-process-filter)
    (set-process-sentinel process #'sl4j--ghostel-process-sentinel)))

(defun sl4j--wrap-running-ghostel-processes ()
  "Install coloring on Ghostel compilations that are already running."
  (dolist (process (process-list))
    (when (process-live-p process)
      (sl4j--maybe-wrap-ghostel-process process))))

;;;###autoload
(defun sl4j-ghostel-toggle-display-style ()
  "Toggle future log records between compact and colored-JSON display."
  (interactive)
  (setq sl4j-ghostel-display-style
        (if (eq sl4j-ghostel-display-style 'pretty) 'json 'pretty))
  (message "SLF4J Ghostel display style: %s (applies to new output)"
           sl4j-ghostel-display-style))

;;;###autoload
(define-minor-mode sl4j-ghostel-color-mode
  "Render structured SLF4J JSON records in live Ghostel compilations.

The rendering is controlled by `sl4j-ghostel-display-style'.  Terminal color
control sequences are consumed by Ghostel rather than displayed.  Non-log
output passes through verbatim."
  :global t
  :group 'sl4j-format
  (if sl4j-ghostel-color-mode
      (progn
        (add-hook 'compilation-start-hook #'sl4j--maybe-wrap-ghostel-process)
        (sl4j--wrap-running-ghostel-processes))
    (remove-hook 'compilation-start-hook #'sl4j--maybe-wrap-ghostel-process)
    (dolist (process (process-list))
      (when (or (eq (process-filter process) #'sl4j--ghostel-process-filter)
                (eq (process-sentinel process) #'sl4j--ghostel-process-sentinel))
        (sl4j--restore-process-handlers process t)))))

;; `custom-functions.el' loads this file for its side effect.  Enable the
;; narrowly-scoped integration by default; users can toggle it with M-x
;; `sl4j-ghostel-color-mode'.
(sl4j-ghostel-color-mode 1)

(provide 'sl4j-format)

;;; sl4j-format.el ends here

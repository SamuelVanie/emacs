;;; ghostel-output-format.el --- Configurable live compilation formatting -*- lexical-binding: t; -*-

;; Ordered, line-oriented formatting rules shared by Ghostel compilation
;; buffers and ordinary `compilation-mode' processes.

(require 'ansi-color)
(require 'cl-lib)
(require 'compile)
(require 'json)
(require 'subr-x)

(defvar smv/ghostel-output-format-mode)
(defvar ghostel-compile--command)

(declare-function ghostel--filter "ghostel" (process output))

(defgroup smv/ghostel-output-format nil
  "Configurable live formatting for compilation output."
  :group 'tools)

(defcustom smv/sl4j-ghostel-display-style 'pretty
  "How the built-in SLF4J rule renders matching records.

`pretty' uses the rule's configurable output segments.  `json' retains the
original JSON and adds syntax coloring."
  :type '(choice (const :tag "Compact pretty output" pretty)
                 (const :tag "Original colored JSON" json))
  :group 'smv/ghostel-output-format)

(defcustom smv/ghostel-output-format-styles
  '((key . ("1;36" . "22;39"))
    (dim . ("2" . "22"))
    (logger . ("35" . "39"))
    (thread . ("34" . "39"))
    (string . ("32" . "39"))
    (constant . ("33" . "39"))
    (severity . smv/ghostel-output-format--severity-style)
    (message . smv/ghostel-output-format--message-style))
  "Named styles usable in output segments.

Each value is either (START . FINISH), containing ANSI SGR parameters, or a
function called with the rendered text and original field value.  The function
must return styled text."
  :type 'sexp
  :group 'smv/ghostel-output-format)

(defcustom smv/ghostel-output-format-rules
  '((:name slf4j
     :input (:type json
             :required (time severity)
             :any (logger message)
             :fields ((time "time" "timestamp")
                      (severity "severity" "level")
                      (logger "logger")
                      (thread "thread")
                      (message "message")))
     :output ((:field severity
               :prefix "[" :suffix "]"
               :transform upcase
               :style severity
               :prefix-style severity
               :suffix-style severity)
              " "
              (:field time :style dim)
              (:field logger :prefix " logger="
               :prefix-style key :style logger)
              (:field thread :prefix " thread="
               :prefix-style key :style thread)
              (:field message :prefix " message="
               :prefix-style key :style message :omit-empty t))
     :append-extra-fields t
     :renderer smv/ghostel-output-format--slf4j-renderer))
  "Ordered rules for transforming complete compilation output lines.

The first matching rule wins.  Unmatched lines pass through unchanged.

A JSON input has the form:

  (:type json
   :required (time severity)
   :fields ((time \"time\" \"timestamp\")
            (severity \"severity\" \"level\")))

Each entry in `:fields' maps a canonical field symbol to one or more JSON keys.
`:required' requires every listed canonical field.  Optional `:any' requires at
least one of its fields.

A regexp input has the form:

  (:type regexp
   :regexp \"^\\\\([^ ]+\\\\) \\\\([A-Z]+\\\\) \\\\(.*\\\\)$\"
   :fields ((time . 1) (severity . 2) (message . 3)))

Regexp rules buffer an unterminated line by default.  Set `:prefix-regexp' in
the input to let clearly unrelated partial output pass through immediately.

An output is a list of literal strings and field segments.  A segment supports
`:prefix', `:suffix', `:transform', `:formatter', `:style', `:prefix-style',
`:suffix-style', and `:omit-empty'.  Missing fields omit the entire segment.

For full control, set rule-level `:parser' and/or `:renderer' functions.  A
parser is called with decoded LINE and RULE and may return nil, a field alist,
a hash table, a `smv/ghostel-output-format-record', or a plist containing
`:fields', `:extras', and `:source'.  A renderer is called with RECORD, RULE,
and decoded LINE and must return a string."
  :type 'sexp
  :group 'smv/ghostel-output-format)

(cl-defstruct
    (smv/ghostel-output-format-record
     (:constructor smv/ghostel-output-format-record-create))
  "The normalized result of matching one output rule."
  fields
  extras
  source)

(defconst smv/ghostel-output-format--missing
  (make-symbol "smv/ghostel-output-format--missing"))

(defconst smv/ghostel-output-format--filter-property
  'smv/ghostel-output-format--original-filter)
(defconst smv/ghostel-output-format--sentinel-property
  'smv/ghostel-output-format--original-sentinel)
(defconst smv/ghostel-output-format--backend-property
  'smv/ghostel-output-format--backend)
(defconst smv/ghostel-output-format--pending-property
  'smv/ghostel-output-format--pending)
(defconst smv/ghostel-output-format--line-start-property
  'smv/ghostel-output-format--at-line-start)


;;;; Styling

(defun smv/ghostel-output-format--sgr (parameters)
  "Return an ANSI SGR sequence using PARAMETERS."
  (string-to-multibyte (concat "\e[" parameters "m")))

(defun smv/ghostel-output-format--paint (text start finish)
  "Surround TEXT with ANSI SGR START and FINISH sequences."
  (concat (smv/ghostel-output-format--sgr start)
          text
          (smv/ghostel-output-format--sgr finish)))

(defun smv/ghostel-output-format--severity-codes (severity)
  "Return ANSI start and finish parameters for SEVERITY."
  (let ((start
         (pcase (upcase (format "%s" severity))
           ((or "FATAL" "ERROR") "1;31")
           ((or "WARN" "WARNING") "1;33")
           ("INFO" "1;32")
           ("DEBUG" "36")
           ("TRACE" "2")
           (_ "1;36"))))
    (cons start
          (cond
           ((string-match-p "\\`[12];" start) "22;39")
           ((string= start "2") "22")
           (t "39")))))

(defun smv/ghostel-output-format--severity-style (text value)
  "Apply a log-severity style to TEXT according to VALUE."
  (let ((codes (smv/ghostel-output-format--severity-codes value)))
    (smv/ghostel-output-format--paint text (car codes) (cdr codes))))

(defun smv/ghostel-output-format--message-style (text _value)
  "Highlight key=value fragments inside message TEXT."
  (let ((position 0)
        pieces)
    (while (string-match
            "\\([[:alnum:]_.-]+\\)=\\([^ ,\t]*\\)"
            text position)
      (push (substring text position (match-beginning 0)) pieces)
      (push (smv/ghostel-output-format--apply-style
             'key (match-string 1 text) (match-string 1 text))
            pieces)
      (push (smv/ghostel-output-format--apply-style 'dim "=" "=") pieces)
      (when (< (match-beginning 2) (match-end 2))
        (push (smv/ghostel-output-format--apply-style
               'string (match-string 2 text) (match-string 2 text))
              pieces))
      (setq position (match-end 0)))
    (push (substring text position) pieces)
    (apply #'concat (nreverse pieces))))

(defun smv/ghostel-output-format--apply-style (style text value)
  "Apply STYLE to TEXT, using VALUE for value-sensitive styles."
  (if (null style)
      text
    (let ((definition
           (if (symbolp style)
               (alist-get style smv/ghostel-output-format-styles)
             style)))
      (cond
       ((and (consp definition)
             (stringp (car definition))
             (stringp (cdr definition)))
        (smv/ghostel-output-format--paint
         text (car definition) (cdr definition)))
       ((functionp definition)
        (funcall definition text value))
       ((and (symbolp style) (functionp style))
        (funcall style text value))
       ((stringp definition)
        (smv/ghostel-output-format--paint text definition "0"))
       (t
        (error "Unknown output style: %S" style))))))


;;;; Values and records

(defun smv/ghostel-output-format--json-value (value)
  "Serialize parsed JSON VALUE compactly."
  (condition-case nil
      (let ((serialized
             (json-serialize value :null-object :null :false-object :false)))
        ;; The serializer may return unibyte UTF-8.  Template renderers use
        ;; ordinary multibyte Emacs text until a process backend encodes it.
        (if (multibyte-string-p serialized)
            serialized
          (decode-coding-string serialized 'utf-8-unix t)))
    (error (format "%s" value))))

(defun smv/ghostel-output-format--single-line-string (value)
  "Return string VALUE with JSON control characters escaped, without quotes."
  (let ((serialized (smv/ghostel-output-format--json-value value)))
    (if (and (>= (length serialized) 2)
             (eq (aref serialized 0) ?\")
             (eq (aref serialized (1- (length serialized))) ?\"))
        (substring serialized 1 -1)
      serialized)))

(defun smv/ghostel-output-format--display-value (value)
  "Render VALUE for direct interpolation in an output template."
  (if (stringp value)
      (smv/ghostel-output-format--single-line-string value)
    (smv/ghostel-output-format--json-value value)))

(defun smv/ghostel-output-format--extra-value (value)
  "Render VALUE in an appended key=value field."
  (if (stringp value)
      (let ((escaped (smv/ghostel-output-format--single-line-string value)))
        (if (or (string-empty-p value)
                (string-match-p "[[:space:]]" value))
            (concat "\"" escaped "\"")
          escaped))
    (smv/ghostel-output-format--json-value value)))

(defun smv/ghostel-output-format--constant-value-p (value)
  "Return non-nil when VALUE is a JSON-like scalar constant."
  (or (numberp value)
      (eq value t)
      (eq value :false)
      (eq value :null)))

(defun smv/ghostel-output-format--field-key (field)
  "Normalize FIELD to the canonical key used in a record."
  (if (stringp field) (intern field) field))

(defun smv/ghostel-output-format--field-cell (record field)
  "Return RECORD's cons cell for FIELD, or nil when it is absent."
  (let* ((key (smv/ghostel-output-format--field-key field))
         (fields (smv/ghostel-output-format-record-fields record)))
    (or (assq key fields)
        (and (symbolp key) (assoc (symbol-name key) fields)))))

(defun smv/ghostel-output-format--fields-alist (value)
  "Normalize field collection VALUE to an alist."
  (cond
   ((null value) nil)
   ((hash-table-p value)
    (let (result)
      (maphash (lambda (key item) (push (cons key item) result)) value)
      (nreverse result)))
   ((listp value) value)
   (t (error "Fields must be an alist or hash table: %S" value))))

(defun smv/ghostel-output-format--normalize-record (value)
  "Normalize custom parser VALUE to a formatter record."
  (cond
   ((null value) nil)
   ((smv/ghostel-output-format-record-p value) value)
   ((hash-table-p value)
    (smv/ghostel-output-format-record-create
     :fields (smv/ghostel-output-format--fields-alist value)
     :source value))
   ((and (listp value) (keywordp (car value)))
    (smv/ghostel-output-format-record-create
     :fields (smv/ghostel-output-format--fields-alist (plist-get value :fields))
     :extras (smv/ghostel-output-format--fields-alist (plist-get value :extras))
     :source (plist-get value :source)))
   ((listp value)
    (smv/ghostel-output-format-record-create :fields value :source value))
   (t
    (error "Parser returned an unsupported record: %S" value))))

(defun smv/ghostel-output-format--record-matches-p (record input)
  "Return non-nil when RECORD satisfies INPUT requirements."
  (and
   (cl-every (lambda (field)
               (smv/ghostel-output-format--field-cell record field))
             (plist-get input :required))
   (let ((any (plist-get input :any)))
     (or (null any)
         (cl-some (lambda (field)
                    (smv/ghostel-output-format--field-cell record field))
                  any)))))


;;;; Rule parsing

(defun smv/ghostel-output-format--parse-json (line input)
  "Parse JSON LINE according to INPUT and return a normalized record."
  (let* ((trimmed (string-trim line))
         (object (and (string-prefix-p "{" trimmed)
                      (condition-case nil
                          (json-parse-string trimmed)
                        (error nil)))))
    (when (hash-table-p object)
      (let ((mapping (plist-get input :fields))
            fields
            consumed
            extras)
        (if mapping
            (dolist (entry mapping)
              (let ((canonical (car entry))
                    (source-keys (cdr entry))
                    (value smv/ghostel-output-format--missing))
                (dolist (source-key source-keys)
                  (push source-key consumed)
                  (when (eq value smv/ghostel-output-format--missing)
                    (let ((candidate
                           (gethash source-key object
                                    smv/ghostel-output-format--missing)))
                      (unless (eq candidate smv/ghostel-output-format--missing)
                        (setq value candidate)))))
                (unless (eq value smv/ghostel-output-format--missing)
                  (push (cons canonical value) fields))))
          (maphash (lambda (key value)
                     (push (cons key value) fields))
                   object))
        (when mapping
          (maphash (lambda (key value)
                     (unless (member key consumed)
                       (push (cons key value) extras)))
                   object))
        (let ((record
               (smv/ghostel-output-format-record-create
                :fields (nreverse fields)
                :extras (nreverse extras)
                :source object)))
          (and (smv/ghostel-output-format--record-matches-p record input)
               record))))))

(defun smv/ghostel-output-format--regexp-group (entry)
  "Return the regexp group number described by field mapping ENTRY."
  (if (integerp (cdr entry)) (cdr entry) (cadr entry)))

(defun smv/ghostel-output-format--parse-regexp (line input)
  "Parse LINE using regexp INPUT and return a normalized record."
  (save-match-data
    (when (string-match (or (plist-get input :regexp)
                            (error "Regexp input is missing :regexp"))
                        line)
      (let (fields)
        (dolist (entry (plist-get input :fields))
          (let ((value (match-string
                        (smv/ghostel-output-format--regexp-group entry) line)))
            (when value
              (push (cons (car entry) value) fields))))
        (let ((record
               (smv/ghostel-output-format-record-create
                :fields (nreverse fields)
                :source line)))
          (and (smv/ghostel-output-format--record-matches-p record input)
               record))))))

(defun smv/ghostel-output-format--parse-rule (line rule)
  "Try to parse LINE with RULE and return a normalized record."
  (let* ((input (plist-get rule :input))
         (parser (or (plist-get rule :parser)
                     (and (eq (plist-get input :type) 'function)
                          (plist-get input :function)))))
    (if parser
        (let ((record
               (smv/ghostel-output-format--normalize-record
                (funcall parser line rule))))
          (and record
               (smv/ghostel-output-format--record-matches-p record input)
               record))
      (pcase (plist-get input :type)
        ('json (smv/ghostel-output-format--parse-json line input))
        ('regexp (smv/ghostel-output-format--parse-regexp line input))
        (_ (error "Unknown input type in rule %S" (plist-get rule :name)))))))


;;;; Template rendering

(defun smv/ghostel-output-format--transform-value (value transform)
  "Apply optional TRANSFORM to VALUE."
  (if transform (funcall transform value) value))

(defun smv/ghostel-output-format--format-segment-value (value formatter)
  "Render VALUE with optional FORMATTER."
  (if formatter
      (funcall formatter value)
    (smv/ghostel-output-format--display-value value)))

(defun smv/ghostel-output-format--render-segment (segment record)
  "Render one field SEGMENT from RECORD, or nil when it is absent."
  (let ((cell (smv/ghostel-output-format--field-cell
               record (plist-get segment :field))))
    (when cell
      (let* ((original (cdr cell))
             (value (smv/ghostel-output-format--transform-value
                     original (plist-get segment :transform)))
             (text (smv/ghostel-output-format--format-segment-value
                    value (plist-get segment :formatter))))
        (unless (and (plist-get segment :omit-empty)
                     (string-empty-p text))
          (concat
           (smv/ghostel-output-format--apply-style
            (plist-get segment :prefix-style)
            (or (plist-get segment :prefix) "") value)
           (smv/ghostel-output-format--apply-style
            (plist-get segment :style) text value)
           (smv/ghostel-output-format--apply-style
            (plist-get segment :suffix-style)
            (or (plist-get segment :suffix) "") value)))))))

(defun smv/ghostel-output-format--render-extra (entry)
  "Render extra field ENTRY as a colored key=value string."
  (let* ((key (format "%s" (car entry)))
         (value (cdr entry))
         (value-style
          (if (smv/ghostel-output-format--constant-value-p value)
              'constant
            'string)))
    (concat
     (smv/ghostel-output-format--apply-style 'key (concat key "=") key)
     (smv/ghostel-output-format--apply-style
      value-style (smv/ghostel-output-format--extra-value value) value))))

(defun smv/ghostel-output-format-render-template (record rule)
  "Render RECORD using RULE's declarative `:output' segments."
  (let (pieces)
    (dolist (segment (plist-get rule :output))
      (cond
       ((stringp segment) (push segment pieces))
       ((and (listp segment) (plist-member segment :field))
        (let ((rendered
               (smv/ghostel-output-format--render-segment segment record)))
          (when rendered (push rendered pieces))))
       (t
        (error "Invalid output segment in rule %S: %S"
               (plist-get rule :name) segment))))
    (setq pieces (nreverse pieces))
    (when (plist-get rule :append-extra-fields)
      (dolist (entry (smv/ghostel-output-format-record-extras record))
        (when pieces (setq pieces (append pieces (list " "))))
        (setq pieces
              (append pieces
                      (list (smv/ghostel-output-format--render-extra entry))))))
    (apply #'concat pieces)))


;;;; Colored JSON renderer

(defun smv/ghostel-output-format--json-string-end (text start)
  "Return the position after the JSON string in TEXT at START."
  (let ((position (1+ start))
        (length (length text))
        done)
    (while (and (< position length) (not done))
      (pcase (aref text position)
        (?\\ (setq position (+ position 2)))
        (?\" (setq position (1+ position) done t))
        (_ (setq position (1+ position)))))
    (and done position)))

(defun smv/ghostel-output-format--json-whitespace-p (character)
  "Return non-nil when CHARACTER is JSON whitespace."
  (memq character '(?\s ?\t ?\n ?\r)))

(defun smv/ghostel-output-format--json-delimiter-p (character)
  "Return non-nil when CHARACTER terminates an unquoted JSON value."
  (or (smv/ghostel-output-format--json-whitespace-p character)
      (memq character '(?, ?: ?\{ ?\} ?\[ ?\]))))

(defun smv/ghostel-output-format--style-json-string (token style &optional function)
  "Color quoted JSON TOKEN using STYLE or content FUNCTION."
  (let* ((content (substring token 1 -1))
         (styled (if function
                     (funcall function content content)
                   (smv/ghostel-output-format--apply-style style content content))))
    (concat (smv/ghostel-output-format--apply-style 'dim "\"" "\"")
            styled
            (smv/ghostel-output-format--apply-style 'dim "\"" "\""))))

(defun smv/ghostel-output-format--style-json-value (token key severity)
  "Color JSON string TOKEN according to KEY and SEVERITY."
  (pcase key
    ((or "severity" "level")
     (let ((content (substring token 1 -1)))
       (concat
        (smv/ghostel-output-format--apply-style 'dim "\"" "\"")
        (smv/ghostel-output-format--apply-style 'severity content severity)
        (smv/ghostel-output-format--apply-style 'dim "\"" "\""))))
    ((or "time" "timestamp")
     (smv/ghostel-output-format--style-json-string token 'dim))
    ("logger" (smv/ghostel-output-format--style-json-string token 'logger))
    ("thread" (smv/ghostel-output-format--style-json-string token 'thread))
    ("message"
     (smv/ghostel-output-format--style-json-string
      token nil #'smv/ghostel-output-format--message-style))
    (_ (smv/ghostel-output-format--style-json-string token 'string))))

(defun smv/ghostel-output-format--color-json (line severity)
  "Return JSON LINE with ANSI syntax coloring based on SEVERITY."
  (let ((position 0)
        (length (length line))
        current-key
        pieces)
    (while (< position length)
      (let ((character (aref line position)))
        (cond
         ((eq character ?\")
          (let ((end (smv/ghostel-output-format--json-string-end line position)))
            (if (not end)
                (progn (push (substring line position) pieces)
                       (setq position length))
              (let ((after end))
                (while (and (< after length)
                            (smv/ghostel-output-format--json-whitespace-p
                             (aref line after)))
                  (setq after (1+ after)))
                (let* ((token (substring line position end))
                       (key-p (and (< after length)
                                   (eq (aref line after) ?:))))
                  (if key-p
                      (progn
                        (setq current-key (substring token 1 -1))
                        (push (smv/ghostel-output-format--style-json-string
                               token 'key)
                              pieces))
                    (push (smv/ghostel-output-format--style-json-value
                           token current-key severity)
                          pieces)
                    (setq current-key nil)))
                (setq position end)))))
         ((memq character '(?\{ ?\} ?\[ ?\] ?, ?:))
          (push (smv/ghostel-output-format--apply-style
                 'dim (substring line position (1+ position)) character)
                pieces)
          (when (memq character '(?\{ ?\[ ?, ?\} ?\]))
            (setq current-key nil))
          (setq position (1+ position)))
         ((smv/ghostel-output-format--json-whitespace-p character)
          (let ((end (1+ position)))
            (while (and (< end length)
                        (smv/ghostel-output-format--json-whitespace-p
                         (aref line end)))
              (setq end (1+ end)))
            (push (substring line position end) pieces)
            (setq position end)))
         (t
          (let ((end (1+ position)))
            (while (and (< end length)
                        (not (smv/ghostel-output-format--json-delimiter-p
                              (aref line end))))
              (setq end (1+ end)))
            (push (smv/ghostel-output-format--apply-style
                   'constant (substring line position end)
                   (substring line position end))
                  pieces)
            (setq current-key nil
                  position end))))))
    (apply #'concat (nreverse pieces))))

(defun smv/ghostel-output-format-render-json (record _rule line)
  "Render original JSON LINE with syntax coloring from RECORD."
  (let ((severity (smv/ghostel-output-format--field-cell record 'severity)))
    (smv/ghostel-output-format--color-json line (and severity (cdr severity)))))

(defun smv/ghostel-output-format--slf4j-renderer (record rule line)
  "Render the default SLF4J RECORD according to its selected display style."
  (if (eq smv/sl4j-ghostel-display-style 'json)
      (smv/ghostel-output-format-render-json record rule line)
    (smv/ghostel-output-format-render-template record rule)))


;;;; Rule dispatch

(defun smv/ghostel-output-format--rule-enabled-p (rule)
  "Return non-nil when RULE is enabled."
  (if (plist-member rule :enabled) (plist-get rule :enabled) t))

(defun smv/ghostel-output-format--render-rule (record rule line)
  "Render RECORD matched by RULE from original LINE."
  (let* ((output (plist-get rule :output))
         (renderer (or (plist-get rule :renderer)
                       (and (functionp output) output)))
         (rendered
          (if renderer
              (funcall renderer record rule line)
            (smv/ghostel-output-format-render-template record rule))))
    (unless (stringp rendered)
      (error "Renderer for rule %S returned %S"
             (plist-get rule :name) rendered))
    rendered))

(defun smv/ghostel-output-format-render-line (line)
  "Apply the first matching formatting rule to LINE.

LINE may be unibyte process data or ordinary multibyte Emacs text.  Return nil
when no rule matched, otherwise return rendered multibyte text."
  (let ((text (if (multibyte-string-p line)
                  line
                (decode-coding-string line 'utf-8-unix t))))
    (catch 'rendered
      (dolist (rule smv/ghostel-output-format-rules)
        (when (smv/ghostel-output-format--rule-enabled-p rule)
          (let ((record (smv/ghostel-output-format--parse-rule text rule)))
            (when record
              (throw 'rendered
                     (smv/ghostel-output-format--render-rule
                      record rule text))))))
      nil)))

(defun smv/ghostel-output-format--rule-possible-prefix-p (rule text)
  "Return non-nil when incomplete TEXT can still match RULE."
  (let* ((input (plist-get rule :input))
         (type (plist-get input :type))
         (trimmed (string-trim-left text)))
    (cond
     ((or (plist-get rule :parser) (eq type 'function)) t)
     ((eq type 'json)
      (or (string-empty-p trimmed)
          (eq (aref trimmed 0) ?\{)))
     ((eq type 'regexp)
      (let ((prefix (plist-get input :prefix-regexp)))
        (or (null prefix)
            (string-empty-p text)
            (string-match-p prefix text))))
     (t t))))

(defun smv/ghostel-output-format--possible-prefix-p (text)
  "Return non-nil when incomplete TEXT may match any enabled rule."
  (cl-some (lambda (rule)
             (and (smv/ghostel-output-format--rule-enabled-p rule)
                  (smv/ghostel-output-format--rule-possible-prefix-p rule text)))
           smv/ghostel-output-format-rules))


;;;; Process stream integration

(defun smv/ghostel-output-format--prepare-rendered (process rendered)
  "Prepare RENDERED text for PROCESS's selected backend."
  (pcase (process-get process smv/ghostel-output-format--backend-property)
    ('ghostel
     (if (multibyte-string-p rendered)
         (encode-coding-string rendered 'utf-8-unix t)
       rendered))
    ('compilation
     (let ((buffer (process-buffer process)))
       (if (buffer-live-p buffer)
           (with-current-buffer buffer
             (ansi-color-apply
              (if (multibyte-string-p rendered)
                  rendered
                (decode-coding-string rendered 'utf-8-unix t))))
         rendered)))
    (_ rendered)))

(defun smv/ghostel-output-format--transform-output (process output)
  "Transform complete rule-matching lines in PROCESS OUTPUT."
  (let* ((pending
          (or (process-get process smv/ghostel-output-format--pending-property) ""))
         (at-line-start
          (if (string-empty-p pending)
              (process-get process smv/ghostel-output-format--line-start-property)
            t))
         (data (concat pending output))
         (position 0)
         pieces)
    (process-put process smv/ghostel-output-format--pending-property "")
    (while (string-match "\r\n\\|\n\\|\r" data position)
      ;; Rule parsers and renderers may replace the global regexp match data.
      (let* ((separator-start (match-beginning 0))
             (separator-end (match-end 0))
             (line (substring data position separator-start))
             (separator (substring data separator-start separator-end))
             (rendered (and at-line-start
                            (smv/ghostel-output-format-render-line line))))
        (push (if rendered
                  (smv/ghostel-output-format--prepare-rendered process rendered)
                line)
              pieces)
        (push separator pieces)
        (setq at-line-start t
              position separator-end)))
    (let ((tail (substring data position)))
      (cond
       ((string-empty-p tail))
       ((and at-line-start (smv/ghostel-output-format--possible-prefix-p tail))
        (process-put process smv/ghostel-output-format--pending-property tail))
       (t
        (push tail pieces)
        (setq at-line-start nil))))
    (process-put process smv/ghostel-output-format--line-start-property
                 at-line-start)
    (apply #'concat (nreverse pieces))))

(defun smv/ghostel-output-format--process-filter (process output)
  "Transform PROCESS OUTPUT and delegate to its original filter."
  (let* ((original
          (process-get process smv/ghostel-output-format--filter-property))
         (pending
          (or (process-get process smv/ghostel-output-format--pending-property) ""))
         (transformed
          (condition-case error-data
              (smv/ghostel-output-format--transform-output process output)
            (error
             ;; Pending bytes have not reached the original filter yet.
             (process-put process smv/ghostel-output-format--pending-property "")
             (process-put process smv/ghostel-output-format--line-start-property nil)
             (message "Output formatter passed through a failed chunk: %s"
                      (error-message-string error-data))
             (concat pending output)))))
    (when (and original (not (string-empty-p transformed)))
      (funcall original process transformed))))

(defun smv/ghostel-output-format--flush-pending (process &optional transform)
  "Send PROCESS's pending output through its original filter.

When TRANSFORM is non-nil, apply formatting before sending it."
  (let ((pending
         (or (process-get process smv/ghostel-output-format--pending-property) ""))
        (original
         (process-get process smv/ghostel-output-format--filter-property)))
    (process-put process smv/ghostel-output-format--pending-property "")
    (when (and original (not (string-empty-p pending)))
      (let ((rendered (and transform
                           (smv/ghostel-output-format-render-line pending))))
        (funcall original process
                 (if rendered
                     (smv/ghostel-output-format--prepare-rendered process rendered)
                   pending))))))

(defun smv/ghostel-output-format--restore-process (process &optional flush)
  "Restore PROCESS handlers, optionally flushing unformatted pending output."
  (let ((original-filter
         (process-get process smv/ghostel-output-format--filter-property))
        (original-sentinel
         (process-get process smv/ghostel-output-format--sentinel-property)))
    (when flush
      (condition-case nil
          (smv/ghostel-output-format--flush-pending process)
        (error nil)))
    (when (and original-filter
               (eq (process-filter process)
                   #'smv/ghostel-output-format--process-filter))
      (set-process-filter process original-filter))
    (when (and original-sentinel
               (eq (process-sentinel process)
                   #'smv/ghostel-output-format--process-sentinel))
      (set-process-sentinel process original-sentinel))
    (process-put process smv/ghostel-output-format--filter-property nil)
    (process-put process smv/ghostel-output-format--sentinel-property nil)
    (process-put process smv/ghostel-output-format--backend-property nil)
    (process-put process smv/ghostel-output-format--pending-property nil)
    (process-put process smv/ghostel-output-format--line-start-property nil)))

(defun smv/ghostel-output-format--process-sentinel (process event)
  "Flush PROCESS, then delegate EVENT to its original sentinel."
  (let ((original
         (process-get process smv/ghostel-output-format--sentinel-property)))
    (if (not (memq (process-status process) '(exit signal)))
        (when original (funcall original process event))
      (unwind-protect
          (condition-case nil
              (smv/ghostel-output-format--flush-pending process t)
            (error nil))
        (smv/ghostel-output-format--restore-process process)
        (when original (funcall original process event))))))

(defun smv/ghostel-output-format--process-backend (process)
  "Return the supported formatting backend for PROCESS, or nil."
  (let ((buffer (and (processp process) (process-buffer process)))
        (filter (and (processp process) (process-filter process))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cond
         ((and (derived-mode-p 'ghostel-mode)
               (local-variable-p 'ghostel-compile--command)
               ghostel-compile--command
               (eq filter #'ghostel--filter))
          'ghostel)
         ((and (derived-mode-p 'compilation-mode)
               (eq filter #'compilation-filter))
          'compilation))))))

(defun smv/ghostel-output-format--maybe-wrap-process (process)
  "Install formatting on PROCESS when it has a supported backend."
  (when smv/ghostel-output-format-mode
    (let ((backend (smv/ghostel-output-format--process-backend process)))
      (when backend
        (process-put process smv/ghostel-output-format--filter-property
                     (process-filter process))
        (process-put process smv/ghostel-output-format--sentinel-property
                     (process-sentinel process))
        (process-put process smv/ghostel-output-format--backend-property backend)
        (process-put process smv/ghostel-output-format--pending-property "")
        (process-put process smv/ghostel-output-format--line-start-property t)
        (set-process-filter process #'smv/ghostel-output-format--process-filter)
        (set-process-sentinel process #'smv/ghostel-output-format--process-sentinel)))))

(defun smv/ghostel-output-format--wrap-running-processes ()
  "Install formatting on supported compilation processes already running."
  (dolist (process (process-list))
    (when (process-live-p process)
      (smv/ghostel-output-format--maybe-wrap-process process))))


;;;; User commands

;;;###autoload
(defun smv/ghostel-output-format-toggle-slf4j-style ()
  "Toggle future SLF4J records between compact and colored-JSON output."
  (interactive)
  (setq smv/sl4j-ghostel-display-style
        (if (eq smv/sl4j-ghostel-display-style 'pretty) 'json 'pretty))
  (message "SLF4J output style: %s (applies to new output)"
           smv/sl4j-ghostel-display-style))

;;;###autoload
(define-minor-mode smv/ghostel-output-format-mode
  "Format matching lines in Ghostel and ordinary compilation processes."
  :global t
  :group 'smv/ghostel-output-format
  (if smv/ghostel-output-format-mode
      (progn
        (add-hook 'compilation-start-hook
                  #'smv/ghostel-output-format--maybe-wrap-process)
        (smv/ghostel-output-format--wrap-running-processes))
    (remove-hook 'compilation-start-hook
                 #'smv/ghostel-output-format--maybe-wrap-process)
    (dolist (process (process-list))
      (when (or (eq (process-filter process)
                    #'smv/ghostel-output-format--process-filter)
                (eq (process-sentinel process)
                    #'smv/ghostel-output-format--process-sentinel))
        (smv/ghostel-output-format--restore-process process t)))))

(provide 'smv/ghostel-output-format)

;;; ghostel-output-format.el ends here

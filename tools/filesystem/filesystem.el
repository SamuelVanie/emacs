(defun smv-tool/write-file (path content)
  "Create new file or overwrite existing file (exercise caution).
INPUT: 
  - path (string): Full path where to write the file
  - content (string): Content to write to the file
Creates parent directories if they don't exist."
  (let ((dir (file-name-directory path)))
    (when dir
      (unless (file-exists-p dir)
        (make-directory dir t))))
  (with-temp-buffer
    (insert content)
    (write-file path))
  (message "File written: %s" path))


(defun smv-tool/edit-file (path edits &optional dry-run)
  "Make selective edits using advanced pattern matching.
INPUT:
  - path (string): File to edit
  - edits (list): List of edit operations. Can be in formats:
    1. Single pair: '(\"old\" \"new\")
    2. List of pairs: '((\"old1\" \"new1\") (\"old2\" \"new2\"))
    3. Alist format: '(((oldText . \"old\") (newText . \"new\")))
  - dry-run (boolean): Preview changes without applying (default: nil)
Returns detailed information about changes made or would be made."
  (unless (file-exists-p path)
    (error "File does not exist: %s" path))
  
  (let* ((normalized-edits (smv-tool/normalize-edits edits))
         (changes-made 0)
         (change-details '())
         (original-size 0)
         (new-size 0))
    
    (if dry-run
        ;; Dry run: work in temporary buffer
        (with-temp-buffer
          (insert-file-contents path)
          (setq original-size (buffer-size))
          
          (dolist (edit normalized-edits)
            (let ((old-text (cdr (assoc 'oldText edit)))
                  (new-text (cdr (assoc 'newText edit)))
                  (matches 0))
              (message "DEBUG: Searching for old-text: [%s] (length %d)" old-text (length old-text))  ; Inspect exact string
              (message "DEBUG: File size before edit: %d" (buffer-size))  ; Confirm buffer loaded
              (goto-char (point-min))
              (while (search-forward old-text nil t)
                (replace-match new-text nil t)
                (setq matches (1+ matches)))
              (when (> matches 0)
                (setq changes-made (+ changes-made matches))
                (push (list :old old-text :new new-text :matches matches) change-details))))
          
          (setq new-size (buffer-size))
          
          ;; Return preview information
          (list :path path
                :changes-count changes-made
                :details (nreverse change-details)
                :original-size original-size
                :new-size new-size
                :preview (if (> changes-made 0)
                             (buffer-substring 1 (min 501 (1+ (buffer-size))))
                           "No changes would be made")))
      
      ;; Real edit: use find-file to properly handle the file
      (save-excursion
        (let ((buffer (find-file-noselect path)))
          (with-current-buffer buffer
            (setq original-size (buffer-size))
            
            ;; Apply edits
            (dolist (edit normalized-edits)
              (let ((old-text (cdr (assoc 'oldText edit)))
                    (new-text (cdr (assoc 'newText edit)))
                    (matches 0))
                (goto-char (point-min))
                (while (search-forward old-text nil t)
                  (replace-match new-text nil t)
                  (setq matches (1+ matches)))
                (when (> matches 0)
                  (setq changes-made (+ changes-made matches))
                  (push (list :old old-text :new new-text :matches matches) change-details))))
            
            (setq new-size (buffer-size))
            
            ;; Save if changes were made
            (when (> changes-made 0)
              (message "saving %s..." path)
              (condition-case err (save-buffer) (error (message "Save error: %s" err)))
              (save-buffer))
            
            ;; Clean up buffer if it wasn't already open
            (unless (get-file-buffer path)
              (kill-buffer buffer)))))
      
      ;; Return results
      (list :path path
            :changes-applied changes-made
            :details (nreverse change-details)
            :original-size original-size
            :new-size new-size))))


(defun smv-tool/normalize-edits (edits)
  "Normalize edits to consistent alist format."
  ;; (message "DEBUG normalize-edits: type=%S, value=%S" (type-of edits) edits)
  
  ;; Convert vector to list if needed
  (when (vectorp edits)
    (setq edits (append edits nil)))
  
  (cond
   ;; Single pair of strings: ("old" "new")
   ((and (listp edits)
         (= (length edits) 2)
         (stringp (car edits))
         (stringp (cadr edits)))
    (list (list (cons 'oldText (car edits))
                (cons 'newText (cadr edits)))))
   
   ;; List of pairs: (("old1" "new1") ("old2" "new2"))
   ;; Need to handle when inner elements might also be vectors
   ((and (listp edits)
         (or (listp (car edits)) (vectorp (car edits))))
    (mapcar (lambda (pair)
              ;; Convert vector pairs to lists
              (when (vectorp pair)
                (setq pair (append pair nil)))
              (if (and (listp pair) (= (length pair) 2))
                  (list (cons 'oldText (car pair))
                        (cons 'newText (cadr pair)))
                (error "Invalid edit pair format: %S" pair)))
            edits))
   
   ;; Already in alist format
   ((and (listp edits)
         (listp (car edits))
         (consp (caar edits)))
    edits)
   
   ;; Invalid format
   (t
    (error "Invalid edits format: %S. Expected pairs of strings or alist format" edits))))


(defun smv-tool/create-directory (path)
  "Create new directory or ensure it exists.
INPUT: path (string) - Directory path to create
Creates parent directories if needed. Succeeds silently if directory already exists."
  (unless (file-exists-p path)
    (make-directory path t)
    (message "Directory created: %s" path))
  (unless (file-directory-p path)
    (error "Path exists but is not a directory: %s" path)))


(defun smv-tool/list-directory (path &optional show-hidden)
  "List directory contents with [FILE] or [DIR] prefixes.
Shows hidden files/directories but excludes .git directory by default.

INPUT: 
  - path (string): Directory path to list
  - show-hidden (boolean, optional): If t, show hidden files (starting with .)
    Default is nil (hide hidden files)

Returns a list of strings with [FILE] or [DIR] prefixes for each entry."
  (unless (file-exists-p path)
    (error "Directory does not exist: %s" path))
  (unless (file-directory-p path)
    (error "Path is not a directory: %s" path))
  
  ;; Get all entries including hidden ones (but still exclude . and ..)
  (let ((all-entries (directory-files path nil nil t)))
    ;; Filter entries based on criteria
    (let ((filtered-entries 
           (cl-remove-if 
            (lambda (entry)
              (or
               ;; Always exclude . and ..
               (string= entry ".")
               (string= entry "..")
               ;; Always exclude .git directory
               (string= entry ".git")
               ;; Exclude hidden files/dirs if show-hidden is nil
               (and (not show-hidden)
                    (string-prefix-p "." entry))))
            all-entries)))
      
      ;; Sort entries: directories first, then files, both alphabetically
      (let ((sorted-entries 
             (sort filtered-entries
                   (lambda (a b)
                     (let ((a-is-dir (file-directory-p (expand-file-name a path)))
                           (b-is-dir (file-directory-p (expand-file-name b path))))
                       (cond
                        ;; Both are dirs or both are files - sort alphabetically
                        ((eq a-is-dir b-is-dir) (string< a b))
                        ;; Directories come first
                        (a-is-dir t)
                        (b-is-dir nil)))))))
        
        ;; Format with prefixes
        (mapcar (lambda (entry)
                  (let ((full-path (expand-file-name entry path)))
                    (if (file-directory-p full-path)
                        (format "[DIR] %s" entry)
                      (format "[FILE] %s" entry))))
                sorted-entries)))))


(defun smv-tool/move-file (source destination)
  "Move or rename files and directories.
INPUT:
  - source (string): Source path
  - destination (string): Destination path  
Fails if destination already exists."
  (unless (file-exists-p source)
    (error "Source does not exist: %s" source))
  (when (file-exists-p destination)
    (error "Destination already exists: %s" destination))
  
  ;; Create destination directory if needed
  (let ((dest-dir (file-name-directory destination)))
    (when (and dest-dir (not (file-exists-p dest-dir)))
      (make-directory dest-dir t)))
  
  (rename-file source destination)
  (message "Moved: %s -> %s" source destination))


(defun smv-tool/search-files (path pattern &optional exclude-patterns)
  "Recursively search for files and directories matching pattern.
INPUT:
  - path (string): Starting directory for search
  - pattern (string): Search pattern (supports wildcards like *.js, file*.py)
  - exclude-patterns (list of strings): Patterns to exclude (glob format supported)
Returns full paths to all matches with [FILE] or [DIR] prefixes."
  (unless (file-exists-p path)
    (error "Search path does not exist: %s" path))
  (unless (file-directory-p path)
    (error "Search path is not a directory: %s" path))
  
  ;; Convert wildcard pattern to regex for directory-files-recursively
  (let* ((regex-pattern (wildcard-to-regexp pattern))
         (all-files (directory-files-recursively path regex-pattern t))
         (filtered-files all-files))
    
    ;; Apply exclude patterns if provided
    (when exclude-patterns
      (setq filtered-files
            (cl-remove-if 
             (lambda (file-path)
               (let ((file-name (file-name-nondirectory file-path)))
                 (cl-some (lambda (exclude-pattern)
                            (string-match-p (concat "^" (wildcard-to-regexp exclude-pattern) "$") file-name))
                          exclude-patterns)))
             filtered-files)))
    
    ;; Add [FILE] or [DIR] prefixes to results
    (mapcar (lambda (file-path)
              (if (file-directory-p file-path)
                  (format "[DIR] %s" file-path)
                (format "[FILE] %s" file-path)))
            filtered-files)))

(defun smv-tool/get-file-info (path)
  "Get detailed file or directory metadata.
INPUT: path (string) - Path to file or directory
Returns detailed metadata including size, timestamps, type, and permissions."
  (unless (file-exists-p path)
    (error "Path does not exist: %s" path))
  
  (let* ((attrs (file-attributes path))
         (is-directory (file-directory-p path))
         (size (if is-directory 
                   (length (directory-files path))
                 (nth 7 attrs)))
         (mod-time (nth 5 attrs))
         (access-time (nth 4 attrs))
         (creation-time (nth 6 attrs))
         (permissions (nth 8 attrs)))
    
    (list :path path
          :type (if is-directory "directory" "file")
          :size (if is-directory 
                    (format "%d entries" size)
                  (format "%d bytes" size))
          :permissions permissions
          :modified-time (format-time-string "%Y-%m-%d %H:%M:%S" mod-time)
          :access-time (format-time-string "%Y-%m-%d %H:%M:%S" access-time)
          :creation-time (format-time-string "%Y-%m-%d %H:%M:%S" creation-time)
          :readable (file-readable-p path)
          :writable (file-writable-p path)
          :executable (file-executable-p path))))


(defun wildcard-to-regexp (wildcard)
  "Convert shell wildcard pattern to regex pattern."
  (let ((regex (regexp-quote wildcard)))
    (setq regex (replace-regexp-in-string "\\\\\\*" ".*" regex))
    (setq regex (replace-regexp-in-string "\\\\\\?" "." regex))
    regex))



(defun smv-tool/grep-regex (search-string path &optional context-lines)
  "LLM-optimized search tool for finding code symbols and patterns in codebases.
Returns structured results with file paths, line numbers, and context to enable
efficient follow-up queries.

WORKFLOW GUIDANCE:
1. Start with directory search to locate relevant files
2. Use file search with context to examine specific implementations
3. Use line numbers from results with read-file tool for detailed analysis

INPUT:
  - search-string (string): Literal string or simple regex pattern to search for
  - path (string): Full path to file or directory to search in
  - context-lines (list of 2 integers, optional): [before, after] context lines
    Only applies to file searches. Recommended: [2, 3] for function definitions

RETURNS:
  - Directory search: List of matches with file paths, line numbers, and matched content
  - File search: Structured blocks with file path, line numbers, and content"
  (unless (file-exists-p path)
    (error "Path does not exist: %s" path))
  
  (if (file-directory-p path)
      ;; === DIRECTORY SEARCH ===
      ;; Use ripgrep (rg) if available, fallback to grep
      (let* ((expanded-path (expand-file-name path))
             (use-rg (executable-find "rg"))
             (command (if use-rg
                         (format "rg -n %s %s"
                                (shell-quote-argument search-string)
                                (shell-quote-argument expanded-path))
                       (format "grep -rn %s %s"
                              (shell-quote-argument search-string)
                              (shell-quote-argument expanded-path))))
             (output (shell-command-to-string command)))
        (when (> (length output) 0)
          (let ((lines (split-string output "\n" t)))
            (mapcar (lambda (line)
                     ;; Format: filepath:line_number:content
                     (if (string-match "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$" line)
                         (format "[MATCH] %s:%s - %s"
                                (match-string 1 line)
                                (match-string 2 line)
                                (string-trim (match-string 3 line)))
                       ;; Fallback if regex doesn't match
                       line))
                   lines))))
    
    ;; === FILE SEARCH ===
    ;; Convert vector to list if needed (GPTel passes JSON arrays as vectors)
    (let* ((context-list (cond
                          ((vectorp context-lines) (append context-lines nil))
                          ((listp context-lines) context-lines)
                          (t nil)))
           (before (if context-list (nth 0 context-list) 0))
           (after (if context-list (nth 1 context-list) 0))
           (results '()))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((lines (split-string (buffer-string) "\n"))
              (query-regexp (regexp-quote search-string)))
          (dotimes (i (length lines))
            (when (string-match-p query-regexp (nth i lines))
              (let* ((match-line-num (1+ i)) ; 1-based line numbers
                     (start-index (max 0 (- i before)))
                     (end-index (min (1- (length lines)) (+ i after)))
                     (start-line-num (1+ start-index))
                     (context-lines-with-nums '())
                     (match-found-at-line nil))
                
                ;; Build context with line numbers
                (dotimes (j (- end-index start-index -1))
                  (let* ((line-index (+ start-index j))
                         (line-num (1+ line-index))
                         (line-content (nth line-index lines))
                         (is-match-line (= line-num match-line-num)))
                    (when is-match-line
                      (setq match-found-at-line line-num))
                    (push (format "%4d%s %s"
                                 line-num
                                 (if is-match-line "*" ":")
                                 line-content)
                          context-lines-with-nums)))
                
                ;; Format the result block
                (let ((result-block 
                       (format "File: %s\nMatch at line %d:\n%s\n"
                              path
                              match-found-at-line
                              (mapconcat #'identity 
                                       (nreverse context-lines-with-nums) 
                                       "\n"))))
                  (push result-block results))))))
      (nreverse results)))))


(defun smv-tool/read-file (path &optional start-line end-line)
  "Read file content with optional line range specification and line numbers.
Optimized for LLM follow-up analysis after grep results.

INPUT:
  - path (string): Full path to the file to read
  - start-line (integer, optional): Starting line number (1-based, inclusive)
  - end-line (integer, optional): Ending line number (1-based, inclusive)

RETURNS:
  String with line numbers and content. If line range specified, shows only
  that range. Otherwise shows entire file."
  (unless (file-exists-p path)
    (error "File does not exist: %s" path))
  
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((lines (split-string (buffer-string) "\n"))
           (total-lines (length lines))
           (actual-start (if start-line (max 1 start-line) 1))
           (actual-end (if end-line (min end-line total-lines) total-lines))
           (result-lines '()))
      
      ;; Add header with file info
      (if (and start-line end-line)
          (push (format "File: %s (lines %d-%d of %d total)\n" 
                       path actual-start actual-end total-lines) result-lines)
        (push (format "File: %s (%d lines)\n" path total-lines) result-lines))
      
      ;; Add numbered content lines
      (dotimes (i (- actual-end actual-start -1))
        (let* ((line-index (+ actual-start i -1))
               (line-num (1+ line-index))
               (line-content (if (< line-index total-lines)
                               (nth line-index lines)
                             "")))
          (push (format "%4d: %s" line-num line-content) result-lines)))
      
      (mapconcat #'identity (nreverse result-lines) "\n"))))


(defun smv-tool/read-multiple-files (file-specs)
  "Read multiple files or file ranges in a single operation.
Efficient for comparing implementations or gathering related context.

INPUT:
  - file-specs (list or vector): List of file specifications, where each spec is:
    - A list [file-path] - reads entire file
    - A list [file-path start-line-str end-line-str] - reads line range

RETURNS:
  String with all file contents, separated by clear delimiters."
  ;; Convert vector to list if needed (GPTel passes JSON arrays as vectors)
  (let* ((specs-list (if (vectorp file-specs) 
                        (append file-specs nil) 
                        file-specs))
         (results '()))
    (dolist (spec specs-list)
      ;; Convert spec vector to list if needed
      (let* ((spec-list (if (vectorp spec) (append spec nil) spec))
             (file-content
              (cond
               ;; List with 1 element - entire file
               ((and (listp spec-list) (= (length spec-list) 1))
                (let ((path (nth 0 spec-list)))
                  (smv-tool/read-file path)))
               ;; List with 3 elements - file with line range
               ((and (listp spec-list) (= (length spec-list) 3))
                (let ((path (nth 0 spec-list))
                      (start (string-to-number (nth 1 spec-list)))
                      (end (string-to-number (nth 2 spec-list))))
                  (smv-tool/read-file path start end)))
               (t
                (error "Invalid file spec: %s. Expected [path] or [path, start-line, end-line]" spec-list)))))
        (push file-content results)
        (push "\n========================================\n" results)))
    
    ;; Remove the last separator and join
    (when results
      (pop results))
    (mapconcat #'identity (nreverse results) "")))


;; read-file
(gptel-make-tool
 :name "read_file"
 :function #'smv-tool/read-file
 :description "Read file content with line numbers. Essential follow-up tool after grep-regex. USE CASES: - Read entire files found: read-file('/path/to/relevant-file.py') - Read specific ranges: read-file('/path/file.js', 45, 65) - Examine context around matches: read-file('/path/file.py', match_line-5, match_line+10). e.g(match_line = 20): read-file('/path/file.py', 15, 30)"
 :confirm nil
 :include t
 :args (list
        '(:name "path"
                :type string
                :description "Full path to the file to read")
        '(:name "start-line"
                :type integer
                :optional t
                :description "Starting line number (1-based, inclusive). Omit to read from beginning.")
        '(:name "end-line"
                :type integer
                :optional t
                :description "Ending line number (1-based, inclusive). Omit to read to end."))
 :category "filesystem")

;; read-multiple-files
(gptel-make-tool
 :name "read_multiple_files"
 :function #'smv-tool/read-multiple-files
 :description "Read multiple files or file sections efficiently in one operation. OPTIMAL FOR: - Comparing related implementations: read-multiple-files([['file1.py'], ['file2.py']]) - Gathering distributed context: read-multiple-files([['main.js', '10', '30'], ['utils.js', '45', '60']]) - Following import chains or inheritance hierarchies REDUCES TOKEN USAGE by batching related reads vs. multiple separate read-file calls."
 :confirm nil
 :include t
 :args (list
        '(:name "file-specs"
                :type array
                :items (:type array :items (:type string))
                :description "Array of file specifications. Each item is an array of strings: [filepath] for entire file, or [filepath, start_line, end_line] for line range. Examples: [['main.py'], ['utils.js', '20', '50']]"))
 :category "filesystem")


;; grep-regex
(gptel-make-tool
 :name "grep_regex"
 :function #'smv-tool/grep-regex
 :description "Search for code symbols, functions, classes, or patterns across codebases. OPTIMAL WORKFLOW: 1. START with directory search to locate relevant files: grep-regex('ClassName', '/project/src/') 2. THEN search specific files with context: grep-regex('function myFunc', 'path/to/file.js', [2, 5]) 3. USE line numbers from results with read-file for detailed examination Returns file paths (directory search) or formatted blocks with line numbers (file search)."
 :confirm nil
 :include t
 :args (list
        '(:name "search-string"
                :type string
                :description "Literal text to search for. Be specific to avoid noise. Examples: 'def process_data', 'class UserModel', 'import pandas'")
        '(:name "path"
                :type string
                :description "Full path to file or directory. Use directory for discovery, file for detailed analysis.")
        '(:name "context-lines"
                :type array
                :items (:type integer)
                :optional t
                :description "Optional [before, after] context lines for file searches. Recommended: [2, 5] for functions, [1, 3] for variables."))
 :category "filesystem")

;; Write file
(gptel-make-tool
 :name "write_file"
 :function #'smv-tool/write-file
 :description "Create new file with some content"
 :confirm t  ; Confirm because it overwrites files
 :include t
 :args (list '(:name "path"
                     :type string
                     :description "Full path where to write the file")
             '(:name "content"
                     :type string
                     :description "Content to write to the file"))
 :category "filesystem")

;; Edit file
(gptel-make-tool
 :name "edit_file"
 :function #'smv-tool/edit-file
 :description "Make selective edits using advanced pattern matching and formatting inside a file. The best practice is to run it with dry-run activated to preview the changes, then reuse without it to apply changes. This preserves file integrity."
 :confirm t
 :include t
 :args (list '(:name "path"
                     :type string
                     :description "File to edit")
             '(:name "edits"
                     :type array
                     :items (:type array
                                  :items (:type string))
                     :description "List of edit operations. Can be simple comma separated pair like [[\"old\", \"new\"]] for one edit or list of comma separated pairs like [[\"old\", \"new\"], [\"old2\", \"new2\"]] for multiple edits")
             '(:name "dry-run"
                     :type boolean
                     :description "Preview changes without applying them (default: false)"
                     :optional t))
 :category "filesystem")

;; Create directory
(gptel-make-tool
 :name "create_directory"
 :function #'smv-tool/create-directory
 :description "Create new directory or ensure it exists. Creates parent directories if needed"
 :confirm nil
 :include t
 :args (list '(:name "path"
                     :type string
                     :description "Directory path to create"))
 :category "filesystem")


(gptel-make-tool
 :name "list_directory"
 :function #'smv-tool/list-directory
 :description "List directory contents with clear [FILE] and [DIR] prefixes."
 :confirm nil
 :include t
 :args (list
        '(:name "path"
                :type string
                :description "Directory path to list. Use '.' for current directory.")
        '(:name "show-hidden"
                :type boolean
                :optional t
                :description "Optional. If true, shows hidden files/directories (starting with .). Default is false."))
 :category "filesystem")

;; Move file
(gptel-make-tool
 :name "move_file"
 :function #'smv-tool/move-file
 :description "Move or rename files and directories. Fails if destination exists"
 :confirm t  ; Confirm because it moves files
 :include t
 :args (list '(:name "source"
                     :type string
                     :description "Source path")
             '(:name "destination"
                     :type string
                     :description "Destination path"))
 :category "filesystem")

;; Search files
(gptel-make-tool
 :name "search_files"
 :function #'smv-tool/search-files
 :description "Recursively search for files/directories with case-insensitive pattern matching. Returns results with [FILE] or [DIR] prefixes. PATTERN EXAMPLES: - '*.js' - all JavaScript files - 'file*.txt' - files starting with 'file' and ending with .txt - '*config*' - files containing 'config' in name - 'test_*.py' - Python test files"
 :confirm nil
 :include t
 :args (list '(:name "path"
                     :type string
                     :description "Starting directory for search")
             '(:name "pattern"
                     :type string
                     :description "Search pattern (case-insensitive, supports wildcards)")
             '(:name "exclude-patterns"
                     :type array
                     :items (:type string)
                     :description "Patterns to exclude (glob format supported)"
                     :optional t))
 :category "filesystem")

;; Get file info
(gptel-make-tool
 :name "get_file_info"
 :function #'smv-tool/get-file-info
 :description "Get detailed file/directory metadata including size, timestamps, type, and permissions"
 :confirm nil
 :include t
 :args (list '(:name "path"
                     :type string
                     :description "Path to file or directory"))
 :category "filesystem")

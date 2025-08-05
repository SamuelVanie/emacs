(defun smv-tool/read-file (path)
  "Read complete contents of a file.
INPUT: path (string) - Full path to the file to read
Returns the complete file contents as a UTF-8 encoded string."
  (unless (file-exists-p path)
    (error "File does not exist: %s" path))
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun smv-tool/read-multiple-files (paths)
  "Read multiple files simultaneously.
INPUT: paths (list of strings) - Array of full file paths to read
Returns a list of cons cells (path . content) for successfully read files.
Failed reads won't stop the entire operation - they're logged but skipped."
  (let (results)
    (dolist (path paths)
      (condition-case err
          (let ((content (smv-tool/read-file path)))
            (push (cons path content) results))
        (error
         (message "Failed to read file %s: %s" path (error-message-string err)))))
    (nreverse results)))

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
  (message "DEBUG normalize-edits: type=%S, value=%S" (type-of edits) edits)
  
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

(defun smv-tool/list-directory (path)
  "List directory contents with [FILE] or [DIR] prefixes.
INPUT: path (string) - Directory path to list
Returns a list of strings with [FILE] or [DIR] prefixes for each entry."
  (unless (file-exists-p path)
    (error "Directory does not exist: %s" path))
  (unless (file-directory-p path)
    (error "Path is not a directory: %s" path))
  
  (let ((entries (directory-files path nil "^[^.]"))) ; Exclude . and ..
    (mapcar (lambda (entry)
              (let ((full-path (expand-file-name entry path)))
                (if (file-directory-p full-path)
                    (format "[DIR] %s" entry)
                  (format "[FILE] %s" entry))))
            entries)))

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
  - pattern (string): Search pattern (case-insensitive, supports wildcards)
  - exclude-patterns (list of strings): Patterns to exclude (glob format supported)
Returns full paths to all matches with [FILE] or [DIR] prefixes."
  (unless (file-exists-p path)
    (error "Search path does not exist: %s" path))
  (unless (file-directory-p path)
    (error "Search path is not a directory: %s" path))
  
  ;; Use directory-files-recursively for the heavy lifting
  (let* ((all-files (directory-files-recursively path pattern t))
         (filtered-files all-files))
    
    ;; Apply exclude patterns if provided
    (when exclude-patterns
      (setq filtered-files
            (cl-remove-if 
             (lambda (file-path)
               (let ((file-name (file-name-nondirectory file-path)))
                 (cl-some (lambda (exclude-pattern)
                            (string-match-p (wildcard-to-regexp exclude-pattern) file-name))
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

;; Utility function for wildcard to regex conversion
(defun wildcard-to-regexp (wildcard)
  "Convert shell wildcard pattern to regex pattern."
  (let ((regex (regexp-quote wildcard)))
    (setq regex (replace-regexp-in-string "\\\\\\*" ".*" regex))
    (setq regex (replace-regexp-in-string "\\\\\\?" "." regex))
    (concat "^" regex "$")))


(defun smv-tool/grep-regex (search-string path &optional context-lines)
  "Search for a string within a file or directory, similar to 'grep'.

When searching a directory, it recursively finds all files containing the
string and returns a list of their paths. The 'context-lines' parameter is ignored.

When searching a file, it finds all matching lines and returns them as a list
of strings. If 'context-lines' is provided as a list of two integers,
like '(2 2)', it will include that many lines of context before and after
each matching line.

INPUT:
  - search-string (string): The literal string to search for.
  - path (string): The full path to the file or directory to search in.
  - context-lines (list of 2 integers, optional): For file searches, specifies
    the number of context lines to show [before, after] the match.
    Example: '(3 1)' shows 3 lines before and 1 line after.
    This parameter is IGNORED if 'path' is a directory.

RETURNS:
  - If 'path' is a directory: A list of file paths that contain a match.
  - If 'path' is a file: A list of strings, where each string is a block
    of text containing a match and its surrounding context."
  (unless (file-exists-p path)
    (error "Path does not exist: %s" path))

  (if (file-directory-p path)
      ;; --- DIRECTORY SEARCH ---
      ;; Use the command-line `grep` for efficiency.
      ;; -r: recursive, -l: list files with matches.
      (let* ((command (format "grep -r -l %s %s"
                             (shell-quote-argument search-string)
                             (shell-quote-argument path)))
             (output (shell-command-to-string command)))
        ;; Return a list of file paths, filtering out empty lines.
        (when (> (length output) 0)
          (split-string output "\n" t)))

    ;; --- FILE SEARCH ---
    (let ((before (or (nth 0 context-lines) 0))  ; CORRECTED LINE
          (after (or (nth 1 context-lines) 0))   ; CORRECTED LINE
          (results '()))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((lines (split-string (buffer-string) "\n"))
              (query-regexp (regexp-quote search-string)))
          (dotimes (i (length lines))
            (when (string-match-p query-regexp (nth i lines))
              (let* ((start-index (max 0 (- i before)))
                     (end-index (min (1- (length lines)) (+ i after)))
                     (context-slice (seq-subseq lines start-index (1+ end-index))))
                (push (mapconcat #'identity context-slice "\n") results))))))
      (nreverse results))))


(gptel-make-tool
 :name "grep-regex"
 :function #'smv-tool/grep-regex
 :description "Searches for a specific string within a given path. If the path points to a directory, the function recursively finds all files containing the string and returns a list of those file paths. If the path is a single file, it returns the matching lines, with the option to include a specified number of lines of context before and after each match. You could for example use that function to get the file where a particular function is implemented then use that result to get the implementation of that function (call the tool with 0 for the above value and 30, to be safe, for below)."
 :confirm nil
 :include t
 :args (list
        '(:name "search-string"
                :type string
                :description "The literal text string to search for. Be as specific as possible to avoid context wasting")
        '(:name "path"
                :type string
                :description "The full path to the file or directory to search in.")
        '(:name "context-lines"
                :type array
                :items (:type integer) 
                :optional t
                :description "An optional list of two integers `[before, after]` that specifies the number of context lines to show. This is only applicable when `path` is a file and is ignored for directory searches."))
 :category "filesystem")

(gptel-make-tool
 :name "read_file"
 :function #'smv-tool/read-file
 :description "Read complete contents of a file with UTF-8 encoding"
 :confirm nil
 :include t
 :args (list '(:name "path"
                     :type string
                     :description "Full path to the file to read"))
 :category "filesystem")

;; Read multiple files
(gptel-make-tool
 :name "read_multiple_files"
 :function #'smv-tool/read-multiple-files
 :description "Read multiple files simultaneously. Failed reads won't stop the entire operation"
 :confirm nil
 :include t
 :args (list '(:name "paths"
                     :type array
                     :items (:type string)
                     :description "Comma separated list of full file paths to read"))
 :category "filesystem")

;; Write file
(gptel-make-tool
 :name "write_file"
 :function #'smv-tool/write-file
 :description "Create new file or overwrite existing file (exercise caution with this)"
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
 :description "Make selective edits using advanced pattern matching and formatting."
 :confirm t  ; Confirm because it modifies files
 :include t
 :args (list '(:name "path"
                     :type string
                     :description "File to edit")
             '(:name "edits"
                     :type array
                     :items (:type array
                                  :items (:type string))
                     :description "List of edit operations. Can be simple comma separated pair like [\"old\", \"new\"] for one edit or list of comma separated pairs like [[\"old\", \"new\"], [\"old2\", \"new2\"]] for multiple edits")
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

;; List directory
(gptel-make-tool
 :name "list_directory"
 :function #'smv-tool/list-directory
 :description "List directory contents with [FILE] or [DIR] prefixes"
 :confirm nil
 :include t
 :args (list '(:name "path"
                     :type string
                     :description "Directory path to list"))
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
 :description "Recursively search for files/directories with case-insensitive pattern matching. Returns results with [FILE] or [DIR] prefixes"
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



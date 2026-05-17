(require 'cl-lib)

(defcustom smv/gptel-repomap-max-depth 2
  "Default maximum directory depth for the repomap."
  :type 'integer)

(defcustom smv/gptel-repomap-max-lines 500
  "Maximum number of lines to include in the repomap."
  :type 'integer)

(defvar smv/gptel-repomap-selected-dirs nil
  "Directories selected for the current gptel repomap.
If nil, use the whole project root.")

(defvar-local smv/gptel-repomap--cached-prompt nil
  "Buffer-local cache to prevent prompting on every message when using the preset.")

(defun smv/gptel-project-root ()
  "Return current project root or signal an error."
  (or (when-let ((proj (project-current nil)))
        (expand-file-name (project-root proj)))
      (user-error "Not in a project")))

(defvar smv/gptel-directory-children-excluded-patterns
  '("\\`#.*#\\'"
    "\\`\\.\\.?\\'"
    "\\`\\..*\\'"
    "\\`node_modules\\'"
    "\\`__pycache__\\'"
    "\\`build\\'"
    "\\`dist\\'"
    "\\`target\\'")
  "List of Emacs regular expressions excluded by `smv/gptel--directory-children'.")

(defun smv/gptel--excluded-child-p (path excluded-patterns)
  "Return non-nil when PATH should be excluded using EXCLUDED-PATTERNS."
  (let ((name (file-name-nondirectory (directory-file-name path))))
    (seq-some
     (lambda (regexp)
       (string-match-p regexp name))
     excluded-patterns)))

(defun smv/gptel--directory-children (dir)
  "Return sorted children of DIR, excluding names matching patterns."
  (seq-sort
   #'string<
   (seq-remove
    (lambda (path)
      (smv/gptel--excluded-child-p
       path
       smv/gptel-directory-children-excluded-patterns))
    (directory-files dir t nil t))))

(defun smv/gptel--repomap-lines (dir max-depth)
  "Return a flat list of tree lines for DIR up to MAX-DEPTH."
  (let ((root (file-name-as-directory (expand-file-name dir))))
    (cl-labels
        ((build-lines (current depth prefix)
           (let* ((children (smv/gptel--directory-children current))
                  (count (length children))
                  (index 0)
                  (result nil))
             (dolist (child children result)
               (setq index (1+ index))
               (let* ((name (file-name-nondirectory (directory-file-name child)))
                      (dirp (file-directory-p child))
                      (lastp (= index count))
                      (branch (if lastp "└── " "├── "))
                      (next-prefix (concat prefix (if lastp "    " "│   "))))
                 (setq result
                       (append
                        result
                        (list (concat prefix branch name (if dirp "/" "")))
                        (when (and dirp (< depth max-depth))
                          (build-lines child (1+ depth) next-prefix)))))))))
      (cons (file-name-nondirectory (directory-file-name root))
            (build-lines root 1 "")))))

(defun smv/gptel--truncate-lines (lines max-lines)
  "Truncate LINES to MAX-LINES, appending a notice if needed."
  (if (<= (length lines) max-lines)
      lines
    (append
     (cl-subseq lines 0 (max 0 (1- max-lines)))
     (list (format "... truncated to %d lines" max-lines)))))

(defun smv/gptel-build-repomap (&optional dirs)
  "Build a textual repomap for DIRS or the current project root."
  (let* ((root (smv/gptel-project-root))
         (targets (or dirs (list root)))
         (header (format "Project root: %s\nRepomap (max depth %d):"
                         root
                         smv/gptel-repomap-max-depth))
         (all-lines
          (apply
           #'append
           (mapcar
            (lambda (dir)
              (let ((abs (expand-file-name dir root)))
                (unless (file-directory-p abs)
                  (user-error "Not a directory: %s" dir))
                (smv/gptel--repomap-lines abs smv/gptel-repomap-max-depth)))
            targets))))
    (string-join
     (cons header
           (smv/gptel--truncate-lines all-lines smv/gptel-repomap-max-lines))
     "\n")))

(defun smv/gptel--project-subdirs (root max-depth)
  "Return project subdirectories under ROOT bounded by MAX-DEPTH."
  (let ((dirs nil))
    (cl-labels
        ((walk (dir depth)
           (when (<= depth max-depth)
             (dolist (child (smv/gptel--directory-children dir))
               (when (file-directory-p child)
                 (push child dirs)
                 (walk child (1+ depth)))))))
      (walk root 1)
      (mapcar (lambda (d) (file-relative-name d root))
              (nreverse dirs)))))

(defun smv/gptel-read-repomap-dirs ()
  "Read project subdirectories for repomap.
If the minibuffer input is empty, use the whole project."
  (let* ((root (smv/gptel-project-root))
         (choices (smv/gptel--project-subdirs root smv/gptel-repomap-max-depth))
         (input
          (completing-read-multiple
           "Folders for repomap (empty = whole project, comma-separated): "
           choices nil t)))
    (setq smv/gptel-repomap-selected-dirs
          (unless (or (null input)
                      (equal input '(""))
                      (seq-every-p #'string-empty-p input))
            input))))


(defun smv/gptel-repomap-system-prompt ()
  "Return a dynamic system prompt including the repomap.
Caches the prompt per-buffer so you aren't prompted on every single interaction."
  (unless smv/gptel-repomap--cached-prompt
    (smv/gptel-read-repomap-dirs)
    (setq smv/gptel-repomap--cached-prompt
           (smv/gptel-build-repomap smv/gptel-repomap-selected-dirs)))
  smv/gptel-repomap--cached-prompt)

(defun smv/gptel-reset-repomap ()
  "Clear the cached repomap prompt in the current buffer so it can be rebuilt."
  (interactive)
  (setq smv/gptel-repomap--cached-prompt nil)
  (message "Repomap cache cleared! (Will prompt on next message)"))


(gptel-make-preset 'repomap
  :description "Add a project repomap to the system prompt"
  :system (list :function 
                (lambda (current-system)
                  (concat current-system
                          "\n\n" 
                          (smv/gptel-repomap-system-prompt)))))

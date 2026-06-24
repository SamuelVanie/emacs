(defvar smv/gptel-ediff-tool-names '("Edit" "Write" "Insert")
  "Tool names whose calls should be previewed via ediff.")

(defvar smv/gptel-ediff-reject-key "R"
  "Key bound in the ediff control buffer to reject the proposed change.")

(defvar smv/gptel-ediff-comment-key "H"
  "Key bound in ediff review buffers to add a region-specific rejection comment.")

(defvar smv/gptel-ediff-review-mode nil
  "Buffer-local flag enabling temporary gptel ediff review bindings.")

(defun smv/gptel-ediff--set-mode-for (buffer path)
  "Try to set BUFFER's major mode based on PATH (cosmetic, for highlighting)."
  (when (and buffer path (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((bfn buffer-file-name))
        (setq buffer-file-name path)
        (unwind-protect
            (ignore-errors (set-auto-mode t))
          (setq buffer-file-name bfn))))))

(defun smv/gptel-ediff--proposed-insert (path line-number new-str)
  "Compute the would-be content of PATH after the Insert tool runs."
  (with-temp-buffer
    (when (and path (file-exists-p path))
      (insert-file-contents path))
    (cond
     ((or (null line-number) (zerop line-number)) (goto-char (point-min)))
     ((< line-number 0)                           (goto-char (point-max)))
     (t (goto-char (point-min)) (forward-line line-number)))
    (insert new-str)
    (unless (or (string-suffix-p "\n" new-str) (eobp))
      (insert "\n"))
    (buffer-string)))

(defun smv/gptel-ediff--proposed-edit (path old-str new-str)
  "Compute the would-be content of PATH after a short-form Edit.
Returns nil if OLD-STR isn't found, or PATH is missing/a directory."
  (when (and path (file-exists-p path) (not (file-directory-p path)))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (when (search-forward old-str nil t)
        (replace-match new-str t t)
        (buffer-string)))))

;;;; --- Diff mode preview for the Edit tool ---------------------------------

(defun smv/gptel-ediff--strip-fences ()
  "Strip ```diff / ``` fences from current buffer, in place."
  (goto-char (point-min))
  (when (looking-at "^ *```\\(?:diff\\|patch\\)?\\s-*\n")
    (delete-region (match-beginning 0) (match-end 0)))
  (goto-char (point-max))
  (skip-chars-backward " \t\r\n")
  (when (looking-back "^ *```\\s-*" (line-beginning-position))
    (delete-region (line-beginning-position) (point-max))))

(defun smv/gptel-ediff--diff-paths (diff-text)
  "Return paths-as-written-in-DIFF-TEXT from the +++ headers."
  (let (paths)
    (with-temp-buffer
      (insert diff-text)
      (smv/gptel-ediff--strip-fences)
      (goto-char (point-min))
      (while (re-search-forward "^\\+\\+\\+ \\([^\t\n]+\\)" nil t)
        (let ((p (string-trim (match-string 1))))
          (unless (string= p "/dev/null")
            (push p paths)))))
    (delete-dups (nreverse paths))))

(defun smv/gptel-ediff--resolve (rel base-dir)
  "Find an existing file for REL under BASE-DIR by trying strip levels.
Handles git-style a/ b/ prefixes the same way `patch' would."
  (let ((parts (split-string rel "/" t)) hit)
    (while (and parts (not hit))
      (let ((abs (expand-file-name (string-join parts "/") base-dir)))
        (if (file-exists-p abs) (setq hit abs)
          (setq parts (cdr parts)))))
    hit))

(defun smv/gptel-ediff--proposed-diff (path diff-text)
  "Apply DIFF-TEXT against copies of the affected files and return
a list of (ABSOLUTE-ORIGINAL-PATH . PROPOSED-CONTENT)."
  (let* ((base-dir (if (file-directory-p path)
                       (file-name-as-directory (expand-file-name path))
                     (file-name-directory (expand-file-name path))))
         (diff-paths (smv/gptel-ediff--diff-paths diff-text))
         (tmp-dir    (file-name-as-directory
                      (make-temp-file "gptel-ediff-" t)))
         path-map results)
    (unwind-protect
        (progn
          ;; Mirror each affected file into tmp-dir at the path the diff names,
          ;; so `patch' with default options finds it where it expects.
          (dolist (dp diff-paths)
            (let* ((src  (smv/gptel-ediff--resolve dp base-dir))
                   (dest (expand-file-name dp tmp-dir)))
              (make-directory (file-name-directory dest) t)
              (when src (copy-file src dest t))
              (push (cons (or src (expand-file-name dp base-dir)) dest)
                    path-map)))
          ;; Run `patch' with the same options as gptel-agent--edit-files.
          (with-temp-buffer
            (insert diff-text)
            (smv/gptel-ediff--strip-fences)
            (goto-char (point-max))
            (unless (eq (char-before) ?\n) (insert "\n"))
            (let ((default-directory tmp-dir)
                  (out (generate-new-buffer " *gptel-ediff-patch*")))
              (unwind-protect
                  (call-process-region (point-min) (point-max)
                                       "patch" nil out nil
                                       "--forward" "--silent")
                (kill-buffer out))))
          ;; Read each result back.
          (dolist (pair path-map)
            (let ((orig (car pair)) (tmp (cdr pair)))
              (when (file-exists-p tmp)
                (push (cons orig
                            (with-temp-buffer
                              (insert-file-contents tmp)
                              (buffer-string)))
                      results)))))
      (when (file-directory-p tmp-dir)
        (delete-directory tmp-dir t)))
    (nreverse results)))


(defun smv/gptel-ediff--proposed (name args)
  "Return a list of (PATH . CONTENT) previews for tool NAME with ARGS, or nil."
  (pcase name
    ("Insert"
     (let ((path (plist-get args :path)))
       (list (cons path
                   (smv/gptel-ediff--proposed-insert
                    path
                    (plist-get args :line_number)
                    (or (plist-get args :new_str) ""))))))
    ("Edit"
     (let* ((path    (plist-get args :path))
            (diffp   (plist-get args :diff))
            (old-str (plist-get args :old_str))
            (new-str (plist-get args :new_str))
            (diff-mode-p (and diffp (not (eq diffp :json-false)) new-str))
            (text-mode-p (or (eq diffp :json-false) old-str)))
       (cond
        (diff-mode-p
         (smv/gptel-ediff--proposed-diff path new-str))
        ((and text-mode-p path (not (file-directory-p path)))
         (when-let ((content (smv/gptel-ediff--proposed-edit
                              path (or old-str "") (or new-str ""))))
           (list (cons path content)))))))
    ("Write"
     (let* ((dir (or (plist-get args :path) "."))
            (filename (plist-get args :filename))
            (path (and filename (expand-file-name filename dir))))
       (list (cons path (or (plist-get args :content) "")))))
    (_ nil)))

(defun smv/gptel-ediff--select-main-window ()
  "Select a non-side, non-dedicated window in the current frame.
If none exists, split one off the frame root."
  (let ((current (selected-window)))
    (when (or (window-parameter current 'window-side)
              (window-dedicated-p current))
      (let ((target (seq-find (lambda (w)
                                (and (not (window-parameter w 'window-side))
                                     (not (window-dedicated-p w))))
                              (window-list nil 'no-mini))))
        (if target
            (select-window target)
          (select-window (split-window (frame-root-window) nil 'below)))))))

(defun smv/gptel-ediff--line-range (beg end)
  "Return a human-readable line or line range for BEG and END in current buffer."
  (let ((start (line-number-at-pos beg t))
        (finish (line-number-at-pos end t)))
    (if (= start finish)
        (format "line %d" start)
      (format "lines %d-%d" start finish))))

(defun smv/gptel-ediff--format-region-comments (comments)
  "Format region-specific rejection COMMENTS for the final refusal message."
  (concat
   "Region-specific rejection comments:\n\n"
   (mapconcat
    (lambda (entry)
      (let ((index (plist-get entry :index))
            (source (plist-get entry :source))
            (range (plist-get entry :range))
            (text (plist-get entry :text))
            (comment (plist-get entry :comment)))
        (format "%d. from %s, %s:\n%s\n\nComment:\n%s"
                index source range text comment)))
    comments
    "\n\n")))

(defun smv/gptel-ediff--buffer-local-set-key (key command)
  "Bind KEY to COMMAND in the current buffer without changing shared mode maps."
  (setq-local smv/gptel-ediff-review-mode t)
  (unless (local-variable-p 'minor-mode-overriding-map-alist)
    (setq-local minor-mode-overriding-map-alist
                (copy-tree minor-mode-overriding-map-alist)))
  (let ((map (or (cdr (assq 'smv/gptel-ediff-review-mode
                            minor-mode-overriding-map-alist))
                 (let ((new-map (make-sparse-keymap)))
                   (push (cons 'smv/gptel-ediff-review-mode new-map)
                         minor-mode-overriding-map-alist)
                   new-map))))
    (define-key map (kbd key) command)))

(defun smv/gptel-ediff--review (path proposed-content)
  "Open ediff comparing on-disk PATH against PROPOSED-CONTENT and block.
Return nil if the user accepted (plain `q'), or a string (possibly empty)
giving the rejection reason."
  (let* ((basename (if path (file-name-nondirectory path) "<new file>"))
         (original (generate-new-buffer (format "*gptel-original: %s*" basename)))
         (proposed (generate-new-buffer (format "*gptel-proposed: %s*" basename)))
         (decision (list nil nil))            ; (rejected-p reason)
         (region-comments nil)
         (saved-config (current-window-configuration))
         (ctrl nil)                           ; Keep track of the ediff control buffer
         ;; 1. Add &rest _ so this lambda is compatible with advice-add
         (quit-fn (lambda (&rest _) (exit-recursive-edit)))
         (comment-fn
          (lambda ()
            (interactive)
            (let* ((source (cond
                            ((eq (current-buffer) original) "original file")
                            ((eq (current-buffer) proposed) "proposed changes")
                            (t nil))))
              (if (not source)
                  (message "Use %s from the original or proposed buffer."
                           smv/gptel-ediff-comment-key)
                (let* ((has-region (use-region-p))
                       (beg (if has-region (region-beginning) (line-beginning-position)))
                       (end (if has-region (region-end) (line-end-position)))
                       (range-end (if has-region (max beg (1- end)) end))
                       (range (smv/gptel-ediff--line-range beg range-end))
                       (text (string-trim
                              (buffer-substring-no-properties beg end)))
                       (comment (read-string
                                 (format "Comment for %s %s: " source range))))
                  (if (string-empty-p comment)
                      (message "Empty region comment ignored.")
                    (push (list :index (1+ (length region-comments))
                                :source source
                                :range range
                                :text text
                                :comment comment)
                          region-comments)
                    (message "Added rejection comment %d."
                             (length region-comments)))))))))
    
    (with-current-buffer original
      (when (and path (file-exists-p path))
        (insert-file-contents path)))
    (smv/gptel-ediff--set-mode-for original path)
    
    (with-current-buffer proposed
      (insert proposed-content))
    (smv/gptel-ediff--set-mode-for proposed path)
    
    (unwind-protect
        (progn
          ;; Step out of any side / dedicated window before ediff reshuffles.
          (smv/gptel-ediff--select-main-window)
          
          ;; 2. Tell Emacs to exit the recursive edit ONLY AFTER ediff completes its own quit
          (advice-add 'ediff-really-quit :after quit-fn)
          
          (setq ctrl (ediff-buffers original proposed))
          (dolist (buffer (list original proposed))
            (with-current-buffer buffer
              (smv/gptel-ediff--buffer-local-set-key
               smv/gptel-ediff-comment-key comment-fn)))
          (with-current-buffer ctrl
            (smv/gptel-ediff--buffer-local-set-key
             smv/gptel-ediff-comment-key comment-fn)
            (smv/gptel-ediff--buffer-local-set-key
             smv/gptel-ediff-reject-key
             (lambda ()
               (interactive)
               (let ((reason (if region-comments
                                 (smv/gptel-ediff--format-region-comments
                                  (nreverse region-comments))
                               (read-string
                                (format "Reject change to %s — reason: " basename)))))
                 (setcar decision t)
                 (setcar (cdr decision) reason)
                 ;; This triggers ediff-really-quit, which native-cleans up, 
                 ;; and then hits our :after advice to unblock Emacs!
                 (ediff-really-quit nil)))))
          (recursive-edit))
      
      ;; 3. Cleanup after the block is lifted
      (advice-remove 'ediff-really-quit quit-fn)
      
      ;; Force-kill the control panel if the user forcefully aborted via C-] (abort-recursive-edit)
      (when (and ctrl (buffer-live-p ctrl))
        (kill-buffer ctrl))
        
      (when (buffer-live-p proposed) (kill-buffer proposed))
      (when (buffer-live-p original) (kill-buffer original))
      (set-window-configuration saved-config))
    
    (when (car decision)
      (or (cadr decision) ""))))

(defun smv/gptel-ediff-tool-call (plist)
  "Pre-tool-call hook: preview Edit/Write/Insert via ediff before running."
  (when gptel-confirm-tool-calls
    (let ((name (plist-get plist :name)))
      (when (member name smv/gptel-ediff-tool-names)
        (when-let* ((args      (plist-get plist :args))
                    (proposals (smv/gptel-ediff--proposed name args))
                    (proposals (seq-filter (lambda (p) (and (car p) (cdr p)))
                                           proposals))
                    ((consp proposals)))
          (let (rejection)
            (catch 'reject
              (dolist (pair proposals)
                (when-let ((reason (smv/gptel-ediff--review
                                    (car pair) (cdr pair))))
                  (setq rejection (cons (car pair) reason))
                  (throw 'reject nil))))
            (if rejection
                (list :block
                      (let ((r (cdr rejection)))
                        (if (string-empty-p r)
                            (format "Tool '%s' was rejected during ediff review of %s."
                                    name (file-name-nondirectory (car rejection)))
                          (format "THE USER HAS REJECTED THE TOOL CALL DURING EDIFF REVIEW OF %s.\nADJUST ACCORDINGLY.\nREASON:\n%s"
                                  (file-name-nondirectory (car rejection)) r))))
              (list :confirm nil))))))))

(add-hook 'gptel-pre-tool-call-functions #'smv/gptel-ediff-tool-call)

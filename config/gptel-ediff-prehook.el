(defvar smv/gptel-ediff-tool-names '("Edit" "ReplaceIn" "Write" "Insert")
  "Tool names whose calls should be previewed via ediff.")

(defvar smv/gptel-ediff-reject-key "R"
  "Key bound in the ediff control buffer to reject the proposed change.")

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

(defun smv/gptel-ediff--proposed (name args)
  "Return (PATH . CONTENT) preview for tool NAME with ARGS, or nil."
  (pcase name
    ("Insert"
     (let ((path (plist-get args :path)))
       (cons path
             (smv/gptel-ediff--proposed-insert
              path
              (plist-get args :line_number)
              (or (plist-get args :new_str) "")))))
    ("Edit"
     (let* ((path    (plist-get args :path))
            (diffp   (plist-get args :diff))
            (old-str (plist-get args :old_str))
            (new-str (plist-get args :new_str))
            (text-mode-p (or (eq diffp :json-false) old-str)))
       (when (and text-mode-p
                  path
                  (not (file-directory-p path)))
         (when-let ((content (smv/gptel-ediff--proposed-edit
                              path
                              (or old-str "")
                              (or new-str ""))))
           (cons path content)))))
    ("Write"
     (let* ((dir (or (plist-get args :path) "."))
            (filename (plist-get args :filename))
            (path (and filename (expand-file-name filename dir))))
       (cons path (or (plist-get args :content) ""))))
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

(defun smv/gptel-ediff--review (path proposed-content)
  "Open ediff comparing on-disk PATH against PROPOSED-CONTENT and block.
Return nil if the user accepted (plain `q'), or a string (possibly empty)
giving the rejection reason."
  (let* ((basename (if path (file-name-nondirectory path) "<new file>"))
         (original (generate-new-buffer (format "*gptel-original: %s*" basename)))
         (proposed (generate-new-buffer (format "*gptel-proposed: %s*" basename)))
         (decision (list nil nil))            ; (rejected-p reason)
         (saved-config (current-window-configuration))
         (quit-fn (lambda () (exit-recursive-edit))))
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
          (add-hook 'ediff-quit-hook quit-fn)
          (let ((ctrl (ediff-buffers original proposed)))
            (with-current-buffer ctrl
              (local-set-key
               (kbd smv/gptel-ediff-reject-key)
               (lambda ()
                 (interactive)
                 (let ((reason (read-string
                                (format "Reject change to %s — reason: " basename))))
                   (setcar decision t)
                   (setcar (cdr decision) reason)
                   (ediff-really-quit nil))))))
          (recursive-edit))
      (remove-hook 'ediff-quit-hook quit-fn)
      (when (buffer-live-p proposed) (kill-buffer proposed))
      (when (buffer-live-p original) (kill-buffer original))
      (set-window-configuration saved-config))
    (when (car decision)
      (or (cadr decision) ""))))

(defun smv/gptel-ediff-tool-call (plist)
  "Pre-tool-call hook: preview Edit/Write/Insert in ediff before running."
  (let ((name (plist-get plist :name)))
    (when (member name smv/gptel-ediff-tool-names)
      (when-let* ((args     (plist-get plist :args))
                  (proposed (smv/gptel-ediff--proposed name args))
                  (path     (car proposed))
                  (content  (cdr proposed)))
        (let ((reason (smv/gptel-ediff--review path content)))
          (cond
           ((null reason)
            ;; Accepted via ediff — skip gptel's normal y-or-n confirmation.
            (list :confirm nil))
           (t
            (list :block
                  (if (string-empty-p reason)
                      (format "Tool '%s' was rejected by the user during ediff review." name)
                    (format "THE USER HAS REJECTED THE TOOL CALL DURING EDIFF REVIEW.\nADJUST ACCORDINGLY.\nREASON:\n%s"
                            reason))))))))))

(add-hook 'gptel-pre-tool-call-functions #'smv/gptel-ediff-tool-call)

;;; ask.el --- GPTel tools for asking user questions  -*- lexical-binding: t -*-

(require 'gptel)
(require 'cl-lib)
(require 'map)

;; --- Helper Functions ---

(defun gptel-ask--get (obj key)
  "Get value of KEY from OBJ (handles alist, plist, or hash-table)."
  (cond
   ((hash-table-p obj) (gethash key obj))
   ((listp obj)
    ;; check if plist or alist
    (if (and (cdr obj) (symbolp (car obj)))
        (plist-get obj (intern (concat ":" key)))
      (alist-get (intern key) obj nil nil #'string=)))
   (t nil)))

(defun gptel-ask--word-wrap (text width)
  "Wrap TEXT to a given WIDTH."
  (if (not (stringp text)) ""
    (with-temp-buffer
      (insert text)
      (let ((fill-column width))
        (fill-paragraph nil))
      (buffer-string))))

(defun gptel-ask--block-bg ()
  "Return a background face suitable for displaying the question UI."
  (cond
   ((derived-mode-p 'org-mode) 'org-block)
   ((derived-mode-p 'markdown-mode) 'markdown-code-face)
   (t `(:background ,(face-attribute 'mode-line-inactive :background)
        :box (:line-width 2 :color "grey75" :style released-button)
        :extend t))))

(defun gptel-ask--overlay-at-point ()
  "Return the question overlay at point, if any."
  (seq-find (lambda (ov) (overlay-get ov 'gptel-ask))
            (overlays-at (point))))

(defun gptel-ask--make-keymap (choices)
  "Generate keymap for CHOICES interaction with number keys."
  (let ((map (make-sparse-keymap))
        (count (min 9 (length choices))))
    ;; Bind numbers 1-9
    (dotimes (i count)
      (let ((idx i)) ;; Capture loop variable for closure
        (define-key map (kbd (format "%d" (1+ i)))
          (lambda () (interactive) (gptel-ask--select-choice idx)))
        (define-key map (kbd (format "<kp-%d>" (1+ i)))
          (lambda () (interactive) (gptel-ask--select-choice idx)))))
    
    ;; Navigation and Confirmation
    (define-key map (kbd "RET") 'gptel-ask--confirm-choice)
    (define-key map (kbd "<return>") 'gptel-ask--confirm-choice)
    (define-key map (kbd "TAB") 'gptel-ask--cycle-choice)
    (define-key map (kbd "<tab>") 'gptel-ask--cycle-choice)
    (define-key map (kbd "n") 'gptel-ask--next-choice)
    (define-key map (kbd "p") 'gptel-ask--prev-choice)
    (define-key map (kbd "C-c C-k") 'gptel-ask--cancel)
    map))



;; --- UI Construction ---

(defun gptel-ask--draw-ui (question choices selection)
  "Return the UI string for QUESTION and CHOICES with SELECTION."
  (let* ((width (min (window-body-width) 80))
         (wrap-width (- width 4))
         (header (propertize (format " 🤖 AI ASKS: %s" 
                                     (gptel-ask--word-wrap question wrap-width))
                             'face 'font-lock-keyword-face))
         (choice-strs
          ;; Uses cl-loop which is standard in cl-lib
          (cl-loop for choice in choices
                   for idx from 0
                   collect
                   (let* ((selected (= idx selection))
                          (val (or (gptel-ask--get choice "value") "Unknown"))
                          (desc (gptel-ask--get choice "description"))
                          (mark (if selected " ● " " ○ "))
                          (face (if selected '(:inherit highlight :weight bold) 'default)))
                     (concat
                      (propertize mark 'face face)
                      (propertize (format "[%d] %s" (1+ idx) val) 'face face)
                      ;; Check for description existence without subr-x
                      (when (and desc (not (equal desc "")))
                        (concat "\n    " 
                                (propertize (gptel-ask--word-wrap desc wrap-width)
                                            'face 'font-lock-comment-face)))))))
         (footer (propertize "\n [RET] Confirm [n] next [p] prev [1-9] Select  [C-c C-k] Cancel"
                             'face '(:inherit shadow :height 0.8)))
         ;; Use mapconcat which is built-in
         (content (concat "\n" header "\n\n" (mapconcat #'identity choice-strs "\n") footer "\n")))
    
    ;; Apply block styling to the whole content
    (propertize content 'face (gptel-ask--block-bg))))

(defun gptel-ask--update-overlay (ov)
  "Redraw the overlay OV based on its current properties."
  (let* ((question (overlay-get ov 'gptel-ask--question))
         (choices (overlay-get ov 'gptel-ask--choices))
         (selection (overlay-get ov 'gptel-ask--selection))
         (new-text (gptel-ask--draw-ui question choices selection)))
    
    ;; We must modify the buffer text covered by the overlay to update appearance
    ;; because we are using a "covering" overlay, not just an after-string
    (let ((inhibit-read-only t)
          (beg (overlay-start ov))
          (end (overlay-end ov)))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert new-text)
        (move-overlay ov beg (point))))))

;; --- Interaction Handlers ---

(defun gptel-ask--select-choice (n)
  "Select choice N and update display."
  (interactive)
  (when-let ((ov (gptel-ask--overlay-at-point))
             (choices (overlay-get ov 'gptel-ask--choices)))
    (when (< n (length choices))
      (overlay-put ov 'gptel-ask--selection n)
      (gptel-ask--update-overlay ov)
      ;; Visual feedback in minibuffer
      (let* ((choice (nth n choices))
             (val (or (gptel-ask--get choice "value") "Option")))
        (message "Selected [%d]: %s" (1+ n) val)))))


(defun gptel-ask--cycle-choice (&optional prev)
  (interactive)
  (when-let* ((ov (gptel-ask--overlay-at-point))
              (len (length (overlay-get ov 'gptel-ask--choices)))
              (curr (overlay-get ov 'gptel-ask--selection)))
    (let ((next (mod (+ curr (if prev -1 1)) len)))
      (overlay-put ov 'gptel-ask--selection next)
      (gptel-ask--update-overlay ov))))

(defun gptel-ask--next-choice () (interactive) (gptel-ask--cycle-choice))
(defun gptel-ask--prev-choice () (interactive) (gptel-ask--cycle-choice t))

(defun gptel-ask--teardown (ov)
  "Remove the UI and overlay completely, restoring buffer state."
  (when (overlayp ov)
    (let ((inhibit-read-only t)
          ;; Get the true start/end we stored or the overlay bounds
          (beg (overlay-start ov))
          (end (overlay-end ov)))
      (when (and beg end)
        (delete-region beg end)))
    (delete-overlay ov)))


(defun gptel-ask--confirm-choice ()
  "Confirm selection and call callback."
  (interactive)
  (when-let* ((ov (gptel-ask--overlay-at-point))
              (callback (overlay-get ov 'gptel-ask--callback))
              (choices (overlay-get ov 'gptel-ask--choices))
              (sel-idx (overlay-get ov 'gptel-ask--selection))
              (choice (nth sel-idx choices))
              (val (gptel-ask--get choice "value")))
    
    (gptel-ask--teardown ov)
    
    ;; Handle "Other" specifically if needed, or just return value
    (if (string-match-p "\\b[Oo]ther\\b" val)
        (let ((custom (read-string "Please specify: ")))
          (funcall callback custom))
      (funcall callback val))))

(defun gptel-ask--cancel ()
  (interactive)
  (when-let ((ov (gptel-ask--overlay-at-point)))
    (when-let ((cb (overlay-get ov 'gptel-ask--callback)))
      (funcall cb "User cancelled interaction."))
    (gptel-ask--teardown ov)))

;; --- Main Tool Functions ---
(defun gptel-ask--question (callback question choices)
  "Implementation for ask_question tool with high-priority overlay."
  (let* ((choices-list (append choices nil))
         (ui-text (gptel-ask--draw-ui question choices-list 0))
         (inhibit-read-only t))
    
    (goto-char (point-max))
    
    (let ((start-pos (point)))
      (unless (bolp) (insert "\n"))
      (insert ui-text)
      (insert "\n") 
      
      (let ((ov (make-overlay start-pos (point))))
        (overlay-put ov 'gptel-ask t)
        (overlay-put ov 'gptel-ask--question question)
        (overlay-put ov 'gptel-ask--choices choices-list)
        (overlay-put ov 'gptel-ask--selection 0)
        (overlay-put ov 'gptel-ask--callback callback)
        (overlay-put ov 'keymap (gptel-ask--make-keymap choices-list))
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'face (gptel-ask--block-bg))
        
        (overlay-put ov 'priority 1000)
        
        (goto-char (overlay-start ov))
        (recenter)))))

(defun gptel-ask--multiple (callback questions)
  "Implementation for ask_multiple tool."
  (let* ((qs (append questions nil)) ;; Vector to list
         (results (make-hash-table :test 'equal))
         (total (length qs)))
    
    (cl-labels ((ask-next (idx)
                  (if (>= idx total)
                      ;; All done, format result as JSON string
                      (funcall callback
			       (format "User's Responses: %s"
				       (json-encode results)))
                    
                    (let* ((item (nth idx qs))
                           (q (gptel-ask--get item "question"))
                           (c (gptel-ask--get item "choices")))
                      (gptel-ask--question 
                       (lambda (ans)
                         (puthash q ans results)
                         (ask-next (1+ idx)))
                       q c)))))
      (ask-next 0))))

;; --- Tool Registration ---

(gptel-make-tool
 :name "ask_question"
 :async t
 :include nil
 :function #'gptel-ask--question
 :description "Ask the user a single question with predefined choices. The tool waits for user input. REQUIRED: 'choices' must contain 'value' keys."
 :args (list
        '(:name "question" :type string :description "The question text.")
        '(:name "choices" :type array :items 
          (:type object :properties 
                 (:value (:type string) :description (:type string))
                 :required ["value"]))))

(gptel-make-tool
 :name "ask_multiple"
 :async t
 :include nil
 :function #'gptel-ask--multiple
 :description "Ask a series of questions sequentially."
 :args (list
        '(:name "questions" :type array :items 
          (:type object :properties 
                 (:question (:type string)
                  :choices (:type array :items (:type object :properties (:value (:type string)))))))))

(provide 'gptel-ask)

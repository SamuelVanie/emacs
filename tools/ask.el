;;; ask.el --- GPTel tools for asking user questions  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This package provides tools for `gptel' that allow the LLM to
;; ask the user questions with multiple-choice answers directly
;; in the gptel buffer using overlays.

;;; Code:

(require 'gptel)
(require 'cl-lib)

;; --- Helper Functions ---

(defun gptel-word-wrap (text width)
  "Wrap TEXT to a given WIDTH."
  (let ((fill-column width))
    (with-temp-buffer
      (insert text)
      (fill-paragraph nil)
      (buffer-string))))

(defun gptel-ask--overlay-at-point ()
  "Return the question overlay at point, if any."
  (seq-find (lambda (ov) (overlay-get ov 'gptel-ask))
            (overlays-at (point))))

(defun gptel-ask--make-keymap (choices)
  "Generate keymap for CHOICES overlay interaction."
  (let ((map (make-sparse-keymap)))
    (dotimes (i (min 9 (length choices)))
      (let ((index i)) ; Capture index for the closure
        (define-key map (kbd (number-to-string (1+ i)))
          (lambda () (interactive) (gptel-ask--select-choice index)))))
    (define-key map (kbd "RET") 'gptel-ask--confirm-choice)
    (define-key map (kbd "<return>") 'gptel-ask--confirm-choice)
    (define-key map (kbd "TAB") 'gptel-ask--cycle-choice)
    (define-key map (kbd "<tab>") 'gptel-ask--cycle-choice)
    (define-key map (kbd "n") 'gptel-ask--next-choice)
    (define-key map (kbd "p") 'gptel-ask--prev-choice)
    (define-key map (kbd "C-c C-k") 'gptel-ask--cancel)
    map))

(defun gptel-ask--create-overlay (from to choices)
  "Create interactive overlay FROM TO with CHOICES."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'gptel-ask t)
    (overlay-put ov 'gptel-ask--choices choices)
    (overlay-put ov 'gptel-ask--selection 0)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'keymap (gptel-ask--make-keymap choices))
    ov))

(defun gptel-ask--update-display (ov)
  "Update visual display of choices in OV to reflect selection."
  (let* ((selection (overlay-get ov 'gptel-ask--selection))
         (buffer (overlay-buffer ov))
         (start (overlay-start ov)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (let ((choice-idx -1))
          ;; Search for the bullet character which is more robust
          (while (re-search-forward "^..│ \\([○●]\\)" (overlay-end ov) t)
            (setq choice-idx (1+ choice-idx))
            (replace-match (if (= choice-idx selection) "●" "○") t t nil 1)))))))

(defun gptel-ask--select-choice (n)
  "Select choice N in the overlay at point."
  (interactive)
  (when-let ((ov (gptel-ask--overlay-at-point)))
    (overlay-put ov 'gptel-ask--selection n)
    (gptel-ask--update-display ov)))

(defun gptel-ask--cycle-choice (&optional prev)
  "Cycle to next or previous choice in overlay."
  (interactive)
  (when-let* ((ov (gptel-ask--overlay-at-point))
              (choices (overlay-get ov 'gptel-ask--choices))
              (current (overlay-get ov 'gptel-ask--selection)))
    (let ((next (if prev
                    (mod (1- current) (length choices))
                  (mod (1+ current) (length choices)))))
      (overlay-put ov 'gptel-ask--selection next)
      (gptel-ask--update-display ov))))

(defun gptel-ask--next-choice ()
  "Move to next choice."
  (interactive)
  (gptel-ask--cycle-choice))

(defun gptel-ask--prev-choice ()
  "Move to previous choice."
  (interactive)
  (gptel-ask--cycle-choice t))

(defun gptel-ask--finalize-display (ov result)
  "Update the overlay to show it's completed and non-interactive."
  (let ((buffer (overlay-buffer ov))
        (end (overlay-end ov)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        ;; Make overlay non-interactive and grayed out
        (overlay-put ov 'keymap nil)
        (overlay-put ov 'face '(:foreground "gray50"))
        (save-excursion
          (goto-char end)
          (insert (propertize (format "\n  ✓ You selected: %s\n" result)
                              'face '(:foreground "green"))))))))

(defun gptel-ask--return-result (ov result)
  "Return RESULT from OV to the waiting tool callback."
  (when-let ((callback (overlay-get ov 'gptel-ask--callback)))
    (funcall callback result))
  (gptel-ask--finalize-display ov result))

(defun gptel-ask--confirm-choice ()
  "Confirm current selection and return result to LLM."
  (interactive)
  (when-let* ((ov (gptel-ask--overlay-at-point))
              (choices (overlay-get ov 'gptel-ask--choices))
              (selection (overlay-get ov 'gptel-ask--selection))
              (choice-text (nth selection choices)))
    (if (string-match-p "\\b[Oo]ther\\b" choice-text) ; Case-insensitive 'Other'
        (let ((custom (read-string "Enter custom response: ")))
          (gptel-ask--return-result ov custom))
      (gptel-ask--return-result ov choice-text))))

(defun gptel-ask--cancel ()
  "Cancel the question."
  (interactive)
  (when-let ((ov (gptel-ask--overlay-at-point)))
    (gptel-ask--return-result ov "User cancelled.")))


;; --- UI Drawing Function ---

(defun gptel-ask--draw-ui (question choices)
  "Draw the question and choices UI into the current buffer."
  (let ((indent "  "))
    ;; Draw the box
    (insert (format "%s┌%s┐\n" indent (make-string 60 ?─)))
    (insert (format "%s│ %s%s│\n" indent
                    (propertize "Question from AI" 'face 'font-lock-keyword-face)
                    (make-string (- 59 (length "Question from AI")) ?\s)))
    (insert (format "%s├%s┤\n" indent (make-string 60 ?─)))

    ;; Insert question, wrapped
    (let ((wrapped-question (gptel-word-wrap question 58)))
      (dolist (line (split-string wrapped-question "\n" t))
        (insert (format "%s│ %s%s │\n" indent line (make-string (- 58 (length line)) ?\s)))))

    (insert (format "%s├%s┤\n" indent (make-string 60 ?─)))

    ;; Insert choices
    (dotimes (i (length choices))
      (let* ((choice (nth i choices))
             (selected (= i 0))
             (prefix (format "%d. " (1+ i)))
             (line (format "%s%s" prefix choice))
             (wrapped-lines (gptel-word-wrap line (- 58 4)))
             (lines (split-string wrapped-lines "\n" t)))
        (insert (format "%s│ %s %s%s │\n" indent
                        (if selected "●" "○")
                        (propertize (nth 0 lines) 'face 'bold)
                        (make-string (- 56 (length (nth 0 lines))) ?\s)))
        (dolist (extra-line (cdr lines))
          (insert (format "%s│    %s%s │\n" indent extra-line (make-string (- 56 (length extra-line)) ?\s))))))

    (insert (format "%s└%s┘\n" indent (make-string 60 ?─)))
    (insert (format "%s%s\n\n" indent
                    (propertize "Select: 1-9, n/p, TAB. Confirm: RET. Cancel: C-c C-k"
                                'face 'font-lock-comment-face)))))


;; --- Tool Definitions ---

(defun gptel-ask--question (callback question choices)
  "Ask QUESTION with CHOICES, call CALLBACK with result."
  (let* ((choices-list (if (vectorp choices) (append choices nil) choices))
         (from (point)))
    ;; 1. Draw the UI
    (gptel-ask--draw-ui question choices-list)
    ;; 2. Create the interactive overlay over the UI
    (let ((ov (gptel-ask--create-overlay from (point) choices-list)))
      ;; 3. Store the async callback in the overlay for later retrieval
      (overlay-put ov 'gptel-ask--callback callback))))

(defun gptel-ask--multiple (callback questions-and-choices)
  "Ask multiple QUESTIONS-AND-CHOICES, call CALLBACK with results."
  (let* ((questions (if (vectorp questions-and-choices)
                        (append questions-and-choices nil)
                      questions-and-choices))
         (results (make-hash-table :test 'equal))
         (total (length questions)))
    (cl-labels ((ask-next (index)
                  (if (>= index total)
                      ;; All done - call final callback with results
                      (funcall callback (cl-loop for k being the hash-keys of results
                                                 using (hash-value v)
                                                 collect (list :question k :answer v)))
                    ;; Ask next question
                    (let* ((q-obj (nth index questions))
                           (question (or (alist-get 'question q-obj) (gethash "question" q-obj)))
                           (choices-vec (or (alist-get 'choices q-obj) (gethash "choices" q-obj)))
                           (choices (if (vectorp choices-vec) (append choices-vec nil) choices-vec)))
                      (gptel-ask--question
                       (lambda (answer)
                         (puthash question answer results)
                         (ask-next (1+ index)))
                       question
                       choices)))))
      (ask-next 0))))


;; --- Tool Registration ---

(gptel-make-tool
 :name "ask_question"
 :async t
 :include t
 :function #'gptel-ask--question
 :description "Ask the user a single question with predefined choices. The user will select one option from an interactive list in the buffer. Use this when you need specific user input to proceed. ALWAYS include a choice like 'None of the above' or 'Other (please specify)' for flexibility. The tool call will wait for the user to make a selection."
 :args (list
        '(:name "question"
          :type string
          :description "The question to ask the user. Should be clear and concise.")
        '(:name "choices"
          :type array
          :items (:type string)
          :description "An array of strings representing the choices. Limit to 9 for keyboard shortcuts. The last choice can be 'Other' to allow free-form input."))
 :category "user-interaction")

(gptel-make-tool
 :name "ask_multiple"
 :async t
 :include t
 :function #'gptel-ask--multiple
 :description "Ask the user a series of related questions sequentially. Each question is presented one at a time with its own set of choices. Use this for configurations, multi-step decisions, or gathering structured information from the user."
 :args (list
        '(:name "questions"
          :type array
          :items (:type object
                  :properties (:question (:type string
                                          :description "The text of the question.")
                               :choices (:type array
                                        :items (:type string)
                                        :description "An array of choice strings for this question."))
                  :required ["question" "choices"])
          :description "An array of question objects. Each object must have 'question' and 'choices' keys."))
 :category "user-interaction")


;; We are no longer using the preview system as the tool handles its own UI.
;; The `with-eval-after-load` block has been removed.

(provide 'gptel-ask)
;;; ask.el ends here

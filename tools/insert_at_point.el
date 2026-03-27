(defun insert-at-register-a (text)
  "Insert TEXT at the position saved in register 'a'."
  (let ((marker (get-register ?a)))
    (when (markerp marker)
      (let ((buffer (marker-buffer marker))
            (position (marker-position marker)))
        (when (and buffer position)
          (with-current-buffer buffer
            (save-excursion
              (goto-char position)
              (insert text))))))))

(defun get-register-a-context ()
  "Return details about the point saved in register 'a' for agent context.
The return value is a property list containing :file, :buffer,
:line, :column, and :content (the line text at the marker)."
  (let ((marker (get-register ?a)))
    (if (and (markerp marker)
             (marker-buffer marker)
             (marker-position marker))
        (let* ((buf (marker-buffer marker))
               (pos (marker-position marker)))
          (with-current-buffer buf
            (save-excursion
              (goto-char pos)
              (list :file (or (buffer-file-name buf) (buffer-name buf))
                    :buffer (buffer-name buf)
                    :line (line-number-at-pos)
                    :column (current-column)
                    :point pos
                    :content (string-trim (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position)))))))
      (error "Register 'a' does not contain a valid marker position"))))




(gptel-make-tool
 :name "insert_at_point"
 :function #'insert-at-register-a
 :description "Insert text at the position required by the user. You should always start by calling the get_point_info tool before to have the full context about the position of the user's cursor where you'll be writing"
 :args (list '(:name "text"
                     :type string
                     :description "The text to insert at the position."))
 :category "emacs")

(gptel-make-tool
 :name "get_point_info"
 :function #'get-register-a-context
 :description "Get details (file, line, content) about the position of the point in project."
 :args nil
 :category "emacs")

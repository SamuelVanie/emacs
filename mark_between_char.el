(defun smv/mark-between-chars (chars)
    (interactive "sEnter two characters (e.g. \"()\"): ")
    (unless (= (length chars) 2)
      (user-error "Please enter exactly two characters"))

    (let* ((char1 (aref chars 0))
           (char2 (aref chars 1))
           (line-start (save-excursion
                         (beginning-of-visual-line)
                         (point)))
           (line-end (save-excursion
                       (end-of-visual-line)
                       (point)))
           ;; Try forward search for first char
           (forward-char1-pos (save-excursion
                                (search-forward (char-to-string char1) line-end t)))
           ;; If first char found, search for second char
           (forward-char2-pos (when forward-char1-pos
                                (save-excursion
                                  (goto-char forward-char1-pos)
                                  (search-forward (char-to-string char2) line-end t))))
           ;; Try backward search for second char
           (backward-char2-pos (unless forward-char1-pos
                                 (save-excursion
                                   (search-backward (char-to-string char2) line-start t))))
           ;; If second char found backward, search for first char backward
           (backward-char1-pos (when backward-char2-pos
                                 (save-excursion
                                   (goto-char backward-char2-pos)
                                   (search-backward (char-to-string char1) line-start t))))
           ;; Try backward search for first char from point
           (middle-char1-pos (save-excursion
                               (search-backward (char-to-string char1) line-start t)))
           ;; Try forward search for second char from point
           (middle-char2-pos (save-excursion
                               (search-forward (char-to-string char2) line-end t))))
      
      (cond
       ;; Middle case: found first char backward and second char forward
       ((and middle-char1-pos middle-char2-pos
             (< middle-char1-pos (point))
             (> middle-char2-pos (point)))
        (goto-char (1+ middle-char1-pos))
        (push-mark (point) t t)
        (goto-char (1- middle-char2-pos)))
       
       ;; Forward case: found both chars
       ((and forward-char1-pos forward-char2-pos)
        (goto-char forward-char1-pos)
        (push-mark (point) t t)
        (goto-char (1- forward-char2-pos)))
       
       ;; Backward case: found both chars
       ((and backward-char1-pos backward-char2-pos)
        (goto-char backward-char2-pos)  ; Move to end of second char
        (push-mark (point) t t)
        (goto-char (1+ backward-char1-pos)))  ; Move to start of first char
       
       ;; Neither char found
       ((not (or forward-char1-pos backward-char2-pos middle-char1-pos middle-char2-pos))
        (message "Neither '%c' nor '%c' found on current visual line" char1 char2))
       
       ;; First char found forward but no second char
       (forward-char1-pos
        (message "Found '%c' but no '%c' after it on current visual line" char1 char2))
       
       ;; Second char found backward but no first char
       (backward-char2-pos
        (message "Found '%c' but no '%c' before it on current visual line" char2 char1))
       
       ;; Other cases where pairs weren't found in the expected order
       (t
        (message "No matching pair of '%c' and '%c' found in the expected order" char1 char2)))))

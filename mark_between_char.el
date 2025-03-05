(defun smv/mark-between-chars ()
  "Mark the region between matching brackets, excluding the brackets themselves.
If the point is on a character in `smv/left-brackets', use `forward-sexp' to find the matching closing bracket and exclude both brackets.
If the point is on a character in `smv/right-brackets', use `backward-sexp' to find the matching opening bracket and exclude both brackets.
Otherwise, prompt the user for two characters to define the region."
  (interactive)

  (let ((smv/left-brackets '("[" "("))
        (smv/right-brackets '("]" ")")))
        ;; Define the bracket lists
    (unless (and (boundp 'smv/left-brackets) (boundp 'smv/right-brackets))
      (error "Please define `smv/left-brackets' and `smv/right-brackets' before using this function"))

    (let* ((current-char (char-after))  ; Get the character at the point
           (current-char-str (if current-char (char-to-string current-char) "")))

      ;; Check if the current character is in smv/left-brackets
      (if (and current-char (member current-char-str smv/left-brackets))
          (progn
            ;; Save the starting point after the opening bracket
            (let ((start-point (1+ (point))))
              ;; Move forward to the closing bracket
              (condition-case nil
                  (progn
                    (forward-sexp)               ; Move to the closing bracket
                    (let ((end-point (1- (point)))) ; Save the ending point before the closing bracket
                      (goto-char start-point)     ; Go back to the start point
                      (push-mark end-point t t)   ; Set the mark at the end point
                      (goto-char start-point)))   ; Return to the start point
                (error (message "No matching closing bracket found")))))

        ;; Check if the current character is in smv/right-brackets
        (if (and current-char (member current-char-str smv/right-brackets))
            (progn
              ;; Save the ending point before the closing bracket
              (let ((end-point (1- (point))))
                ;; Move backward to the opening bracket
                (condition-case nil
                    (progn
                      (backward-sexp)             ; Move to the opening bracket
                      (let ((start-point (1+ (point)))) ; Save the starting point after the opening bracket
                        (goto-char end-point)      ; Go back to the end point
                        (push-mark start-point t t) ; Set the mark at the start point
                        (goto-char end-point)))    ; Return to the end point
                  (error (message "No matching opening bracket found")))))

          ;; If the current character is not in either list, prompt the user
          (let* ((chars (read-string "Enter two characters (e.g. \"()\"): "))
                 (char1 (aref chars 0))
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
              (goto-char (1+ forward-char1-pos))
              (push-mark (point) t t)
              (goto-char (1- forward-char2-pos)))

             ;; Backward case: found both chars
             ((and backward-char1-pos backward-char2-pos)
              (goto-char (1+ backward-char1-pos))
              (push-mark (point) t t)
              (goto-char (1- backward-char2-pos)))

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
              (message "No matching pair of '%c' and '%c' found in the expected order" char1 char2)))))))))

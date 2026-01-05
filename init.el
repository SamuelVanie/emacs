(setq gc-const-threshold (* 90 1000 1000))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(setopt use-short-answers t)
(setq delete-selection-temporary-region t)

(setq warning-minimum-level :error)
(global-hl-line-mode 1)

;; an indicator that will help in not writing really long lines
;; while programming
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-character ?\u2588)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq enable-recursive-minibuffers t)

(cond
 ((eq system-type 'darwin)  ;; macOS
  (setq read-process-output-max (* 64 1024)))  ;; 64KB
 ((eq system-type 'gnu/linux)  ;; Linux
  (setq read-process-output-max (* 1024 1024)))  ;; 1MB
 )

(cond
 ((eq system-type 'darwin)
  (defvar smv/default-font-size 180)
  (defvar smv/default-variable-font-size 180))
 ((eq system-type 'gnu/linux)
  (defvar smv/default-font-size 139)
  (defvar smv/default-variable-font-size 139))
 )

;; remove noise for non allowed command in emacs if your system make them
(setq ring-bell-function 'ignore)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; prevent emacs from hanging on when long lines are present in the current file
(global-so-long-mode t)

;; auto refresh buffers when files changes
(global-auto-revert-mode t)
(global-visual-line-mode t)

;; Prevent dired-find-alternative warning message
(put 'dired-find-alternate-file 'disabled nil)

;; to scroll down inside the popup
(define-key global-map (kbd "C-M-'")
            (lambda ()
              (interactive)
              (scroll-other-window 2)))

;; to scroll up side the popup
(define-key global-map (kbd "C-M-\"")
            (lambda ()
              (interactive)
              (scroll-other-window-down 2)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)

(setq read-file-name-completion-ignore-case t)
(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-expand-line-all-buffers
        ))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (global-treesit-fold-mode))

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :straight t
      :init
      (exec-path-from-shell-initialize)))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-list-files-after-cd t)


(with-eval-after-load 'eshell
  (if (eq system-type 'android)
      (defalias 'eshell/manjaro-exec
	(lambda (&rest args)
	  (eshell-command-result
	   (format "proot-distro login manjaro --bind %s:/root --bind /sdcard:/sdcard --isolated -- sh -c 'cd %s && %s'"
		   (getenv "HOME")
		   (string-replace (getenv "HOME") "/root" (eshell/pwd))
		   (string-join args " ")))))))

(setq eshell-aliases-file (format "%s%s" user-emacs-directory "aliases"))
(global-set-key (kbd "C-c e") 'eshell)

;; this will make emacs ibuffer the default used to list buffers
(defalias 'list-buffers 'ibuffer)

;; Ibuffer appearance configuration
(setq ibuffer-use-other-window t)

(defun my-both-modes-active-p (buffer-name action)
  "Return non-nil if buffer is org-mode with gptel active."
  (with-current-buffer buffer-name
    (and (derived-mode-p 'org-mode)
         (bound-and-true-p gptel-mode))))

(add-to-list 'display-buffer-alist
             '("\\*Ibuffer\\*"
               (display-buffer-in-side-window)
               (window-height . 0.4)     ;; Takes 40% of the frame height
               (side . bottom)           ;; Display at bottom
               (slot . 0)))

(defun kill-all-buffers ()
  "Kill all buffers without asking for confirmation."
  (interactive)
  (dolist (buffer (buffer-list))
    (kill-buffer buffer)))

(global-set-key (kbd "C-c k a") 'kill-all-buffers)
(global-set-key (kbd "C-k") 'kill-line)

(use-package popper
  :straight t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("C-M-`"   . popper-cycle)
         ("M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
	  agent-shell-mode
          (lambda (buf) (with-current-buffer buf
                          (bound-and-true-p gptel-mode)))))
  (setq popper-window-height "40")
  (setq popper-display-control nil)
  ;; Match eshell, shell, term and/or vterm buffers
  (setq popper-reference-buffers
	(append popper-reference-buffers
		'("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
		  "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
		  "^\\*term.*\\*$"   term-mode   ;term as a popup
		  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                  "^\\*eat.*\\*$"    eat-mode    ;eat as a popup
                  )))

  (popper-mode +1)
  (popper-echo-mode +1))

(scroll-bar-mode -1) ; Disable visible scroll bar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable menu bar

(column-number-mode)
(global-display-line-numbers-mode t) ;; print line numbers for files


;; Set frame transparency
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))



;; some modes doesn't have to start with lines enable
(dolist (mode '(
                term-mode-hook
                doc-view-minor-mode-hook
                gptel-mode-hook
		org-mode-hook
                shell-mode-hook
                dired-mode-hook
                vterm-mode-hook
		agent-shell-mode-hook
                eat-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () 
                   (display-line-numbers-mode 0)
                   (setq-local global-hl-line-mode nil))))

(if (eq system-type 'darwin)
    (progn
      (set-frame-font "VictorMono Nerd Font-19" nil t)
      (set-face-attribute 'fixed-pitch nil :family "FantasqueSansM Nerd Font Mono"))
  (add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-15"))
  (set-face-attribute 'fixed-pitch nil :family "FantasqueSansM Nerd Font"))

(set-face-attribute 'variable-pitch nil :family "Iosevka Nerd Font")
;; (set-face-attribute 'variable-pitch nil :family "FantasqueSansM Nerd Font")

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Indentation based on the indentation of the previous non-blank line.
(setq-default indent-line-function #'indent-relative-first-indent-point)

;; In modes such as `text-mode', pressing Enter multiple times removes
;; the indentation. The following fixes the issue and straights that text
;; should never add tabs but spaces
(setq indent-tabs-mode nil)

;; is properly indented using `indent-relative' or
;; `indent-relative-first-indent-point'.
(setq-default indent-line-ignored-functions '())
(global-set-key (kbd "C-<tab>") 'tab-to-tab-stop)

(use-package hydra
  :straight t) ;; hydra permit to repeat a command easily without repeating the keybindings multiple
(use-package general
  :straight t) ;; permit to define bindings under another one easily

(use-package repeat
  :straight nil
  :hook (after-init . repeat-mode)
  :custom
  (repeat-too-dangerous '(kill-this-buffer))
  (repeat-exit-timeout 5))

(defun smv/remove-overlays ()
  (interactive)
  (remove-overlays))

(defun smv/surround-with-pair (open close)
  "Surround the active region or insert pair at point using insert-pair.
        OPEN is the opening character, CLOSE is the closing character."
  (if (use-region-p)
      (insert-pair 1 open close)
    (insert-pair nil open close)))

(with-eval-after-load 'hydra
  (defhydra hydra-surround (:color blue :hint nil)
    "

    ^Quotes^          ^Brackets^        ^Symbols^         ^Custom^
    ^^^^^^^^--------------------------------------------------------
    _\"_: double       _(_: parentheses  _<_: angles       _c_: custom pair
    _'_: single        _[_: square       _`_: backticks    _=_: equals
    _~_: tilde         _{_: curly        _*_: asterisks    _s_: custom strings
                                         _+_: plus	     
    "
    ("\"" (smv/surround-with-pair "\"" "\""))
    ("'" (smv/surround-with-pair "'" "'"))
    ("~" (smv/surround-with-pair "~" "~"))
    ("(" (smv/surround-with-pair "(" ")"))
    ("[" (smv/surround-with-pair "[" "]"))
    ("{" (smv/surround-with-pair "{" "}"))
    ("<" (smv/surround-with-pair "<" ">"))
    ("`" (smv/surround-with-pair "`" "`"))
    ("*" (smv/surround-with-pair "*" "*"))
    ("=" (smv/surround-with-pair "=" "="))
    ("+" (smv/surround-with-pair "+" "+"))
    ("s" (let ((open (read-string "Opening string: "))
               (close (read-string "Closing string: ")))
           (smv/surround-with-pair open close)))
    ("q" nil "quit"))
  )


(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)


  (meow-motion-define-key
   '("<escape>" . ignore)
   '("e" . meow-next)
   '("u" . meow-prev)
   '("n" . meow-left)
   '("s" . meow-insert)
   '("i" . meow-right)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("S" . hydra-surround/body)
   '("C" . meow-pop-to-mark)
   '("V" . meow-unpop-to-mark)
   '("<" . previous-buffer)
   '("<home>" . meow-temp-normal)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '(">" . next-buffer)
   '("K" . kill-current-buffer)
   )

  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("SPC" . smv/remove-overlays))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("}" . forward-paragraph)
   '(")" . forward-sexp)
   '("{" . backward-paragraph)
   '("(" . backward-sexp)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-pop-to-mark)
   '("d" . duplicate-line)
   '("D" . delete-pair)
   '("e" . meow-next)        ;; Down (next line)
   '("E" . meow-next-expand)
   '("f" . meow-find)
   '("F" . yank-media)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("i" . meow-right) ;; Right (forward char)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("J" . meow-undo)
   '("k" . meow-kill)
   '("K" . kill-current-buffer)
   '("l" . meow-line)
   '("L" . meow-visual-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-left) ;; Left (backward char)
   '("N" . meow-left-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-insert)
   '("S" . hydra-surround/body)
   '("t" . meow-till)
   '("u" . meow-prev)        ;; Up (prev line)
   '("U" . meow-prev-expand)        
   '("v" . meow-search)
   '("V" . meow-unpop-to-mark)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("Y" . copy-from-above-command)
   '("z" . meow-pop-selection)
   '("<" . previous-buffer)
   '(">" . next-buffer)
   '("<escape>" . ignore)))

(defun meow--tag-find-bounds ()
  "Find bounds of the current HTML/XML tag.
Returns (BEG . END) cons cell or nil if not found."
  (save-excursion
    (let ((start-pos (point))
          opening-tag-start
          opening-tag-end
          closing-tag-start
          closing-tag-end
          tag-name)
      
      ;; Try to find if we're inside a tag
      ;; First, look backward for opening tag
      (when (re-search-backward "<\\([[:alnum:]:_-]+\\)[^>]*>" nil t)
        (setq opening-tag-start (match-beginning 0))
        (setq opening-tag-end (match-end 0))
        (setq tag-name (match-string 1))
        
        ;; Now search forward for the matching closing tag
        (let ((depth 1))
          (goto-char opening-tag-end)
          (while (and (> depth 0)
                      (re-search-forward 
                       (concat "</?\\(" (regexp-quote tag-name) "\\)[^>]*>") 
                       nil t))
            (if (string= (substring (match-string 0) 0 2) "</")
                (setq depth (1- depth))
              ;; Check if it's not a self-closing tag
              (unless (string= (substring (match-string 0) -2) "/>")
                (setq depth (1+ depth))))
            (when (= depth 0)
              (setq closing-tag-end (match-end 0))
              (setq closing-tag-start (match-beginning 0)))))
        
        ;; Verify the original position is within bounds
        (when (and closing-tag-end
                   (>= start-pos opening-tag-start)
                   (<= start-pos closing-tag-end))
          (cons opening-tag-start closing-tag-end))))))

(defun meow--tag-find-inner ()
  "Find inner bounds of the current HTML/XML tag (content only).
Returns (BEG . END) cons cell or nil if not found."
  (save-excursion
    (let ((start-pos (point))
          opening-tag-start
          opening-tag-end
          closing-tag-start
          tag-name)
      
      ;; Find opening tag
      (when (re-search-backward "<\\([[:alnum:]:_-]+\\)[^>]*>" nil t)
        (setq opening-tag-start (match-beginning 0))
        (setq opening-tag-end (match-end 0))
        (setq tag-name (match-string 1))
        
        ;; Find matching closing tag
        (let ((depth 1))
          (goto-char opening-tag-end)
          (while (and (> depth 0)
                      (re-search-forward 
                       (concat "</?\\(" (regexp-quote tag-name) "\\)[^>]*>") 
                       nil t))
            (if (string= (substring (match-string 0) 0 2) "</")
                (setq depth (1- depth))
              (unless (string= (substring (match-string 0) -2) "/>")
                (setq depth (1+ depth))))
            (when (= depth 0)
              (setq closing-tag-start (match-beginning 0)))))
        
        ;; Verify position is within bounds and return inner content
        (when (and closing-tag-start
                   (>= start-pos opening-tag-end)
                   (<= start-pos closing-tag-start))
          (cons opening-tag-end closing-tag-start))))))

(use-package meow
  :straight t
  :after hydra
  :config
  (add-to-list 'meow-char-thing-table '(?t . tag))
  (meow-thing-register 'tag
                       'meow--tag-find-inner
                       'meow--tag-find-bounds)
  (meow-setup)
  ;; remove those hints that clutter vision
  (setq meow-expand-hint-remove-delay 0)
  (meow-global-mode 1))

;; tabs manipulations
(with-eval-after-load 'general    
  
  ;; Some more complex commands
  (general-define-key
   :keymaps 'meow-normal-state-keymap
   :prefix "%"
   "s" #'scratch-buffer
   "e" #'split-window-below
   "i" #'split-window-right)

  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "="
   "="		#'indent-region
   "SPC"		#'point-to-register
   "+"		#'increment-register
   "M"		#'bookmark-set-no-overwrite
   "N"		#'rectangle-number-lines
   "b"		#'bookmark-jump
   "c"		#'clear-rectangle
   "d"		#'delete-rectangle
   "f"		#'frameset-to-register
   "g"		#'insert-register
   "i"		#'insert-register
   "j"		#'jump-to-register
   "k"		#'kill-rectangle
   "l"		#'bookmark-bmenu-list
   "m"		#'bookmark-set
   "n"		#'number-to-register
   "o"		#'open-rectangle
   "r"		#'copy-rectangle-to-register
   "s"		#'copy-to-register
   "t"		#'string-rectangle
   "w"		#'window-configuration-to-register
   "x"		#'copy-to-register
   "y"		#'yank-rectangle
   "M-w"	#'copy-rectangle-as-kill
   )

  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "~ s"
   "n" #'smerge-vc-next-conflict
   "u" #'smerge-keep-upper
   "l" #'smerge-keep-lower
   "b" #'smerge-keep-all
   "r" #'smerge-refine
   "s" #'smerge-resolve)
  )

(winner-mode 1) ;; activate the package that recalls the current layout to redo it

(use-package windmove
  :after meow
  :straight nil
  :config
  (setq windmove-wrap-around t)
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "$"
   "n"  #'windmove-left
   "i"  #'windmove-right
   "e"  #'windmove-down
   "u"  #'windmove-up
   "x"  #'kill-buffer-and-window
   "$" #'delete-window
   "+"  #'balance-windows
   "m"  #'maximize-window
   "s n"  #'windmove-swap-states-left
   "s i"  #'windmove-swap-states-right
   "s e"  #'windmove-swap-states-down
   "s u"  #'windmove-swap-states-up
   "d n"  #'windmove-delete-left
   "d i"  #'windmove-delete-right
   "d e"  #'windmove-delete-down
   "d u"  #'windmove-delete-up)
  )

(use-package winum
  :straight t
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8))
  :config
  (winum-mode))

(use-package vterm
  :defer t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-copy-exclude-prompt t))

(use-package eat
  :demand t
  :straight (eat :type git
		 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :bind
  ("<f7>" . eat)
  :config
  ;; Better scrollback management
  (setq eat-kill-buffer-on-exit t))

;; android configuration
(cond ((eq system-type 'darwin) ; macOS
       (setq browse-url-browser-function 'browse-url-default-macosx-browser
             browse-url-default-macosx-application "Microsoft Edge"))
      ((eq system-type 'android)
       (defun smv/termux-copy-url-to-clipboard (url &optional _new-window)
	 "Copy URL to Android clipboard via termux-clipboard-set."
	 (interactive "sURL: ")
	 (let ((process-connection-type nil)) ; Use pipes instead of pty
	   (with-temp-buffer
	     (insert url)
	     (call-process-region (point-min) (point-max)
				  "termux-clipboard-set"
				  nil 0 nil)))
	 (message "URL copied to clipboard: %s" url))

       (setq browse-url-browser-function 'smv/termux-copy-url-to-clipboard)
       )
      ((eq system-type 'gnu/linux)
       (setq browse-url-generic-program "microsoft-edge-stable")))

(defun smv/browse-search ()
  "Unified search across multiple websites."
  (interactive)
  (let* ((sites '(("Bing" . "https://www.bing.com/search?q=")
                  ("Google" . "https://www.google.com/search?q=")
                  ("YouTube" . "https://www.youtube.com/results?search_query=")
                  ("Wikipedia" . "https://en.wikipedia.org/wiki/Special:Search?search=")
                  ("NixSearch" . "https://search.nixos.org/packages?from=0&size=50&sort=relevance&type=packages&query=")
                  ("Reddit" . "https://www.reddit.com/search/?q=")
                  ("Stack Overflow" . "https://stackoverflow.com/search?q=")
                  ("GitHub" . "https://github.com/search?q=")))
         (site (completing-read "Choose a site: " (mapcar #'car sites)))
         (query (read-string (format "%s search: " site)))
         (url (cdr (assoc site sites))))
    (browse-url-generic (concat url (url-hexify-string query)))))

(global-set-key (kbd "C-c b") 'smv/browse-search)

(use-package expand-region
  :straight t
  :config
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   "*" #'er/expand-region)
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "'"
   "o" #'er/mark-outside-pairs
   "i" #'er/mark-inside-pairs
   "u" #'er/mark-url
   "n" #'er/mark-ts-node
   "d" #'er/mark-method-call
   "p" #'er/mark-paragraph
   "'" #'er/mark-inside-quotes
   "\"" #'er/mark-outside-quotes
   "m" #'er/mark-email))

(use-package drag-stuff
  :straight t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package dired
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("p" "~/projects/"		  "Projects")
     ("s" "/ssh:my-remote-server"      "SSH server")
     ("e" "/sudo:root@localhost:/etc"  "Modify program settings")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f f" . dirvish-side)
   ("C-c f d" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("q"   . dirvish-quit)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

(use-package vim-tab-bar
  :straight t
  :after general
  :commands vim-tab-bar-mode
  :hook
  (tab-bar-mode . vim-tab-bar-mode)
  :config
  (general-define-key
   :keymaps 'meow-normal-state-keymap
   :prefix "#"
   "l" #'tab-bar-new-tab
   "n" #'tab-bar-switch-to-prev-tab
   "i" #'tab-bar-switch-to-next-tab
   "u" #'tab-bar-close-tab))

(use-package kirigami
  :straight t
  :bind
  ("C-c k o" . kirigami-open-fold)
  ("C-c k O" . kirigami-open-fold-rec)
  ("C-c k m" . kirigami-close-folds)
  ("C-c k c" . kirigami-close-fold)
  ("C-c k r" . kirigami-open-folds)
  ("C-c k TAB" . kirigami-toggle-fold))

(use-package multiple-cursors
  :straight t
  :after (general hydra)
  :config
  (setq mc/always-run-for-all t)
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 ^Mark^			^Skip^				^Edit^			^Unmark^
 ^^^^^^^^-------------------------------------------------------------------------------------------
 _>_: next like this        _i_: skto next		 _+_: edit lines	   _u_: unmark previous
 _<_: prev like this        _n_: skto prev	         _=_: mark all		   _e_: unmark next
 _g_: mark regexp in region _q_: quit		  
 "
    ("+" mc/edit-lines)
    (">" mc/mark-next-like-this)
    ("<" mc/mark-previous-like-this)
    ("=" mc/mark-all-like-this)
    ("i" mc/skip-to-next-like-this)
    ("n" mc/skip-to-previous-like-this)
    ("u" mc/unmark-previous-like-this)
    ("e" mc/unmark-next-like-this)
    ("g" mc/mark-all-in-region-regexp)
    ("q" nil))
  (define-key mc/keymap (kbd "<return>") nil)
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   "+" #'hydra-multiple-cursors/body)
  (setq mc/cmds-to-run-once
        (append mc/cmds-to-run-once
                '(hydra-multiple-cursors/body
                  hydra-multiple-cursors/mc/edit-lines
                  hydra-multiple-cursors/mc/mark-next-like-this
                  hydra-multiple-cursors/mc/unmark-next-like-this
                  hydra-multiple-cursors/mc/mark-previous-like-this
                  hydra-multiple-cursors/mc/unmark-previous-like-this
                  hydra-multiple-cursors/mc/mark-all-like-this
                  hydra-multiple-cursors/mc/skip-to-next-like-this
                  hydra-multiple-cursors/mc/skip-to-previous-like-this))))

(use-package doom-themes
  :straight t)
(use-package ef-themes
  :straight t)
(use-package standard-themes
  :straight t)
(use-package kaolin-themes
  :straight t
  :config
  (kaolin-treemacs-theme)
  (load-theme 'kaolin-ocean))
(use-package catppuccin-theme
  :straight t)
(use-package solo-jazz-theme
  :straight t)
(use-package stimmung-themes
  :straight (stimmung-themes :type git :host github :repo "motform/stimmung-themes" :files ("*.el")))
(use-package rebecca-theme
  :straight t)

(use-package pink-bliss-uwu-theme
  :straight (pink-bliss-uwu-theme :type git :host github :repo "themkat/pink-bliss-uwu" :files ("*.el")))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package nerd-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons)

;; to install emoji rendering in emacs
;; some external packages to install are : fonts-noto-color-emoji and fonts-emojione on ubuntu
;; noto-fonts-emoji and ttf-joypixels on archlinux
(use-package unicode-fonts
  :straight t
  :config (unicode-fonts-setup))

(use-package which-key ;; print next keybindings
  :straight t
  :diminish which-key-mode
  :config ;; only runs after the mode is loaded
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

(use-package helm
  :straight t
  :demand t
  :after general
  :bind
  ("M-x" . helm-M-x)
  ("C-s" . helm-occur)
  :config
  (setq helm-mode-fuzzy-match t)
  (setq helm-full-frame nil)
  (setq helm-split-window-inside-p t)
  (setq helm-always-two-windows nil)
  (setq helm-completion-in-region-fuzzy-match t)
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "^"
   "#" #'helm-show-kill-ring
   "i" #'helm-imenu
   "t" #'helm-magit-todos
   "c" #'smv/helm-zoxide-cd)
  (helm-mode)
  :bind
  (
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("M-g h o" . helm-occur-mode)
   ("M-g h c" . smv/helm-zoxide-cd)
   ("M-g h m" . helm-mark-ring)
   ("M-g h k" . helm-show-kill-ring)
   ("M-g h s" . helm-do-grep-ag))
  )

(use-package wgrep-helm
  :straight t
  :after helm)

(defun smv/helm-zoxide-candidates ()
  "Generate a list of zoxide query candidates."
  (when-let ((zoxide (executable-find "zoxide")))
    (with-temp-buffer
      (call-process zoxide nil t nil "query" "-l")
      (split-string (buffer-string) "\n" t))))


(defun smv/zoxide-add-path (path-to-add)
  "Internal helper to add a given PATH-TO-ADD to zoxide.
  Returns t on success, nil on failure."
  (let ((expanded-path (expand-file-name path-to-add)))
    (if (file-directory-p expanded-path)
        (progn
          (call-process (executable-find "zoxide") nil nil nil "add" expanded-path)
          (message "Added '%s' to zoxide." expanded-path)
          t)
      (message "'%s' is not a valid directory." expanded-path)
      nil)))

(defun smv/zoxide-add-prompt-directory ()
  "Prompt for a directory and add it to zoxide."
  (interactive)
  (if-let ((zoxide (executable-find "zoxide")))
      (let ((dir (read-directory-name "Directory to add to zoxide: " default-directory nil t)))
        (when dir ; User didn't cancel
          (smv/zoxide-add-path dir)))
    (message "zoxide executable not found.")))

(defun smv/helm-zoxide-source ()
  "Create a Helm source for zoxide directories."
  (helm-build-sync-source "Zoxide Directories"
    :candidates #'smv/helm-zoxide-candidates
    :fuzzy-match t
    :action (helm-make-actions
             "Change Directory" 
             (lambda (candidate)
               (dired candidate)
               (message "Changed directory to %s" candidate)))))

(defun smv/helm-zoxide-cd ()
  "Use Helm to interactively select and change to a zoxide directory."
  (interactive)
  (if (executable-find "zoxide")
      (helm :sources (smv/helm-zoxide-source)
            :buffer "*helm zoxide*")))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package org ;; org-mode, permit to take notes and other interesting stuff with a specific file extension
  :straight t
  :config
  (setq org-agenda-files
        '("~/.org/todo.org"
          "~/.org/projects.org"))

  ;; easily move task to another header
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("todo.org" :maxlevel . 1)
          ("projects.org" :maxlevel . 1)
          ("personal.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "M-i") 'org-insert-item))

(use-package phscroll
  :straight (phscroll :type git :host github :repo "misohena/phscroll" :files ("*.el"))
  :after org
  :commands (org-phscroll-mode)
  :config
  (setq org-startup-truncated nil))

(use-package org-modern
  :straight t
  :after org
  :config
  (modify-all-frames-parameters
   '((right-divider-width . 30)
     (internal-border-width . 30)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-agenda-tags-column 0
   org-ellipsis "…")

  (global-org-modern-mode))

(use-package denote
  :after org
  :straight t
  :config
  (setq denote-directory "~/.org/notes"))

(use-package denote-journal
  :straight t
  :after denote)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("ru" . "src rust")))

(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))

(use-package ox-typst
  :straight t
  :defer t
  :after org)

(use-package ox-pandoc
  :straight t
  :defer t
  :after org)

;; Automatically tangle our Emacs.org config file when we save it
(defun smv/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name (format "%s%s" user-emacs-directory "emacs.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'smv/org-babel-tangle-config)))

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Straight ANSI colors work properly in shell-mode too
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode)
  :config
  ;; Set a dedicated directory for undo-tree files
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  ;; Create the directory if it doesn't exist
  (unless (file-exists-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo" t)))
;; Enable global undo-tree mode

;; Memory management for all terminal modes
(setq comint-buffer-maximum-size 5000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; Recovery function when corruption occurs
(defun smv/fix-terminal-corruption ()
  "Fix visual corruption in terminal buffers."
  (interactive)
  (when (derived-mode-p 'vterm-mode 'eat-mode 'term-mode)
    (recenter)
    (redraw-display)
    (set-char-table-range char-width-table '(#x1fb00 . #x1fbf9) 1)
    (when (derived-mode-p 'vterm-mode)
      (vterm-clear-scrollback))
    (message "Terminal corruption fixes applied")))

(global-set-key (kbd "C-c t r") 'smv/fix-terminal-corruption)

;; Store all backup files in a centralized directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Create the directory if it doesn't exist
(unless (file-exists-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups" t))

;; Enable auto-save
(setq auto-save-default t)

;; Store all auto-save files in a centralized directory
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Create the directory if it doesn't exist
(unless (file-exists-p "~/.emacs.d/auto-save-list")
  (make-directory "~/.emacs.d/auto-save-list" t))

(global-set-key (kbd "C-M-;") 'comment-region)

(use-package mason
  :straight t
  :config
  (mason-ensure))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (general-define-key
   :keymaps 'meow-normal-state-keymap
   :prefix "h"
   "h" #'lsp-ui-doc-toggle
   "q" #'lsp-ui-doc-hide
   "f" #'lsp-ui-doc-focus-frame
   "u" #'lsp-ui-doc-unfocus-frame
   "d" #'lsp-ui-peek-find-definitions
   "e" #'lsp-ui-flycheck-list
   "r" #'lsp-ui-peek-find-references
   "i" #'lsp-ui-peek-find-implementations)
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package lsp-ui
  :straight t
  :after lsp
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package dap-mode
  :straight t
  :after (lsp general)
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (general-define-key
   :keymaps 'meow-normal-state-keymap
   :prefix "%"
   "d" #'dap-hydra)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package flycheck
  :straight t
  :config
  (setq flycheck-error-list-minimum-level 'error)
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "`"
   "(" #'flycheck-buffer
   "d" #'flycheck-disable-checker
   ")" #'flycheck-list-errors)
  )

(use-package markdown-mode
  :straight t)

(use-package yasnippet
  :straight t
  :demand t
  :config
  (yas-global-mode)
  (general-define-key
   :prefix "C-c s"
   "s" #'yas-insert-snippet
   "r" #'yas-reload-all))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package auto-yasnippet
  :after general
  :straight t
  :config
  (general-define-key
   :keymaps 'meow-normal-state-keymap
   :prefix "_"
   "w" #'aya-create
   "x" #'aya-expand
   "h" #'aya-expand-from-history
   "d" #'aya-delete-from-history
   "c" #'aya-clear-history
   "n" #'aya-next-in-history
   "p" #'aya-previous-in-history
   "s" #'aya-persist-snippet
   "o" #'aya-open-line
   ))

(use-package yaml-mode
  :straight t
  :defer t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)
         ))

(use-package emmet-mode
  :straight t
  :defer t)

(defun smv/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  )

(use-package web-mode
  :straight t
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css?\\'" . web-mode)
         )
  :hook
  (web-mode . smv/web-mode-hook)
  (web-mode . emmet-mode)
  )

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

(use-package rjsx-mode
  :straight t
  :defer t
  :after prettier
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.ts\\'" . rjsx-mode))
  :hook
  (rjsx-mode . emmet-mode)
  (rjsx-mode . prettier-mode))

(use-package prettier
  :straight t
  :defer t
  :after web-mode)

(use-package restclient
  :straight t
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
	      ("C-c C-f" . json-mode-beautify)))

(use-package rust-ts-mode
  :straight t
  :defer t
  :mode "\\.rs\\'")

(use-package ruby-ts-mode
  :straight t
  :defer t
  :mode "\\.rb\\'")

(use-package typst-ts-mode
  :straight t
  :mode "\\.typ\\'")

(use-package dart-mode
  :defer t
  :mode "\\.dart\\'")

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Available since Emacs 29 (Use `dabbrev-ignored-buffer-regexps' on older Emacs)
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package cape
  :straight t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("M-p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  )

(use-package orderless
  :straight t
  :custom
  (orderless-style-dispatchers '(orderless-affix-dispatch))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :straight t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'quit)     ;; Configure handling of exact matches
  (corfu-quit-no-match t) ;; quit when there's no match

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  
  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )

(use-package docker
  :defer t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :after docker)

(use-package copilot
  :straight (:type git :host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :demand t
  :bind
  (:map copilot-completion-map
        ("C-M-x" . copilot-accept-completion)
        ("C-M-<down>" . copilot-accept-completion-by-word)
        ("C-M-<right>" . copilot-next-completion)
        ("C-M-<left>" . copilot-previous-completion)
        ))

(use-package transient
  :straight t)

(use-package direnv
  :straight t)

(with-eval-after-load 'direnv
  (advice-add 'direnv-update-environment :after
              (lambda (&rest _)
		(when (getenv "EMACSLOADPATH")
                  (let ((paths (split-string (getenv "EMACSLOADPATH") ":")))
                    (dolist (path (reverse paths))
                      (add-to-list 'load-path path)))))))

(setenv "GROQ_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.gq_key") (string-trim (buffer-string))))
(setenv "ANTHROPIC_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.ant_key") (string-trim (buffer-string))))
(setenv "DEEPSEEK_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.deep_key") (string-trim (buffer-string))))
(setenv "OPENROUTER_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.openr_key") (string-trim (buffer-string))))
(setenv "GEMINI_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.gem_key") (string-trim (buffer-string))))

(use-package gptel
  :straight t
  :after general
  :config
  ;; something that makes it more convenient to add mcp tools in gptel
  (require 'gptel-integrations)
  ;; OPTIONAL configuration
  (setq gptel-default-mode 'org-mode)
  (setq gptel-use-context 'user)
  ;; (setq gptel-confirm-tool-calls t)
  (setq gptel-include-tool-results t)
  (setq gptel-include-reasoning nil)
  (setq gptel-track-media t)
  (setq-local org-yank-image-save-method "/tmp")
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  
  (add-hook 'gptel-mode-hook
            (lambda ()
              (if gptel-mode
    		  (font-lock-add-keywords nil
    					  '(("^@user\\b" 0 '(face nil display "👤 User: "))
    					    ("^@assistant\\b" 0 '(face nil display "🤖 Assistant: "))))

    		
    		(font-lock-remove-keywords 'org-mode
    					   '(("^@user\\b" 0 '(face nil display "👤 User: "))
    					     ("^@assistant\\b" 0 '(face nil display "🤖 Assistant: ")))))
              (font-lock-flush)))
  

  (let ((fname (expand-file-name "gptel-custom-functions.el" "~/.emacs.d/config/")))
    (when (file-exists-p fname)
      (load-file fname)))

  (let ((fname (expand-file-name "gptel-models.el" "~/.emacs.d/config/")))
    (when (file-exists-p fname)
      (load-file fname)))
  
  
  ;; configuring the window size
  (add-to-list 'display-buffer-alist
               `(my-both-modes-active-p
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.37)
                 (window-parameters . ((no-other-window . t)))))
  
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "!"
   ")" #'gptel-add
   "!" #'gptel-send
   "r" #'gptel-rewrite
   "(" #'gptel)

  (let ((fname (expand-file-name "gptel-gemini-oauth-cloudpa.el" "~/.org/auth/")))
    (when (file-exists-p fname)
      (load-file fname)
      (gptel-make-gemini-oauth "Gemini-OAuth" :stream t)))

  (let ((fname (expand-file-name "gptel-anthropic-oauth.el" "~/.org/auth/")))
    (when (file-exists-p fname)
      (load-file fname)
      (gptel-make-anthropic-oauth "Claude-OAuth" :stream t)))

  (let (
    	(fname (expand-file-name "partner_prompt.el" (concat user-emacs-directory "presets/")))
    	)
    (when (file-exists-p fname)
      (load-file fname)
      (setf (alist-get 'partner gptel-directives) #'smv/pair_partner)))

  :bind
  ("C-c RET" . gptel-send)
  ("C-c g g" . gptel)
  ("C-c g r" . gptel-rewrite)
  ("C-c g a" . gptel-abort))

(use-package gptel-magit
  :straight t
  :after (magit gptel)
  :hook (magit-mode . gptel-magit-install)
  :config
  (load-file (format "%s%s/%s%s" user-emacs-directory "config" "gptel-magit-message" ".el")))

(use-package gptel-agent
  :after gptel
  :straight (:host github :repo "karthink/gptel-agent" :files ("*"))
  :config
  (add-to-list 'gptel-agent-dirs (expand-file-name "agents/" user-emacs-directory))
  (gptel-agent-update))

;; load tools
;; (load-file (format "%s%s/%s%s" user-emacs-directory "tools" "fetch_url" ".el"))

(defun smv-tool/list_projects ()
  "Return a list of full paths to directories in ~/projects/."
  (let ((projects-dir (expand-file-name "~/projects/")))
    (when (file-directory-p projects-dir)
      (seq-filter
       (lambda (path)
         (and (file-directory-p path)
              (not (string-match-p "/\\.\\.?$" path))))
       (directory-files projects-dir t "^[^.]")))))

(defun smv-tool/run_command (command)
  (with-temp-buffer
    (let* ((cd-match (string-match "^cd \\([^ ;&|]+\\) *&& *\\(.+\\)$" command))
           (actual-command (if cd-match (match-string 2 command) command))
           (working-dir (if cd-match
                            (expand-file-name (match-string 1 command))
                          default-directory))
           (exit-code (call-process "bash" nil (current-buffer) nil
                                    "-c"
                                    (format "cd %s && source ~/.bashrc && %s"
                                            (shell-quote-argument working-dir)
                                            actual-command)))
           (output (buffer-string)))
      (if (zerop exit-code)
          output
	(format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s"
		exit-code output)))))

(defun smv-tool/ask_partner (question &optional directory)
  "Call gemini given the prompt"
  (let* ((partner-prompt-content
          (with-temp-buffer
            (insert-file-contents (format "%s%s" user-emacs-directory "presets/partner_prompt.el"))
            (string-trim (buffer-string))))
    	 (full-gemini-argument
          (concat partner-prompt-content
                  "\n\nHere is your task:\n\n"
                  question))
    	 (quoted-gemini-argument
          (shell-quote-argument full-gemini-argument))
    	 (command
          (concat "gemini " quoted-gemini-argument " --yolo 2> /dev/null"))
    	 (default-directory (or directory default-directory)))
    (shell-command-to-string (format "%s" command))))

(defun smv-tool/fetch_url_content (url)
  (smv/fetch-content url))

(with-eval-after-load 'gptel
  ;; shell command execution tool
  (gptel-make-tool
   :name "run_command"                    ; javascript-style  snake_case name
   :function #'smv-tool/run_command
   :description "Execute a shell command on the system and get the corresponding output. IMPORTANT: ***MAKE SURE TO ALWAYS USE FULL PATHS IF YOUR COMMAND CONTAINS SOME***"
   :confirm t
   :include t
   :args (list '(:name "command"             ; a list of argument specifications
                       :type string
                       :description "The shell command to execute. For probably long output commands please use bash utilities like grep, tail, head... to retrieve only important parts."))
   :category "system")

  (gptel-make-tool
   :name "list_projects"
   :function #'smv-tool/list_projects
   :description "List the paths of the projects that are available on the system"
   :category "project-info")

  
  (gptel-make-tool
   :name "ask_partner"
   :function #'smv-tool/ask_partner
   :description "Get any information from the internet or explore the current project. Give full summary information asked. You should be as precise as possible about what you need"
   :args (list '(:name "question"
                       :type string
                       :description "Your request. e.g What is the most up to date way of writing slidemasters using pptxgenjs library?")
               '(:name "directory"
                       :type string
                       :optional t
                       :description "Directory path to search in for project exploration tasks. Omit for online research."))
   :category "info-gathering")

  (gptel-make-tool
   :name "fetch_url_content"                    ; javascript-style  snake_case name
   :function #'smv-tool/fetch_url_content
   :description "Fetch the content of a web page in text format"
   :confirm t
   :args (list '(:name "url"             ; a list of argument specifications
                       :type string
                       :description "The url of the webpage to fetch. e.g: https://google.com"))
   :category "browsing")

  
  (load-file (format "%s%s/%s%s" user-emacs-directory "tools" "filesystem/filesystem" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "tools" "ask" ".el"))
  )

;; tools from mcp servers
(use-package mcp
  :straight (mcp :type git :host github :repo "lizqwerscott/mcp.el" :files ("*.el"))
  :after gptel
  :custom (mcp-hub-servers
           `(
             ("Context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp")))
             ;; playwright install --with-deps # installs browsers in playwright (and their deps) if required
             ("playwright" . (:command "npx" :args ("@playwright/mcp@latest" "--isolated" "--browser" "msedge")))
             ("pyautogui" . (:url "http://172.20.224.1:8000/mcp/"))
             ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
             ;; ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "~/projects")))
             ))
  :config
  (require 'mcp-hub)
  ;; (add-hook 'after-init-hook
  ;;       (lambda ()
  ;;         (mcp-hub-start-all-server nil '("sequential-thinking"))))
  )

(use-package agent-shell
  :straight t
  :defer t
  :config
  (add-to-list 'display-buffer-alist
               '((major-mode . agent-shell-mode)
		 (display-buffer-reuse-window display-buffer-in-side-window)
		 (side . right)
		 (window-width . 0.37))))

(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :straight t
  :config
  (projectile-mode))

(setq project-find-functions 
      (remq 'project-try-vc project-find-functions))

(use-package magit
  :straight t
  :commands magit-status
  :config
  (setq magit-refresh-status-buffer nil)
  (setq magit-diff-highlight-indentation nil
	magit-diff-highlight-trailing nil
	magit-diff-paint-whitespace nil
	magit-diff-highlight-hunk-body nil
	magit-diff-refine-hunk nil)
  ;; (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  ;; use C-c C-d if you want to see the diff
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Split windows horizontally for side-by-side comparison
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Split windows horizontally for side-by-side comparison
(setq ediff-split-window-function 'split-window-horizontally)

(use-package grip-mode
  :straight t
  :config
  (setq grip-command 'auto) ;; auto, grip, go-grip or mdopen
  (setq grip-preview-use-webkit t)
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

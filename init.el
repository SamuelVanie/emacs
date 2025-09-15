(setq gc-const-threshold (* 100 1000 1000))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(setopt use-short-answers t)

(setq warning-minimum-level :error)
(global-hl-line-mode 1)

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

;; remove noise for not non allowed command in emacs if your system make them
(setq ring-bell-function 'ignore)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; prevent emacs from hanging on when long lines are present in the current file
(global-so-long-mode t)

;; auto refresh buffers when files changes
(global-auto-revert-mode t)
(global-visual-line-mode t)
(delete-selection-mode t)

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

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq package-enable-at-startup nil)

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package dired-x
  :bind
  (:map dired-mode-map
        ("k" . dired-create-empty-file)
        ("<tab>" . dired-subtree-toggle)))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

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

(use-package dashboard
  :ensure t
  :demand t
  :after nerd-icons
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-display-icons-p t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "My safe place")
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package treesit-auto
  :demand t
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t
      :init
      (exec-path-from-shell-initialize)))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-list-files-after-cd t)

;; Watch out you should have fish installed on your computer
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
  :ensure t ; or :straight t
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
                shell-mode-hook
                dired-mode-hook
                vterm-mode-hook
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
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; This ensures that pressing Enter will insert a new line and indent it.
(global-set-key (kbd "RET") #'newline-and-indent)

;; Indentation based on the indentation of the previous non-blank line.
(setq-default indent-line-function #'indent-relative-first-indent-point)

;; In modes such as `text-mode', pressing Enter multiple times removes
;; the indentation. The following fixes the issue and ensures that text
;; is properly indented using `indent-relative' or
;; `indent-relative-first-indent-point'.
(setq-default indent-line-ignored-functions '())

(global-set-key (kbd "C-<tab>") 'tab-to-tab-stop)


(use-package dtrt-indent
  :ensure t
  :demand t
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)
  :config
  (dtrt-indent-global-mode))

(use-package hydra
  :ensure t
  :demand t) ;; hydra permit to repeat a command easily without repeating the keybindings multiple
(use-package general
  :ensure t
  :demand t) ;; permit to define bindings under another one easily

(use-package repeat
  :ensure nil
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
  (interactive)
  (if (use-region-p)
      (insert-pair 1 open close)
    (insert-pair nil open close)))


(defun smv/surround-custom ()
  "Prompt for custom opening and closing characters to surround text."
  (interactive)
  (let ((open (read-char "Opening character: "))
        (close (read-char "Closing character: ")))
    (smv/surround-with-pair open close)))


(defun smv/surround-with-string (open-str close-str)
  "Surround region or point with arbitrary strings."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert close-str)
        (goto-char beg)
        (insert open-str))
    (insert open-str close-str)
    (backward-char (length close-str))))

(with-eval-after-load 'hydra
  (defhydra hydra-surround (:color blue :hint nil)
    "
^Quotes^          ^Brackets^        ^Symbols^         ^Custom^
^^^^^^^^--------------------------------------------------------
_\"_: double       _(_: parentheses  _<_: angles       _c_: custom pair
_'_: single        _[_: square       _`_: backticks    _t_: HTML tag
_~_: tilde         _{_: curly        _*_: asterisks    _s_: custom strings
                                   _=_: equals
                                   _+_: plus
"
    ("\"" (smv/surround-with-pair ?\" ?\"))
    ("'" (smv/surround-with-pair ?' ?'))
    ("~" (smv/surround-with-pair ?~ ?~))
    ("(" (smv/surround-with-pair ?\( ?\)))
    ("[" (smv/surround-with-pair ?\[ ?\]))
    ("{" (smv/surround-with-pair ?\{ ?\}))
    ("<" (smv/surround-with-pair ?\< ?\>))
    ("`" (smv/surround-with-pair ?` ?`))
    ("*" (smv/surround-with-pair ?* ?*))
    ("=" (smv/surround-with-pair ?= ?=))
    ("+" (smv/surround-with-pair ?+ ?+))
    ("c" smv/surround-custom)
    ("s" (let ((open (read-string "Opening string: "))
               (close (read-string "Closing string: ")))
           (smv/surround-with-string open close)))
    ("q" nil "quit" :color red))
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
   '("J" . hydra-surround/body)
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
   '("{" . backward-paragraph)
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
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("F" . yank-media)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("H" . meow-left-expand)
   '("i" . meow-right) ;; Right (forward char)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("J" . hydra-surround/body)
   '("k" . meow-kill)
   '("K" . kill-current-buffer)
   '("l" . meow-line)
   '("L" . meow-visual-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-left) ;; Left (backward char)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-prev)        ;; Up (prev line)
   '("U" . meow-undo)        ;; Undo moved to U
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


(use-package meow
  :ensure t
  :demand t
  :after hydra
  :config
  (meow-setup)
  ;; remove those hints that clutter vision
  (setq meow-expand-hint-remove-delay 0)
  (meow-global-mode 1))

;; tabs manipulations
(with-eval-after-load 'general    
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "#"
   "l" #'tab-new
   "d" #'dired-other-tab
   "f" #'find-file-other-tab
   "r" #'tab-rename
   "u" #'tab-close
   "i" #'tab-next
   "n" #'tab-previous)

  ;; Some more complex commands
  (general-define-key
   :keymaps 'meow-normal-state-keymap
   :prefix "%"
   "s" #'scratch-buffer
   "e" #'split-window-below
   "i" #'split-window-right)


  ;; Some more complex commands
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "="
   "=" #'meow-indent
   "t" #'repeat
   "r" #'repeat-complex-command)

  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix ")"
   "n" #'smerge-vc-next-conflict
   "u" #'smerge-keep-upper
   "l" #'smerge-keep-lower
   "b" #'smerge-keep-all
   "r" #'smerge-refine
   "s" #'smerge-resolve)

  
  (general-define-key
   :keymaps 'global-map
   :prefix "C-c f"
   "f" #'ffap
   "s" #'ffap-menu))

(use-package avy
  :ensure t
  :demand t
  :after meow
  :config
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "@"
   "@"  #'avy-goto-char-in-line
   "#"  #'avy-goto-char
   "l d"  #'avy-kill-whole-line
   "l l"  #'avy-goto-end-of-line
   "u"  #'avy-goto-line-above
   "e"  #'avy-goto-line-below
   "l y"  #'avy-copy-line
   "r d"  #'avy-kill-region
   "r y"  #'avy-copy-region
   "r t"  #'avy-transpose-lines-in-region
   "r r"  #'avy-resume
   "r m"  #'avy-move-region)
  (general-define-key
   :prefix "C-z @"
   "@"  #'avy-goto-char-in-line
   "#"  #'avy-goto-char
   "l d"  #'avy-kill-whole-line
   "l l"  #'avy-goto-end-of-line
   "u"  #'avy-goto-line-above
   "e"  #'avy-goto-line-below
   "l y"  #'avy-copy-line
   "r d"  #'avy-kill-region
   "r y"  #'avy-copy-region
   "r t"  #'avy-transpose-lines-in-region
   "r r"  #'avy-resume
   "r m"  #'avy-move-region))

(use-package multiple-cursors
  :ensure t
  :demand t
  :config
  (setq mc/cmds-to-run-once '())
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 ^Mark^             ^Skip^               ^Edit^
 ^^^^^^^^-----------------------------------------
 _>_: next like this    _i_: to next like this   _+_: edit lines
 _<_: prev like this    _n_: to prev like this   _=_: mark all like this
 _q_: quit
 "
    ("+" mc/edit-lines)
    (">" mc/mark-next-like-this)
    ("<" mc/mark-previous-like-this)
    ("=" mc/mark-all-like-this)
    ("i" mc/skip-to-next-like-this)
    ("n" mc/skip-to-previous-like-this)
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
                  hydra-multiple-cursors/mc/mark-previous-like-this
                  hydra-multiple-cursors/mc/mark-all-like-this
                  hydra-multiple-cursors/mc/skip-to-next-like-this
                  hydra-multiple-cursors/mc/skip-to-previous-like-this))))

(winner-mode 1) ;; activate the package that recalls the current layout to redo it

(use-package windmove
  :after meow
  :ensure nil
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
  :ensure t
  :demand t
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

(use-package eterm-256color
  :ensure t
  :demand t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode)
  (add-hook 'eat-mode-hook #'eterm-256color-mode))

(use-package vterm
  :ensure t
  :defer t
  :config
  ;; Set proper terminal capabilities
  (setq vterm-term-environment-variable "eterm-color")
  
  (setq vterm-kill-buffer-on-exit t) ; Prevent accumulation of dead buffers
  (setq vterm-copy-exclude-prompt t))
  
(use-package multi-vterm
  :after vterm
  :bind (("C-c v n" . multi-vterm-project)
         ("C-c v f" . multi-vterm)
         ("C-c v r" . multi-vterm-rename-buffer)
         ("C-x C-y" . multi-vterm-dedicated-toggle))
  :config
  ;; Prevent key binding conflicts
  (define-key vterm-mode-map [return] #'vterm-send-return)
  
  ;; Better buffer management
  (setq multi-vterm-buffer-name "vterm")
  (setq multi-vterm-dedicated-window-height-percent 40))

(use-package eat
  :demand t
  :ensure (:fetcher codeberg
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

(setq browse-url-generic-program "MicrosoftEdge.exe")
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
  :ensure t
  :demand t
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

(use-package doom-themes
  :ensure t
  :demand t)
(use-package ef-themes
  :ensure t
  :demand t
  :config
  (load-theme 'ef-winter))
(use-package standard-themes
  :ensure t
  :demand t)
(use-package kaolin-themes
  :ensure t
  :demand t
  :config
  (kaolin-treemacs-theme))
(use-package catppuccin-theme
  :ensure t
  :demand t)
(use-package challenger-deep-theme
  :ensure t
  :demand t)
(use-package solo-jazz-theme
  :ensure t
  :demand t)
(use-package stimmung-themes
  :ensure (:fetcher github :repo "motform/stimmung-themes" :files ("*.el"))
  :demand t)
(use-package rebecca-theme
  :ensure t
  :demand t)
(use-package stimmung-themes
  :ensure (:fetcher github :repo "monkeyjunglejuice/matrix-emacs-theme" :files ("*.el"))
  :demand t)
(use-package pink-bliss-uwu-theme
  :ensure (:fetcher github :repo "themkat/pink-bliss-uwu" :files ("*.el"))
  :demand t)

(use-package all-the-icons
  :ensure t
  :demand t
  :if (display-graphic-p))

(use-package nerd-icons
  :ensure t
  :demand t)

(use-package all-the-icons-dired
  :ensure t
  :demand t
  :after all-the-icons)

;; to install emoji rendering in emacs
;; some external packages to install are : fonts-noto-color-emoji and fonts-emojione on ubuntu
;; noto-fonts-emoji and ttf-joypixels on archlinux
(use-package unicode-fonts
  :ensure t
  :demand t
  :config (unicode-fonts-setup))

(use-package which-key ;; print next keybindings
  :ensure t
  :demand t
  :diminish which-key-mode
  :config ;; only runs after the mode is loaded
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

(use-package helm
  :ensure t
  :demand t
  :bind
  ("M-x" . helm-M-x)
  ("C-s" . helm-occur)
  :config
  (setq helm-mode-fuzzy-match t)
  (setq helm-full-frame nil)
  (setq helm-split-window-inside-p t)
  (setq helm-always-two-windows nil)
  (setq helm-completion-in-region-fuzzy-match t)
  (helm-mode)
  :bind
  (
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("C-c h c" . smv/helm-zoxide-cd)
   ("C-c h m" . helm-mark-ring)
   ("C-c h k" . helm-show-kill-ring)
   ("C-c h s" . helm-do-grep-ag)
   ("C-c h f" . helm-find)
   ("C-c h n" . helm-complete-file-name-at-point)
   ("C-c h t" . helm-magit-todos))
  )

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
               (cd candidate)
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
  :demand t
  :ensure (:wait org-contrib)
  :config
  (setq org-agenda-files
        '("~/.org/todo.org"
          "~/.org/projects.org"))

  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)")
          (sequence "BACKLOG(b!)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v!)" "WAIT(w@/!)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; easily move task to another header
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("todo.org" :maxlevel . 1)
          ("projects.org" :maxlevel . 1)
          ("personal.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@school" . ?s)
          ("personal" . ?p)
          ("note" . ?n)
          ("idea" . ?i)))

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "TODO"
                  ((org-agenda-overriding-header "All tasks")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))
          
          ("a" "Active"
           ((todo "ACTIVE"
                  ((org-agenda-overriding-header "Ongoing Tasks")))))

          ("st" "School todos" tags-todo "+@school/TODO")
          ("sp" "School Projects" tags-todo "+@school/ACTIVE")
          ("sr" "School Review" tags-todo "+@school/REVIEW")

          ("pt" "Personal todos" tags-todo "+personal/TODO")
          ("pl" "Personal Projects" tags-todo "+personal/ACTIVE")
          ("pr" "Personal Review" tags-todo "+personal/REVIEW")
          
          ("oa" "OnePoint Archimind" tags "+archimind+@school+coding/TODO")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))))

  (setq org-capture-templates ;; quickly add todos entries without going into the file
        `(("t" "Tasks")
          ("tt" "Task" entry (file+olp "~/.org/todo.org" "Tasks")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("ta" "Archimind task" entry (file+regexp "~/.org/todo.org" "PHASE 5")
           "**** TODO %?\n %T\n %a\n %i")))


  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "M-i") 'org-insert-item))

(use-package phscroll
  :ensure (:fetcher github :repo "misohena/phscroll" :files ("*.el"))
  :demand t
  :hook (org-mode . org-phscroll-mode)
  :config
  (setq org-startup-truncated nil))

(use-package org-modern
  :ensure t
  :demand t
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

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Set the directory where journal files will be stored
  (setq org-journal-dir "~/.org/journal/")
  ;; Optional: Set a file name format (default is YYYYMMDD)
  (setq org-journal-file-format "%Y-%m-%d.org")
  :bind
  ("C-c n j" . org-journal-new-entry)
  :config
  ;; Optional: Automatically add a timestamp to new entries
  (setq org-journal-enable-timestamp t)
  ;; Ensure the directory exists
  (make-directory org-journal-dir t))

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
  :ensure t
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

;; Ensure ANSI colors work properly in shell-mode too
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(use-package undo-tree
  :ensure (:wait t)
  :init
  (global-undo-tree-mode)
  :config
  ;; Set a dedicated directory for undo-tree files
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  ;; Create the directory if it doesn't exist
  (unless (file-exists-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo" t)))
;; Enable global undo-tree mode

;; Critical Unicode and encoding fixes
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Memory management for all terminal modes
(setq comint-buffer-maximum-size 5000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; Better eshell configuration to prevent corruption
(with-eval-after-load 'eshell
  ;; Prevent eshell from becoming too large and causing issues
  (add-hook 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-buffer-maximum-lines 5000)
  
  ;; Better prompt handling
  (setq eshell-highlight-prompt t)
  (setq eshell-cmpl-ignore-case t)
  
  ;; Fix visual line issues
  (add-hook 'eshell-mode-hook 
            (lambda ()
              (setq-local global-hl-line-mode nil)
              (setq-local line-spacing 0))))

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

;; Fix meow mode integration with terminals
(with-eval-after-load 'meow
  ;; Don't apply meow keys in terminal modes
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(eat-mode . insert))
  (add-to-list 'meow-mode-state-list '(term-mode . insert)))

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

(use-package wgrep
  :ensure t
  :bind
  ("C-x C-," . wgrep-change-to-wgrep-mode)
  :config
  (global-set-key (kbd "C-c r") 'rgrep))

;; Permit to get the first results directly in the compilation buffer
;; This kind of buffer is the one used for grep
(setq compilation-scroll-output 'first-error)

;; Ignore some directories
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-directories)
     (add-to-list 'grep-find-ignored-directories "*.git")))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "M-l")
  :bind
  ("M-p M-p" . lsp-mode)
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
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package dap-mode
  :ensure t
  :defer t
  :after (lsp-mode general)
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
  :ensure t
  :mode "\\.nix\\'"
  :config
  :hook (nix-mode . lsp-deferred))

(use-package flycheck
  :ensure t
  :demand t
  :config
  (setq flycheck-error-list-minimum-level 'error)
  (general-define-key
   :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
   :prefix "("
   "(" #'flycheck-clear
   "d" #'flycheck-disable-checker
   ")" #'flycheck-list-errors)
  )

(use-package markdown-mode
  :ensure t
  :demand t)

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode)
  (general-define-key
   :prefix "C-c s"
   "s" #'yas-insert-snippet
   "r" #'yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package auto-yasnippet
  :after general
  :ensure t
  :demand t
  :config
  (general-define-key
   :prefix "C-z *"
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
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)
         ))

(use-package lsp-java
  :ensure t
  :hook (java-ts-mode . lsp-deferred))

(use-package dap-java :defer t)

(use-package emmet-mode
  :ensure t)

(defun smv/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  )

(use-package web-mode
  :ensure t
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
  :defer t
  :after prettier
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.ts\\'" . rjsx-mode))
  :hook
  (rjsx-mode . emmet-mode)
  (rjsx-mode . prettier-mode))

(use-package prettier
  :ensure t
  :after web-mode)

(use-package rust-ts-mode
  :defer t
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . lsp-deferred))

(use-package ruby-ts-mode
  :defer t
  :mode "\\.rb\\'"
  :hook (ruby-ts-mode . lsp-deferred))

(use-package typst-ts-mode
  :ensure t
  :defer t
  :mode "\\.typ\\'")

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.typ" . "typst"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("tinymist")) ; Or the command to run your LSP server
                    :activation-fn (lsp-activate-on "typst")
                    :server-id 'tinymist)))

(use-package dart-mode
  :ensure t
  :defer t
  :mode "\\.dart\\'"
  :hook (dart-mode . lsp-deferred))

(use-package lsp-dart
  :ensure t
  :defer t
  :after lsp-mode)

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
  :ensure t
  :demand t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
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
  :ensure t
  :demand t
  :custom
  (orderless-style-dispatchers '(orderless-affix-dispatch))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :demand t
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches

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
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :after docker)

(use-package copilot
  :defer t
  :ensure (:fetcher github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind
  (:map copilot-completion-map
        ("C-M-x" . copilot-accept-completion)
        ("C-M-<down>" . copilot-accept-completion-by-word)
        ("C-M-<right>" . copilot-next-completion)
        ("C-M-<left>" . copilot-previous-completion)
        ))

(use-package transient
  :ensure t)

(use-package direnv
  :ensure t)

(setenv "GROQ_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.gq_key") (string-trim (buffer-string))))
(setenv "ANTHROPIC_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.ant_key") (string-trim (buffer-string))))
(setenv "DEEPSEEK_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.deep_key") (string-trim (buffer-string))))
(setenv "OPENROUTER_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.openr_key") (string-trim (buffer-string))))
(setenv "GEMINI_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.gem_key") (string-trim (buffer-string))))

(use-package gptel
  :ensure t
  :demand t
  :config
  ;; something that makes it more convenient to add mcp tools in gptel
  (require 'gptel-integrations)
  ;; OPTIONAL configuration
  (setq gptel-default-mode 'org-mode)
  (setq gptel-use-context 'user)
  ;; (setq gptel-confirm-tool-calls t)
  (setq gptel-include-tool-results t)
  (setq gptel-include-reasoning nil)
  (gptel-make-gemini "Gemini"
    :key (with-temp-buffer (insert-file-contents "~/.org/.gem_key") (string-trim (buffer-string)))
    :stream t)
  (gptel-make-deepseek "DeepSeek"       ;Any name you want
    :stream t                           ;for streaming responses
    :key (with-temp-buffer (insert-file-contents "~/.org/.deep_key") (string-trim (buffer-string))))
  (gptel-make-openai "OpenRouter"
    ;; :online in the language slug to add the search plugin
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (with-temp-buffer (insert-file-contents "~/.org/.openr_key") (string-trim (buffer-string)))
    :models '(
              perplexity/sonar-pro ;; 3 in - 15 out
              anthropic/claude-sonnet-4 ;; 3 in - 15 out
              anthropic/claude-3.7-sonnet ;; 3 in - 15 out
              openai/gpt-5 ;; 1.25 in - 10 out
              google/gemini-2.5-pro ;; 1.25 in - 10 out
              openai/gpt-4.1 ;; 2 in - 8 out
              switchpoint/router ;; 0.85 in - 3.40 out
              openai/gpt-4.1-mini ;; 0.40 in - 1.60 out
              x-ai/grok-code-fast-1 ;; 0.2 in - 1.5 out
              qwen/qwen3-coder  ;; 0.302 in - 0.302 out
              google/gemini-2.5-flash ;; 0.30 in - 2.50 out
              minimax/minimax-m1 ;; 0.30 in - 1.65 out
              moonshotai/kimi-k2 ;; 0.14 in - 2.49 out
              deepseek/deepseek-chat-v3.1 ;; 0.2 in - 0.8 out
              z-ai/glm-4.5 ;; 0.2 in - 0.2 out
              ))

  (setq
   gptel-backend (gptel-make-anthropic "Anthropic"
                   :key (with-temp-buffer (insert-file-contents "~/.org/.ant_key") (string-trim (buffer-string)))
                   :stream t))
  
  (gptel-make-gh-copilot "Copilot")

  ;; ;; local models
  (gptel-make-openai "lmstudio"
    :host "http://10.32.68.169:1234"
    :endpoint "/v1/chat/completions"
    :stream t
    :key "dummy"
    :models '(
              microsoft/phi-4-reasoning-plus
              deepseek-coder-7b-instruct-v1.5
              google/gemma-3-12b
              whisper-large-v3
              llama-3-groq-8b-tool-use
              deepseek/deepseek-r1-0528-qwen3-8b
              ))

  ;; loads agents
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "command_line" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "lite_mayuri" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "mayuri" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "mayuri_front" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "mayuri_back" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "mayuri_reverse_archi" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "mayuri_designer" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "architect" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "tasker" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "task" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "summarizer" ".el"))
  (load-file (format "%s%s/%s%s" user-emacs-directory "agents" "summarizer_google" ".el"))

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
   "(" #'gptel)
  
  :bind
  ("C-c RET" . gptel-send)
  ("C-c g g" . gptel)
  ("C-c g a" . gptel-abort))

;; load tools
(load-file (format "%s%s/%s%s" user-emacs-directory "tools" "fetch_url" ".el"))

;; My custom emacs tools
(defun smv-tool/get_project_root ()
  (if (project-current) (project-root (project-current)) default-directory))

(defun smv-tool/run_command (command)
  (shell-command-to-string (format "cd %s && %s" (smv-tool/get_project_root) command)))

(defun smv-tool/ask_partner (question)
  "Call gemini given the prompt"
  (let ((command (concat "gemini -p " 
                         (shell-quote-argument question))))
    (shell-command-to-string (format "cd %s && %s" (smv-tool/get_project_root) command))))

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
                       :description "The shell command to execute. e.g: echo 'test'"))
   :category "system")

  (gptel-make-tool
   :name "get_project_root"
   :function #'smv-tool/get_project_root
   :description "Get the full path of the current project rootdir. Could be interesting to run at the beginning of work, because all the following work depends on it."
   :category "project-info")

  (gptel-make-tool
   :name "ask_partner"
   :function #'smv-tool/ask_partner
   :description "Get any information from the internet or the current project using natural language. It's like your big brother who knows everything, you call him when you're really stuck, you got no alternative to get info."
   :args (list '(:name "question"             ; a list of argument specifications
                       :type string
                       :description "Your info gathering request. e.g What is the most up to date way of writing slidemasters using pptxgenjs library?"))
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
  )

;; tools from mcp servers
(use-package mcp
  :ensure (:fetcher github :repo "lizqwerscott/mcp.el" :files ("*.el"))
  :demand t
  :after gptel
  :custom (mcp-hub-servers
           `(
             ("Context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp")))
             ;; playwright install --with-deps # installs browsers in playwright (and their deps) if required
             ("playwright" . (:command "npx" :args ("@playwright/mcp@latest" "--isolated")))
             ("pyautogui" . (:url "http://172.20.224.1:8000/mcp/"))
             ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
             ))
  :config
  (require 'mcp-hub)
  (add-hook 'after-init-hook
        (lambda ()
          (mcp-hub-start-all-server nil '("filesystem")))))

(use-package claude-code-ide
  :ensure (:fetcher github :repo "manzaltu/claude-code-ide.el" :files ("*.el" "scripts/*"))
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setq claude-code-ide-terminal-backend 'eat)
  (setq claude-code-ide-window-width 50)
  (setq claude-code-ide-vterm-anti-flicker t)
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(setq project-find-functions 
    (remq 'project-try-vc project-find-functions))

(use-package magit
  :ensure (:wait t)
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :config
  (require 'eaf)
  (require 'eaf-markdown-previewer))

(setq gc-const-threshold (* 80 1000 1000))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(cond
 ((eq system-type 'darwin)  ;; macOS
  (setq read-process-output-max (* 64 1024)))  ;; 64KB
 ((eq system-type 'gnu/linux)  ;; Linux
  (setq read-process-output-max (* 1024 1024)))  ;; 1MB
 )

;; You will most likely need to adjust this font size for your system!
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

(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-<tab>") 'tab-to-tab-stop)
(setq tab-width 4)


;; auto refresh buffers when files changes
(global-auto-revert-mode t)

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


(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-use-term-integration t))



(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line
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
          compilation-mode))
  ;; Match eshell, shell, term and/or vterm buffers
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                  "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                  "^\\*term.*\\*$"   term-mode   ;term as a popup
                  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                  )))

  (popper-mode +1)
  (popper-echo-mode +1))

(scroll-bar-mode -1) ; Disable visible scroll bar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable menu bar


(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t) ;; print line numbers for files


;; Set frame transparency
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))



;; some modes doesn't have to start with lines enable
(dolist (mode '(org-mode-hook
                term-mode-hook
                doc-view-minor-mode-hook
                shell-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(if (eq system-type 'darwin)
    (progn
      (set-frame-font "JetbrainsMono Nerd Font-19" nil t)
      (set-face-attribute 'fixed-pitch nil :family "FantasqueSansM Nerd Font Mono"))
  (add-to-list 'default-frame-alist '(font . "JetbrainsMono Nerd Font-15"))
  (set-face-attribute 'fixed-pitch nil :family "FantasqueSansMono Nerd Font"))

(set-face-attribute 'variable-pitch nil :family "Iosevka Nerd Font")
;; (set-face-attribute 'variable-pitch nil :family "FantasqueSansM Nerd Font")

(use-package ligature
  :ensure t
  :demand t
  :config
  ;; Enable all JetBrains Mono ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                       "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                       "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                       "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                       "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                       "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                       ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                       "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                       "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                       "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                       "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow-motion-define-key
   '("<escape>" . ignore)
   '("e" . meow-next)
   '("u" . meow-prev)
   '("n" . meow-left)
   '("s" . meow-insert)
   '("i" . meow-right)
   '("C" . meow-pop-to-mark)
   '("V" . meow-unpop-to-mark)
   '("<" . back-to-indentation)
   '(">" . end-of-visual-line)
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
   '("e" . meow-next)        ;; Down (next line)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . undefined)         ;; Disable old left binding
   '("H" . meow-left-expand)
   '("i" . meow-right) ;; Right (forward char)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-left) ;; Left (backward char)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-prev)        ;; Up (prev line)
   '("U" . meow-undo)        ;; Undo moved to U <button class="citation-flag" data-index="1">
   '("v" . meow-search)
   '("V" . meow-unpop-to-mark)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("Y" . copy-from-above-command)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<" . back-to-indentation)
   '(">" . end-of-visual-line)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :demand t
  :config
  (meow-setup)
  ;; remove the overlay
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

;; Buffers manipulations
(general-define-key
 :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
 :prefix "\\"
 "b k" #'kill-buffer-and-window)


;; Some more complex commands
(general-define-key
 :keymaps 'meow-normal-state-keymap
 :prefix "%"
 "s" #'scratch-buffer)

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
   "+"  #'balance-windows
   "m"  #'maximize-window
   "s n"  #'windmove-swap-states-left
   "s i"  #'windmove-swap-states-right
   "s e"  #'windmove-swap-states-down
   "s u"  #'windmove-swap-states-up)
  )

(use-package vterm
  :ensure t
  :defer t)

(use-package multi-vterm
  :after vterm
  :ensure t
  :defer t
  :bind (("C-c v n" . multi-vterm-project)
         ("C-c v f" . multi-vterm)
         ("C-c v r" . multi-vterm-rename-buffer)
         ("C-x C-y" . multi-vterm-dedicated-toggle))
  :config
  (define-key vterm-mode-map [return]                      #'vterm-send-return)
  ;; terminal height percent of 30
  (setq multi-vterm-dedicated-window-height-percent 45))

(setq browse-url-generic-program "microsoft-edge-stable")
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
   "*" #'er/expand-region))

(use-package doom-themes
  :ensure t
  :demand t)
(use-package ef-themes
  :ensure t
  :demand t
  :config (load-theme 'doom-acario-dark t))

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
   ("C-c h s" . helm-do-grep-ag)
   ("C-c h f" . helm-find)
   ("C-c h t" . helm-magit-todos))
  )

(defun smv/helm-zoxide-candidates ()
  "Generate a list of zoxide query candidates."
  (when-let ((zoxide (executable-find "zoxide")))
    (with-temp-buffer
      (call-process zoxide nil t nil "query" "-l")
      (split-string (buffer-string) "\n" t))))

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

(defun smv/org-font-setup ()
  (font-lock-add-keywords 'org-mode ;; Change the list icon style from "-" to "."
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([+]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

  ;; configuration of heading levels size
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "JetbrainsMono Nerd Font" :weight 'regular :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun smv/org-style-pdf ()
  ;; in Case error
  ;; with xetex fmt files
  ;; reformat with
  ;; sudo pacman -S texlive-xetex
  ;; sudo fmtutil-sys --byfmt xelatex
  ;; install the extra of latex from your package repo
  (require 'ox-latex)

  ;; Activer l'utilisation de minted
  ;; font python source blocs install Pygments
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("linenos" "true")
          ("breaklines" "true")
          ("fontsize" "\\scriptsize")))

  ;; Style des blocs source dans Org Mode
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; Ajouter des en-têtes et des pieds de page
  (setq org-latex-default-packages-alist
        (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

  ;; Utiliser minted dans les documents LaTeX
  (add-to-list 'org-latex-packages-alist '("" "minted" t)))

(defun smv/org-mode-setup()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (smv/org-style-pdf)
  (smv/org-font-setup))


(use-package org ;; org-mode, permit to take notes and other interesting stuff with a specific file extension
  :demand t
  :ensure (:wait org-contrib)
  :hook (org-mode . smv/org-mode-setup)
  :config
  (setq org-ellipsis " ▼:")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/.org/todo.org"
          "~/.org/projects.org"
          "~/.org/personal.org"))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

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

          ("st" "School todos" tags-todo "+@school/TODO")
          ("sp" "School Projects" tags-todo "+@school/ACTIVE")
          ("sr" "School Review" tags-todo "+@school/REVIEW")

          ("pt" "Personal todos" tags-todo "+personal/TODO")
          ("pl" "Personal Projects" tags-todo "+personal/ACTIVE")
          ("pr" "Personal Review" tags-todo "+personal/REVIEW")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))))

  (setq org-capture-templates ;; quickly add todos entries without going into the file
        `(("t" "Tasks")
          ("tt" "Task" entry (file+olp "~/.org/todo.org" "Tasks")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))


  (smv/org-font-setup)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "M-i") 'org-insert-item))

(use-package org-bullets ;; change the bullets in my org mode files
  :ensure t
  :demand t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶")))

;; Outline numbering for org mode
(use-package org-num
  :ensure nil
  :load-path "lisp/"
  :after org
  :hook (org-mode . org-num-mode))

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
  :init
  (setq lsp-keymap-prefix "C-l")
  :bind
  ("M-p l" . lsp-mode)
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  :hook (nix-mode . lsp-deferred))

(use-package flycheck
  :ensure t
  :demand t)

(use-package markdown-mode
  :ensure t
  :demand t)

(use-package yasnippet
  :ensure t
  :demand t
  :config (yas-global-mode))

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

(use-package emmet-mode
  :ensure t)

(defun smv/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
  )

(use-package web-mode
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
  :after prettier
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.ts\\'" . rjsx-mode))
  :hook
  (rjsx-mode . emmet-mode)
  (rjsx-mode . prettier-mode))

(use-package prettier
  :after web-mode)

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . lsp-deferred))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :hook (ruby-ts-mode . lsp-deferred))

(use-package dart-mode
  :mode "\\.dart\\'"
  :hook (dart-mode . lsp-deferred))

(use-package lsp-dart
  :after lsp-mode)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  ("M-p c" . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (add-to-list 'company-backends 'company-files))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

(use-package company-tabnine
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine t))

(use-package dap-mode
  :after (lsp-mode general)
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")))

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :after docker)

(use-package copilot
  :ensure (:fetcher github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind
  (:map copilot-completion-map
        ("C-M-<down>" . copilot-accept-completion)
        ("C-M-<up>" . copilot-accept-completion-by-word)
        ("C-M-<right>" . copilot-next-completion)
        ("C-M-<left>" . copilot-previous-completion)
        ))

(use-package transient
  :ensure t)

(use-package gptel
  :ensure t
  :demand t
  :config
  ;; OPTIONAL configuration
  (setq
   gptel-model 'gemini-pro
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (with-temp-buffer (insert-file-contents "~/.org/.gem_key") (string-trim (buffer-string)))
                   :stream t))
  :bind ("C-c g" . gptel-send))

(use-package aidermacs
  :ensure (:fetcher github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "gemini/gemini-2.0-flash-thinking-exp")
  (global-set-key (kbd "C-c x") 'aidermacs-transient-menu)
  (aidermacs-setup-minor-mode)
  (setq aidermacs-show-diff-after-change t)
  (setq aidermacs-backend 'vterm)
  (setenv "GROQ_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.gq_key") (string-trim (buffer-string))))
  (setenv "GEMINI_API_KEY" (with-temp-buffer (insert-file-contents "~/.org/.gem_key") (string-trim (buffer-string)))))

(use-package magit
  :ensure (:wait t)
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :init
  (magit-maybe-define-global-key-bindings t))

(use-package magit-todos
  :ensure t
  :defer t
  :after magit
  :config (magit-todos-mode 1))

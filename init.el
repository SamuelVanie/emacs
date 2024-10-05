(setq gc-const-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024))

;; You will most likely need to adjust this font size for your system!
(defvar smv/default-font-size 139)
(defvar smv/default-variable-font-size 139)

;; remove noise for not non allowed command in emacs if your system make them
(setq ring-bell-function 'ignore)

;; Make frame transparency overridable
(defvar smv/frame-transparency '(90 . 90))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq-default indent-tabs-mode nil)

;; auto refresh buffers when files changes
(global-auto-revert-mode t)

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

;; Initialize package sources
(require 'package)
(require 'cl)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; straight.el section
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package dired-x
  :straight nil
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(add-hook 'dired-x-mode-hook 'all-the-icons-dired-mode)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
                                        ;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-list-files-after-cd t)

;; Watch out you should have fish installed on your computer
(setq-default explicit-shell-file-name "/usr/bin/fish")
(setq eshell-aliases-file "~/.emacs.d/aliases")

(use-package eshell-toggle
  :bind ("C-x C-z" . eshell-toggle))

;; this will make emacs ibuffer the default used to list buffers
(defalias 'list-buffers 'ibuffer)

(defun kill-all-buffers ()
  "Kill all buffers without asking for confirmation."
  (interactive)
  (dolist (buffer (buffer-list))
    (kill-buffer buffer)))

(global-set-key (kbd "C-c k a") 'kill-all-buffers)
(global-set-key (kbd "C-k") 'kill-line)

(scroll-bar-mode -1) ; Disable visible scroll bar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable menu bar


(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t) ;; print line numbers for files


;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha smv/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,smv/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set frame font
(add-to-list 'default-frame-alist '(font . "JetbrainsMono Nerd Font"))

;; some modes doesn't have to start with lines enable
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Change the font size (139) according to your screen
(custom-set-faces
 '(fixed-pitch ((t (:height 139 :family "DaddyTimeMono Nerd Font"))))
 '(variable-pitch ((t (:weight light :height 139 :family "JetbrainsMono Nerd Font")))))

(use-package ligature
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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package general ;; for setting keybindings
  :config
  (general-create-definer smv/leader-keys
                          :keymaps '(normal visual emacs)
                          :prefix "SPC"
                          :global-prefix "SPC")

  (smv/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra) ;; hydra permit to repeat a command easily without repeating the keybindings multiple

(use-package xah-fly-keys
  :config
  (xah-fly-keys t)
  (xah-fly-keys-set-layout "colemak")
  (setq xah-fly-use-control-key nil)
  (setq xah-fly-use-meta-key nil)
  (define-key xah-fly-command-map (kbd "k") 'swiper)
  (define-key xah-fly-command-map (kbd "SPC b") 'copilot-complete))

(use-package ace-jump-mode
  :bind (
         ("C-c SPC" . ace-jump-mode)
         :map xah-fly-command-map
         ("SPC z" . ace-jump-mode)
         ))

(use-package vterm)

(use-package multi-vterm
  :ensure t
  :config
  (define-key vterm-mode-map [return]                      #'vterm-send-return)
  (global-set-key (kbd "C-x C-y") 'multi-vterm)
  (setq vterm-keymap-exceptions nil))

(use-package doom-themes)
(use-package ef-themes
  :config
  (load-theme 'manoj-dark t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-ivy
  :after all-the-icons)

(use-package nerd-icons)

(use-package all-the-icons-dired
  :after all-the-icons)

(use-package which-key ;; print next keybindings
  :init (which-key-mode) ;; happens before the package is loaded
  :diminish which-key-mode
  :config ;; only runs after the mode is loaded
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
                                        ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
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
        (set-face-attribute (car face) nil :font "VictorMono" :weight 'regular :height (cdr face)))
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

(defun smv/org-mode-setup()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (smv/org-font-setup))


(use-package org ;; org-mode, permit to take notes and other interesting stuff with a specific file extension
    :straight org-contrib
    :hook (org-mode . smv/org-mode-setup)
    :config
    (setq org-ellipsis " ▼:")
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-startup-folded 'content)

    (setq org-agenda-files
            '("~/.org/todo.org"
            "~/.org/projects.org"))

    (setq org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
            (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

    ;; easily move task to another header
    (setq org-refile-targets
            '(("archive.org" :maxlevel . 1)
            ("todo.org" :maxlevel . 1)
            ("projects.org" :maxlevel . 1)))

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
        
        ("pt" "Personal todos" tags-todo "+personal/TODO")
        
        ("pl" "Personal Projects" tags-todo "+personal/ACTIVE")
        
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

(use-package org-fragtog
    :hook (org-mode-hook . org-fragtog-mode))

(use-package ox-reveal)

(use-package org-bullets ;; change the bullets in my org mode files
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶")))

;; Outline numbering for org mode
(use-package org-num
    :straight nil
    :load-path "lisp/"
    :after org
    :hook (org-mode . org-num-mode))

;; use to stretch the page on the center to be able to focus on document writing
(use-package olivetti
    :hook (org-mode-hook . olivetti-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
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
                      (expand-file-name "~/.emacs.d/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'smv/org-babel-tangle-config)))

(use-package undo-tree
      :config
      (global-undo-tree-mode))

(global-set-key (kbd "C-M-;") 'comment-region)

(use-package wgrep)
(global-set-key (kbd "C-c r") 'rgrep)

(use-package direnv
  :config
  (direnv-mode))

(use-package flymake
  :bind
  ("M-g f l" . flymake-show-project-diagnostics))

;; set global keybinding for quickfix
(global-set-key (kbd "M-g e a") 'eglot-code-actions)

(use-package markdown-mode)

(use-package yasnippet
    :config (yas-global-mode))

(use-package auto-yasnippet
  :bind
  ("C-c C-y w" . aya-create)
  ("C-c C-y TA". aya-expand)
  ("C-c C-y SP". aya-expand-from-history)
  ("C-c C-y d" . aya-delete-from-history)
  ("C-c C-y c" . aya-clear-history)
  ("C-c C-y n" . aya-next-in-history)
  ("C-c C-y p" . aya-previous-in-history)
  ("C-c C-y s" . aya-persist-snippet)
  ("C-c C-y o" . aya-open-line))

(use-package yaml-mode
:mode (("\\.yml\\'" . yaml-mode)
            ("\\.yaml\\'" . yaml-mode)
            ))

(use-package emmet-mode)

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
    (web-mode . prettier-mode)
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
  :mode (("\\.js\\'" . rjsx-mode)
            ("\\.ts\\'" . rjsx-mode))
  :hook
  (rjsx-mode . emmet-mode)
  (rjsx-mode . prettier-mode))

(use-package prettier)

(use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . eglot-ensure)
    :config
    (setq typescript-indent-level 2))

(use-package php-mode
  :mode "\\.php\\'"
  )

(use-package eglot-java
  :after eglot)

(use-package rust-mode)

(use-package rust-ts-mode
    :mode "\\.rs\\'"
    :bind-keymap
    ("C-c c" . rust-mode-map)
    :hook (rust-ts-mode . eglot-ensure))

(use-package flutter)

(use-package dart-mode
    :mode "\\.dart\\'"
    :hook (dart-mode . eglot-ensure))

(use-package company
    :after eglot-mode
    :hook (eglot-mode . company-mode)
    :bind
    (:map company-mode
          ("M-o" . company-manual-begin))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

(use-package company-box
    :hook
    (company-mode . company-box-mode))

(use-package company-tabnine
    :config
    (add-to-list 'company-backends #'company-tabnine t))

(use-package dape
 :after eglot)

(use-package docker
    :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(defun rk/copilot-complete-or-accept ()
    "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
    (interactive)
    (if (copilot--overlay-visible)
        (progn
        (copilot-accept-completion)
        (open-line 1)
        )
    (copilot-complete)))

(defun rk/copilot-quit ()
"Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
(interactive)
(condition-case err
    (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
        (setq copilot-disable-predicates (list (lambda () t)))
        (copilot-clear-overlay)
        (run-with-idle-timer
            1.0
            nil
            (lambda ()
            (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(defun rk/no-copilot-mode ()
"Helper for `rk/no-copilot-modes'."
(copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                                inferior-python-mode
                                eshell-mode
                                term-mode
                                vterm-mode
                                comint-mode
                                compilation-mode
                                debugger-mode
                                dired-mode-hook
                                compilation-mode-hook
                                flutter-mode-hook
                                minibuffer-mode-hook)
    "Modes in which copilot is inconvenient.")

(defvar rk/copilot-manual-mode nil
    "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defvar rk/copilot-enable-for-org nil
    "Should copilot be enabled for org-mode buffers?")


(defun rk/copilot-enable-predicate ()
    ""
    (and
    (eq (get-buffer-window) (selected-window))))

(defun rk/copilot-disable-predicate ()
    "When copilot should not automatically show completions."
    (or rk/copilot-manual-mode
        (member major-mode rk/no-copilot-modes)
        (and (not rk/copilot-enable-for-org) (eq major-mode 'org-mode))
        (company--active-p)))

(defun rk/copilot-change-activation ()
    "Switch between three activation modes:
    - automatic: copilot will automatically overlay completions
    - manual: you need to press a key (C-M-<return>) to trigger completions
    - off: copilot is completely disabled."
    (interactive)
    (if (and copilot-mode rk/copilot-manual-mode)
        (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
            (message "activating copilot manual mode")
            (setq rk/copilot-manual-mode t))
        (message "activating copilot mode")
        (global-copilot-mode))))


(straight-use-package '(copilot :host github
                            :repo "copilot-emacs/copilot.el"
                            :branch "main"
                            :files ("dist" "*.el")))

(require 'copilot)
;; keybindings that are active when copilot shows completions
(define-key copilot-mode-map (kbd "C-M-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "C-M-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "C-M-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "C-M-<return>") #'copilot-accept-completion-by-line)

;;global keybindings
(define-key global-map (kbd "C-M-<down>") #'rk/copilot-complete-or-accept)
(define-key global-map (kbd "C-M-<escape>") #'rk/copilot-change-activation)

;;Do copilot-quit when pressing C-g
(advice-add 'keyboard-quit :before #'rk/copilot-quit)

;; ;; deactivate copilot for certain modes
(add-to-list 'copilot-enable-predicates #'rk/copilot-enable-predicate)
(add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

;; (defun smv/gptel-api-key ()
;;   "Retrieve my OpenAI API key from a secure location."
;;   (with-temp-buffer
;;     (insert-file-contents-literally "~/.open_api_key")
;;     (string-trim (buffer-string))))

;; (use-package gptel)
;; (setq gptel-api-key (smv/gptel-api-key))

(use-package youdotcom
    :bind
    ("C-c y" . youdotcom-enter)
    :config
    (setq youdotcom-rag-api-key ""))

(use-package magit
    :commands magit-status
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

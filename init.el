;; Making lsp more responsive
(setq gc-const-threshold (* 80 1000 1000))
(setq read-process-output-max (* 64 1024)) ;; 64kb

;; remove noise for not non allowed command in emacs if your system make them
(setq ring-bell-function 'ignore)

;; Make frame transparency overridable
(defvar smv/frame-transparency '(90 . 90))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq-default indent-tabs-mode nil)
(set-face-attribute 'default nil :height 159)

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
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

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
(setq-default explicit-shell-file-name "/opt/homebrew/bin/fish")
(setq eshell-aliases-file "~/.emacs.d/aliases")

(use-package eshell-toggle
  :bind ("C-x C-z" . eshell-toggle))

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
(add-to-list 'default-frame-alist '(font . "DaddyTimeMono Nerd Font Mono"))

;; some modes doesn't have to start with lines enable
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(require 'ansi-color)
(require 'eshell)
(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

(use-package vterm)

(use-package multi-vterm
  :after vterm 
  :ensure t
  :bind (("C-c v n" . multi-vterm-project)
         ("C-c v r" . multi-vterm-rename-buffer)
         ("C-x C-y" . multi-vterm-dedicated-toggle))
  :config
  (define-key vterm-mode-map [return]                      #'vterm-send-return)
  ;; dedicated terminal height of 30%
  (setq multi-vterm-dedicated-window-height-percent 30))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :custom
  (repeat-too-dangerous '(kill-this-buffer))
  (repeat-exit-timeout 5))

(use-package xah-fly-keys
  :init
  (setq xah-fly-use-control-key nil)
  (setq xah-fly-use-meta-key nil)
  :config
  (xah-fly-keys-set-layout "colemak")
  (xah-fly-keys 1))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode))

(use-package surround
  :ensure t
  :bind-keymap ("C-c s" . surround-keymap))

(use-package windmove
  :straight nil
  :config
  (windmove-default-keybindings))

(use-package doom-themes)
(use-package ef-themes
    :config
    (load-theme 'manoj-dark t))

(use-package all-the-icons
    :if (display-graphic-p))

(use-package all-the-icons-ivy
  :after all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons)

(use-package nerd-icons)

(use-package which-key ;; print next keybindings
	     :init (which-key-mode) ;; happens before the package is loaded
	     :diminish which-key-mode
	     :config ;; only runs after the mode is loaded
	     (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
          :map ivy-minibuffer-map
          ("TAB" . ivy-alt-done)
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

(defhydra hydra-text-scale (:timeout 3)
  "scalte text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(smv/leader-keys ;; use general to set a keybinding to quickly change text size
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))

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
        (set-face-attribute (car face) nil :font "Chalkboard" :weight 'regular :height (cdr face)))
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

        ("st" "School Todos" tags-todo "+@school/TODO")
        ("sp" "School Projects" tags-todo "+@school/ACTIVE")
        
        ("pt" "Personal Todos" tags-todo "+personal/TODO")
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


;; Install htmlize for source block hightlighting
(use-package htmlize
  :after org)

(use-package org-fragtog
    :hook (org-mode-hook . org-fragtog-mode))

(use-package ox-reveal)
(setq Org-Reveal-root "~/.config/emacs/reveal-js")
(setq Org-Reveal-title-slide nil)

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

;; Automatically tangle our Emacs.org config file when we save it
(defun smv/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/emacs.org"))
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

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode))

(use-package cider)
(use-package clojure-ts-mode
  :mode ("\\.clj\\'" . clojure-ts-mode)
  :hook (clojure-ts-mode . eglot-ensure))

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
    :config
    (setq typescript-indent-level 2))

(use-package php-mode
:hook (php-mode . eglot-ensure)
:mode "\\.php\\'")

(use-package eglot-java
    :after eglot)

;;(use-package ess)

(use-package rust-mode)

(use-package rust-ts-mode
  :hook (rust-ts-mode . eglot-ensure)
  :mode "\\.rs\\'"
  :bind-keymap
  ("C-c c" . rust-mode-map))

(use-package flutter)

(use-package dart-mode
    :hook (dart-mode . eglot-ensure)
    :mode "\\.dart\\'")

(use-package dape)

(use-package company
    :after eglot-mode
    :hook (eglot-managed-mode . company-mode)
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

(use-package docker
    :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

(setq copilot-node-executable "/opt/homebrew/bin/node")

;; keybindings that are active when copilot shows completions
(define-key copilot-mode-map (kbd "C-M-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "C-M-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "C-M-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "C-M-<return>") #'copilot-accept-completion-by-line)
(define-key copilot-mode-map (kbd "C-M-<down>") 'copilot-accept-completion)

(use-package magit
    :commands magit-status
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

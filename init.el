(setq gc-const-threshold (* 50 1000 1000))

;; You will most likely need to adjust this font size for your system!
(defvar smv/default-font-size 112)
(defvar smv/default-variable-font-size 112)

;; remove noise for not non allowed command in emacs if your system make them
(setq ring-bell-function 'ignore)

;; Make frame transparency overridable
(defvar smv/frame-transparency '(90 . 90))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq-default indent-tabs-mode nil)

;; Initialize package sources
(require 'package)
(require 'cl)
(require 'dired-x)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                            ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

(setq use-package-always-ensure t)

(unless (package-installed-p 'quelpa)
    (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
    '(quelpa-use-package
    :fetcher git
    :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package dashboard
    :ensure t
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

(use-package undo-tree)
(global-undo-tree-mode)

(require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  (setq eshell-list-files-after-cd t)

;; Watch out you should have fish installed on your computer
  (setq shell-file-name "/usr/bin/fish")
  (setq eshell-aliases-file "~/.emacs.d/aliases")

  (use-package eshell-toggle
  :bind ("C-x C-z" . eshell-toggle))

(use-package vterm)
(use-package vterm-toggle
:bind ("C-x C-y" . vterm-toggle))


(use-package multi-vterm
      :config
      (add-hook 'vterm-mode-hook
		      (lambda ()
		      (setq-local evil-insert-state-cursor 'box)
		      (evil-insert-state)))
      (define-key vterm-mode-map [return]                      #'vterm-send-return)
      )

(global-set-key (kbd "M-g f") 'avy-goto-line) ;; go to a line but not with line number
(global-set-key (kbd "C-c l") 'avy-copy-line) ;; copy a line I'm not on

(global-set-key (kbd "C-c m") 'avy-move-line) ;; move a line I'm not on
(global-set-key (kbd "M-g w") 'avy-goto-word-1) ;; move to a word inputing 1 character
(global-set-key (kbd "C-:") 'avy-goto-char-timer) ;; move to a particular character on any window using 1 input character
(setq avy-timeout 0.7)

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
(add-to-list 'default-frame-alist '(font . "AnonymicePro Nerd Font"))

;; some modes doesn't have to start with lines enable
(dolist (mode '(org-mode-hook
            term-mode-hook
            shell-mode-hook
            eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "AnonymicePro Nerd Font" :height smv/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "AnonymicePro Nerd Font" :height smv/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "AnonymicePro Nerd Font" :height smv/default-variable-font-size :weight 'light)

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (use-package general ;; for setting keybindings
      :ensure t
      :config
      (general-create-definer smv/leader-keys
          :keymaps '(normal visual emacs)
          :prefix "SPC"
          :global-prefix "SPC")

      (smv/leader-keys
          "t" '(:ignore t :which-key "toggles")
          "tt" '(counsel-load-theme :which-key "choose theme")))

    ;; Activate vim keybindings inside of emacs
  (use-package evil
      :init
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (setq evil-want-C-u-scroll nil)
      (setq evil-want-C-d-scroll nil)
      (setq evil-v$-excludes-newline t)
      (setq evil-respect-visual-line-mode t)
      (setq evil-undo-system 'undo-tree)
      (setq evil-want-C-i-jump nil)
      :config
      (evil-mode 1)
      (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
      (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

      (define-key evil-insert-state-map (kbd "C-n") nil)
      (define-key evil-insert-state-map (kbd "C-p") nil)

      (define-key evil-normal-state-map (kbd "C-n") nil)
      (define-key evil-normal-state-map (kbd "C-p") nil)

      (define-key evil-normal-state-map (kbd "C-u") 'evil-jump-forward)

      (define-key evil-visual-state-map (kbd "C-n") nil)
      (define-key evil-visual-state-map (kbd "C-p") nil)

      (define-key evil-visual-state-map (kbd "C-a") nil)
      (define-key evil-normal-state-map (kbd "C-a") nil)
      (define-key evil-insert-state-map (kbd "C-a") nil)

      (define-key evil-visual-state-map (kbd "C-e") nil)
      (define-key evil-normal-state-map (kbd "C-e") nil)
      (define-key evil-insert-state-map (kbd "C-e") nil)

      (define-key evil-visual-state-map (kbd "C-d") nil)
      (define-key evil-normal-state-map (kbd "C-d") nil)
      (define-key evil-insert-state-map (kbd "C-d") nil)

      (evil-set-initial-state 'messages-buffer-mode 'normal)
      (evil-set-initial-state 'dashboard-mode 'normal))


;; Add evil-keybindings to more modes inside of emacs
  (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))


  (use-package evil-surround
      :ensure t
      :config
      (global-evil-surround-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-badger t))

(use-package all-the-icons
    :if (display-graphic-p))

(use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))

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
  :after ivy
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

(use-package treemacs
:ensure t
:defer t
:init
(with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
:config
(progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-extension-regex            treemacs-last-period-regex-value
        treemacs-file-follow-delay               0.2
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          nil
        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                        'left
        treemacs-read-string-input               'from-child-frame
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home        nil
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                (not (null treemacs-python-executable)))
    (`(t . t)
        (treemacs-git-mode 'deferred))
    (`(t . _)
        (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
    :bind
    (:map global-map
            ("M-0"       . treemacs-select-window)
            ("C-x t 1"   . treemacs-delete-other-windows)
            ("C-x t t"   . treemacs)
            ("C-x t d"   . treemacs-select-directory)
            ("C-x t B"   . treemacs-bookmark)
            ("C-x t C-t" . treemacs-find-file)
            ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
:after (treemacs evil)
:ensure t)

(use-package treemacs-projectile
:after (treemacs projectile)
:ensure t)

(use-package treemacs-all-the-icons)

(use-package treemacs-icons-dired
:hook (dired-mode . treemacs-icons-dired-enable-once)
:ensure t)

(use-package treemacs-magit
:after (treemacs magit)
:ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
:after (treemacs persp-mode) ;;or perspective vs. persp-mode
:ensure t
:config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
:after (treemacs)
:ensure t
:config (treemacs-set-scope-type 'Tabs))

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

(use-package hydra) ;; hydra permit to repeat a command easily without repeating the keybindings multiple times

(defhydra hydra-text-scale (:timeout 3)
  "scalte text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(smv/leader-keys ;; use general to set a keybinding to quickly change text size
  "ts" '(hydra-text-scale/body :which-key "scale text"))

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
                        (org-level-5 . 1.1)
                        (org-level-6 . 1.1)
                        (org-level-7 . 1.1)
                        (org-level-8 . 1.1)))
        (set-face-attribute (car face) nil :font "Roboto Condensed" :weight 'light :height (cdr face)))
        ;; Ensure that anything that should be fixed-pitch in Org files appears that way
        (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
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

(use-package org
    :ensure org-contrib)

(use-package org-notify
    :ensure nil
    :after org
    :config
    (org-notify-start)

    (org-notify-add 'default
		'(:time "1d" :period "30m" :duration 50 :actions -notify)
		'(:time "2d" :period "50m" :duration 40 :actions -notify)
		'(:time "3d" :period "1h" :duration 20 :actions -notify))
)

(defun smv/org-mode-setup()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (setq evil-auto-indent nil))


(use-package org ;; org-mode, permit to take notes and other interesting stuff with a specific file extension
    :hook (org-mode . smv/org-mode-setup)
    :config
    (setq org-ellipsis " ▼:")
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    (setq org-agenda-files
            '("~/.org/todo.org"
            "~/.org/projects.org"
            "~/.org/journal.org"))

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
            ("agenda" . ?a)
            ("planning" . ?p)
            ("publish" . ?P)
            ("batch" . ?b)
            ("note" . ?n)
            ("idea" . ?i)))

    (setq org-agenda-custom-commands
        '(("d" "Dashboard"
        ((agenda "" ((org-deadline-warning-days 7)))
        (todo "NEXT"
            ((org-agenda-overriding-header "Next Tasks")))
        (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

        ("n" "Next Tasks"
        ((todo "NEXT"
            ((org-agenda-overriding-header "Next Tasks")))))

        ("s" "School Tasks" tags-todo "+school")
        ("P" "Projects" tags-todo "+projects")

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
        ((org-agenda-overriding-header "Low Effort Tasks")
        (org-agenda-max-todos 20)
        (org-agenda-files org-agenda-files)))))

    (setq org-capture-templates ;; quickly add todos entries without going into the file
        `(("t" "Tasks")
        ("tt" "Task" entry (file+olp "~/.org/todo.org" "Inbox")
                "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

        ("j" "Journal Entries")
        ("jm" "Meeting" entry
                (file+olp+datetree "~/.org/journal.org")
                "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
                :clock-in :clock-resume
                :empty-lines 1)))

    (smv/org-font-setup))

(use-package org-bullets ;; change the bullets in my org mode files
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶")))

;; Outline numbering for org mode
(use-package org-num
:load-path "lisp/"
:after org
:hook (org-mode . org-num-mode))

(use-package org-projectile)

(use-package olivetti) ;; use to stretch the page on the center to be able to focus on document writing

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
                      (expand-file-name "~/.emacs.d/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'smv/org-babel-tangle-config)))

(global-set-key (kbd "C-M-;") 'comment-region)

(defun efs/lsp-mode-setup ()
(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
(lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
:commands (lsp lsp-deferred)
:hook (lsp-mode . efs/lsp-mode-setup)
:init
(setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
:config
(lsp-enable-which-key-integration t))

;; only watch over the current project directory files
(setq lsp-file-watch-ignored (list (rx-to-string `(and (or bos "/" (and "/home" (* any)) "/") (not (any ".")))
                                               'no-group)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-enable t)
  :bind
  (:map evil-normal-state-map ("H" . lsp-ui-doc-toggle)))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package flycheck)

(use-package yasnippet
    :config (yas-global-mode))

(use-package yaml-mode
:mode (("\\.yml\\'" . web-mode)
            ("\\.yaml\\'" . web-mode)
            ))

(use-package dap-mode
    :after
    lsp-mode
    :config
    (dap-auto-configure-mode)
    :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
)


(evil-define-key 'normal dap-mode-map (kbd "K") #'dap-tooltip-at-point)

; Basic dap-mode keybindings (similar to VSCode)
(define-key dap-mode-map (kbd "<f5>") 'dap-debug)
(define-key dap-mode-map (kbd "<f9>") 'dap-breakpoint-toggle)
(define-key dap-mode-map (kbd "C-<f9>") 'dap-breakpoint-condition)
(define-key dap-mode-map (kbd "M-<f9>") 'dap-breakpoint-log-message)
(define-key dap-mode-map (kbd "<f10>") 'dap-next)
(define-key dap-mode-map (kbd "<f11>") 'dap-step-in)
(define-key dap-mode-map (kbd "S-<f11>") 'dap-step-out)
(define-key dap-mode-map (kbd "<f12>") 'dap-ui-inspect-thing-at-point)
(define-key dap-mode-map (kbd "C-<f5>") 'dap-stop-thread)
(define-key dap-mode-map (kbd "S-<f5>") 'dap-restart-frame)

;; dap-ui keybindings
(define-key dap-ui-repl-mode-map (kbd "C-<f5>") 'dap-stop-thread)
(define-key dap-ui-repl-mode-map (kbd "S-<f5>") 'dap-restart-frame)
(define-key dap-ui-repl-mode-map (kbd "<f12>") 'dap-ui-inspect-thing-at-point)


(require 'dap-cpptools)

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
    (web-mode . lsp-deferred)
    :config
    (require 'dap-firefox)
    (dap-firefox-setup)
)

(setq dap-firefox-debug-program '("node" "/home/vanieb/.emacs.d/var/dap/extensions/vscode/firefox-devtools.vscode-firefox-debug/extension/dist/adapter.bundle.js"))

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

(use-package lsp-tailwindcss
    :init
    (setq lsp-tailwindcss-add-on-mode t))

(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)
            ("\\.ts\\'" . rjsx-mode))
  :hook
  (rjsx-mode . emmet-mode)
  (rjsx-mode . prettier-mode)
  (rjsx-mode . lsp-deferred))

(use-package prettier)

(use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . lsp-deferred)
    :config
    (setq typescript-indent-level 2)
    (require 'dap-node)
    (dap-node-setup))

(use-package php-mode
  :mode "\\.php\\'"
  )

(use-package lsp-java
    :config
    (add-hook 'java-mode-hook 'lsp)
    ;; current VSCode defaults for quick load
)

(use-package markdown-mode)
(use-package poly-R)

(add-to-list 'auto-mode-alist
            '("\\.[rR]md\\'" . poly-gfm+r-mode))

;; use braces around code block language strings:
(setq markdown-code-block-braces t)

(use-package rust-mode
    :hook (rust-mode . lsp-deferred)
    :config
    (require 'dap-cpptools)
    (dap-cpptools-setup))

(use-package flutter)

(use-package dart-mode
    :mode "\\.dart\\'"
    :hook (dart-mode . lsp-deferred)
)

(use-package lsp-dart
    :config
    (add-hook 'dart-mode-hook 'lsp))

(setq lsp-dart-sdk-dir "/home/vanieb/development/flutter/bin/cache/dart-sdk")
(setq lsp-dart-flutter-sdk "/home/vanieb/development/flutter")
(setq flutter-sdk-path "/home/vanieb/development/flutter")

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package docker
    :ensure t
    :bind ("C-c d" . docker))

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
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


(use-package copilot
:quelpa (copilot :fetcher github
                  :repo "zerolfx/copilot.el"
                  :diminish
                  :branch "main"
                  :files ("dist" "*.el")
))
;; keybindings that are active when copilot shows completions
(define-key copilot-mode-map (kbd "C-M-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "C-M-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "C-M-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "C-M-<down>") #'copilot-accept-completion-by-line)

;; global keybindings
(define-key global-map (kbd "C-M-<return>") #'rk/copilot-complete-or-accept)
(define-key global-map (kbd "C-M-<escape>") #'rk/copilot-change-activation)

;; Do copilot-quit when pressing C-g
(advice-add 'keyboard-quit :before #'rk/copilot-quit)

;; complete by pressing right or tab but only when copilot completions are
;; shown. This means we leave the normal functionality intact.
(advice-add 'right-char :around #'rk/copilot-complete-if-active)

;; deactivate copilot for certain modes
(add-to-list 'copilot-enable-predicates #'rk/copilot-enable-predicate)
(add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

(eval-after-load 'copilot
  '(progn
     ;; Note company is optional but given we use some company commands above
     ;; we'll require it here. If you don't use it, you can remove all company
     ;; related code from this file, copilot does not need it.
     (require 'company)
     (global-copilot-mode)))

;; (defun smv/gptel-api-key ()
;;   "Retrieve my OpenAI API key from a secure location."
;;   (with-temp-buffer
;;     (insert-file-contents-literally "~/.open_api_key")
;;     (string-trim (buffer-string))))

;; (use-package gptel)
;; (setq gptel-api-key (smv/gptel-api-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
:commands magit-status
:custom
(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  
(setq gc-const-threshold (* 2 1000 1000))

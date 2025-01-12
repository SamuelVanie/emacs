(setq gc-const-threshold (* 80 1000 1000))

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


;; auto refresh buffers when files changes
(global-auto-revert-mode t)

;; setting transparency for the window
(cond
 ((eq system-type 'darwin)  ;; macOS
  (set-frame-parameter (selected-frame) 'alpha '(92 . 50))
  (add-to-list 'default-frame-alist '(alpha . (92 . 50))))
 ((eq system-type 'gnu/linux)  ;; Linux
  (set-frame-parameter nil 'alpha-background 60)
  (add-to-list 'default-frame-alist '(alpha-background . 60)))
 )

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

;; Move by half pages
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

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
  :bind
  (:map dired-mode-map
        ("k" . dired-create-empty-file))
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  ;; Gets my files from the gifs directory
  ;; excluding the ones that ends with ~ and t (for my links.txt file)
  (let ((gifs (let* (
                     (root-dir (concat user-emacs-directory "gifs"))
                     (file-names (directory-files root-dir nil "^[^.].*[^~t]$")))
                (mapcar (lambda (x) (concat root-dir "/" x)) file-names))))
    (setq dashboard-startup-banner gifs)))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package treesit-auto
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
(setq explicit-shell-file-name "/bin/zsh")

(use-package envrc)

;; Writing a function that will permit to load the packages from an emacsclient that weren't launched inside a nix shell
;; The function assumes that the folder that contains the lisp code
;; that emacs should requires is in /nix-store-location/share/emacs/site-lisp/elpa/pkg-name-version/
(defun smv/add-nix-pkg-to-lpath (PKG_ENV)
  "Load the PKG_ENV directory to the load path of current emacs session
  it permits to then require the package"

  (let ((pkg-nix-path (getenv PKG_ENV)))

    (unless pkg-nix-path
      (user-error "Environment variable '%s' is not set" PKG_ENV))

    (let ((pkg-suffix "/share/emacs/site-lisp/elpa/"))

      (string-match "-emacs-\\([^/]+\\)" pkg-nix-path)

      (let* ((pkg-full-path (match-string 1 pkg-nix-path))
             (path-to-add (concat pkg-nix-path pkg-suffix pkg-full-path)))
        (unless (member path-to-add load-path)
          (add-to-list 'load-path path-to-add))))))

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
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
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
    (set-frame-font "JetbrainsMono Nerd Font-19" nil t)
  (add-to-list 'default-frame-alist '(font . "JetbrainsMono Nerd Font-15")))

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

(use-package hydra) ;; hydra permit to repeat a command easily without repeating the keybindings multiple
(use-package general) ;; permit to define bindings under another one easily

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :custom
  (repeat-too-dangerous '(kill-this-buffer))
  (repeat-exit-timeout 5))

(use-package xah-fly-keys
  :ensure t
  :init
  (setq xah-fly-use-control-key nil)
  (setq xah-fly-use-meta-key nil)
  :config
  (xah-fly-keys-set-layout "colemak")
  (define-key xah-fly-command-map (kbd "/") nil)
  (define-key xah-fly-command-map (kbd ";") nil)
  (add-to-list 'xah-right-brackets "\"")
  (add-to-list 'xah-brackets "''")
  (add-to-list 'xah-left-brackets "\"")
  (xah-fly-keys 1))

;; navigate between functions
(define-key xah-fly-command-map (kbd "&") #'backward-sexp)
(define-key xah-fly-command-map (kbd "(") #'forward-sexp)

;; line manipulations
(define-key xah-fly-command-map (kbd ";") #'duplicate-line)

;; tabs manipulations
(general-define-key
 :keymaps 'xah-fly-command-map
 :prefix "#"
 "n" #'tab-new
 "d" #'dired-other-tab
 "f" #'find-file-other-tab
 "r" #'tab-rename
 "u" #'tab-close
 "l" #'tab-previous
 "y" #'tab-next)

;; buffer movements
(define-key xah-fly-command-map (kbd "@") #'previous-buffer)
(define-key xah-fly-command-map (kbd "$") #'next-buffer)
(general-define-key
 :keymaps 'xah-fly-command-map
 :prefix "%"
 "s" #'scratch-buffer)

(define-key xah-fly-command-map (kbd "<") #'back-to-indentation)
(define-key xah-fly-command-map (kbd ">") #'end-of-visual-line)


(defun smv/surround-region (character)
  (interactive "sEnter a character:")
  (xah-insert-bracket-pair character character))

;; Some more complex commands
(define-key xah-fly-command-map (kbd "SPC s x") #'smv/surround-region)
(define-key xah-fly-command-map (kbd "SPC e a") #'xah-select-text-in-quote)

(use-package avy
  :after xah-fly-keys
  :straight nil
  :bind
  (:map xah-fly-command-map
        ("* *" . avy-goto-char-in-line)
        ("* c" . avy-goto-char)
        ("* l d" . avy-kill-whole-line)
        ("* l l" . avy-goto-end-of-line)
        ("* <up>" . avy-goto-line-above)
        ("* <down>" . avy-goto-line-below)
        ("* l y" . avy-copy-line)
        ("* r d" . avy-kill-region)
        ("* r y" . avy-copy-region)
        ("* r t" . avy-transpose-lines-in-region)
        ("* r r" . avy-resume)
        ("* r m" . avy-move-region)))

(use-package windmove
  :after xah-fly-keys
  :straight nil
  :bind
  (:map xah-fly-command-map
        ("/ w n" . windmove-left)
        ("/ w i" . windmove-right)
        ("/ w e" . windmove-down)
        ("/ w u" . windmove-up)
        ("/ w +" . balance-windows)
        ("/ w m" . maximize-window)
        ("/ w s n" . windmove-swap-states-left)
        ("/ w s i" . windmove-swap-states-right)
        ("/ w s e" . windmove-swap-states-down)
        ("/ w s u" . windmove-swap-states-up)))

(use-package vterm)

(use-package multi-vterm
  :ensure t
  :bind (("C-c v n" . multi-vterm-project)
         ("C-c v f" . multi-vterm)
         ("C-c v r" . multi-vterm-rename-buffer)
         ("C-x C-y" . multi-vterm-dedicated-toggle))
  :config
  (define-key vterm-mode-map [return]                      #'vterm-send-return)
  ;; terminal height percent of 30
  (setq multi-vterm-dedicated-window-height-percent 45))

(use-package doom-themes)
(use-package ef-themes
  :config (load-theme 'doom-pine t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package nerd-icons)

(use-package all-the-icons-dired
  :after all-the-icons)

(use-package which-key ;; print next keybindings
  :init (which-key-mode) ;; happens before the package is loaded
  :diminish which-key-mode
  :config ;; only runs after the mode is loaded
  (setq which-key-idle-delay 0.3))

(use-package vertico
  :init
  (vertico-mode)
  :config
  ;; disable case sensitiveness for files and dir
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (setq completion-styles '(basic substring partial-completion flex))
  (keymap-set vertico-map "?" #'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "M-TAB" #'minibuffer-complete))

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package consult
  :after xah-fly-keys
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ("C-s" . consult-line)
  ("M-y" . consult-yank-from-kill-ring)
  ("M-g M-g" . consult-goto-line)
  (:map xah-fly-command-map
        ("/ c f" . consult-fd)
        ("/ c s" . consult-ripgrep)
        ("/ c i" . consult-imenu)
        ("/ c k" . consult-kmacro)
        ("/ c m" . consult-global-mark)
        ("SPC t" . consult-buffer))
  )

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult-lsp
  :after (consult lsp-mode)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

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

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

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

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(global-set-key (kbd "C-M-;") 'comment-region)

(use-package wgrep)
(global-set-key (kbd "C-c r") 'rgrep)

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
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

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

(use-package flycheck)

(use-package markdown-mode)

(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets)

(use-package auto-yasnippet
  :bind
  (:map xah-fly-command-map
        ("/ a w" . aya-create)
        ("/ a x" . aya-expand)
        ("/ a h" . aya-expand-from-history)
        ("/ a d" . aya-delete-from-history)
        ("/ a c" . aya-clear-history)
        ("/ a n" . aya-next-in-history)
        ("/ a p" . aya-previous-in-history)
        ("/ a s" . aya-persist-snippet)
        ("/ a o" . aya-open-line)
        ))

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

(use-package rust-mode)

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :bind-keymap
  ("C-c c" . rust-mode-map)
  :hook (rust-ts-mode . lsp-deferred))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :hook (ruby-ts-mode . lsp-deferred))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-mode
        ("M-p" . company-manual-begin))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

(use-package company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine t))

(use-package dap-mode
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

(use-package dockerfile-mode)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind
  (:map copilot-completion-map
        ("C-M-<down>" . copilot-accept-completion)
        ("C-M-<up>" . copilot-accept-completion-by-word)
        ("C-M-<right>" . copilot-next-completion)
        ("C-M-<left>" . copilot-previous-completion)
        )
  :ensure t)

(use-package magit
    :commands magit-status
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

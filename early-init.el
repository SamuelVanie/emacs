(setq package-enable-at-startup nil)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-file-dialog nil
      use-short-answers t
      enable-recursive-minibuffers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq initial-frame-alist `((horizontal-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil)
                            (border-width . 0)))


(defun prot-emacs-no-minibuffer-scroll-bar (frame)
  "Remove the minibuffer scroll bars from FRAME."
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent)))

(add-hook 'after-make-frame-functions #'prot-emacs-no-minibuffer-scroll-bar)

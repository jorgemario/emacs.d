;;; simo-ui.el -- UI setup

;;; Commentary:

;;; Code:

(require 'use-package)

;; trust all themes
(setq custom-safe-themes t)

(set-frame-font "Hack 14" nil t)

;; see https://unix.stackexchange.com/questions/29786/font-size-issues-with-emacs-in-linum-mode/30087#30087
;; (eval-after-load "linum"
;;   '(set-face-attribute 'linum nil :height 120))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;; powerline (status line)
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package all-the-icons
  :ensure t)

(provide 'simo-ui)

;;; simo-ui.el ends here

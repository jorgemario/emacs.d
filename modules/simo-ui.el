;;; simo-ui.el -- UI setup

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'linum)

;; trust all themes
(setq custom-safe-themes t)

;; linum 4 char width
(setq linum-format "%4d ")

;; fix line number scaling
;; see https://unix.stackexchange.com/questions/29786/font-size-issues-with-emacs-in-linum-mode/30087#30087
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
          (ceiling (* (if (boundp 'text-scale-mode-step)
                  (expt text-scale-mode-step
                    text-scale-mode-amount) 1)
              (if (car (window-margins))
                  (car (window-margins)) 1)
              ))))

(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; powerline (status line)
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))


(provide 'simo-ui)

;;; simo-ui.el ends here

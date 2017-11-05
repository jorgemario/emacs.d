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
  "Fix linum for scaled text."
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
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-vim-theme))

;; (use-package flycheck-color-mode-line
;;   :ensure t
;;   :config
;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;;   '(flycheck-color-mode-line-error-face ((t (:inherit flycheck-fringe-error :background "#F07667" :foreground "#EFEFEF" :weight normal))))
;;   '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning :background "#DDA0DD" :foreground "#efefef" :weight normal)))))

;; (use-package spaceline
;;   :ensure t
;;   :disabled t
;;   :config
;;   (progn
;;     (require 'spaceline-config)
;;     (spaceline-spacemacs-theme)
;;     (setq powerline-height 20)))

(use-package smart-mode-line
  :ensure t
  :init
  (sml/setup)
  :config
  (setq sml/shorten-directory t
        sml/shorten-modes t)
  (add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d/" ":E:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/dev/beluca/" ":IMS:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/dev/ms-edge/" ":EDGE:")))

(provide 'simo-ui)

;;; simo-ui.el ends here

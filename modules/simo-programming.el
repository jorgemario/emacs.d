;;; simo-programming.el --- Programming configurations

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'diminish)

;; diminish eldoc mode
(diminish 'eldoc-mode)

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package lisp-mode
  :config
  (defun my-visit-ielm ()
    "Switch to default `ielm' buffer.
     Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'my-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :bind (("M-k" . paredit-raise-sexp)
         :map paredit-mode-map
         ("M-(" . paredit-wrap-round)
         ("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)
         ("<DEL>" . paredit-forward-delete)
         ("<backspace>" . paredit-backward-delete))
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; a little more syntax highlighting
(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (font-lock-add-keywords
                                  nil
                                  '(("(\\(facts?\\)"
                                     (1 font-lock-keyword-face))
                                    ("(\\(background?\\)"
                                     (1 font-lock-keyword-face))))
                                 (define-clojure-indent (fact 1))
                                 (define-clojure-indent (facts 1)))))

(use-package clj-refactor
  :ensure t
  :bind ("<s-return>" . cljr-add-missing-libspec)
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1) ; for adding require/use/import statements
              ;; This choice of keybinding leaves cider-macroexpand-1 unbound
              (cljr-add-keybindings-with-prefix "C-c r"))))

(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (bind-key "C-c C-k"
            '(lambda ()
               (interactive)
               (save-buffer)
               (cider-load-buffer))
            cider-mode-map)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  :bind (("s-<tab>" . company-complete)))

;; Requires joker https://github.com/candid82/joker#installation
;; Make sure to create provider the .joker file to avoid  false positives. ln -s ~/.emacs.d/.joker ~/.joker
(use-package flycheck-joker
  :ensure t)

;; common-lisp/quicklisp with sbcl
(defvar quicklisp-helper-src "~/quicklisp/slime-helper.el")

(when (file-exists-p quicklisp-helper-src)
  (message "Loading quicklisp helper...")
  (load (expand-file-name quicklisp-helper-src))

  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(provide 'simo-programming)

;;; simo-programming.el ends here

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
  :bind ("M-k" . paredit-raise-sexp)
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

(provide 'simo-programming)

;;; simo-programming.el ends here

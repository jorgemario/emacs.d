;;; init.el --- Personal Emacs configuration
;;
;; URL: https://github.com/jorgemario/emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.

(require 'package)

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; update the package metadata
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)

(defvar emacs-dir (file-name-directory load-file-name))

;; Core modules directory.
(defvar modules-dir (expand-file-name "modules" emacs-dir))

;; Personal configuration directory.
(defvar personal-dir (expand-file-name "personal" emacs-dir))

;; Add modules directory to emacs's load-path
(add-to-list 'load-path modules-dir)

(when (file-exists-p personal-dir)
  (message "Loading personal configuration files...")
  (mapc 'load (directory-files personal-dir 't "^[^#\.].*el$")))

(message "Loading core modules...")

(require 'simo-core)
;(require 'simo-defaults)
(require 'simo-magit)
(require 'simo-org)
(require 'simo-programming)
(require 'simo-ui)

(when (eq system-type 'darwin)
  (message "Loading osx configuration file...")
  (require 'simo-osx))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (helm-descbinds helm-ag helm-projectile helm clj-refactor powerline iedit gruvbox-theme rainbow-mode ztree zop-to-char yasnippet yaml-mode which-key use-package super-save rainbow-delimiters pt projectile paredit noflet multiple-cursors move-text markdown-mode magit inflections inf-clojure imenu-anywhere hydra flycheck expand-region exec-path-from-shell evil elisp-slime-nav edn easy-kill crux company cider avy anzu aggressive-indent ag)))
;;  '(safe-local-variable-values
;;    (quote
;;     ((checkdoc-package-keywords-flag)
;;      (eval when
;;            (require
;;             (quote rainbow-mode)
;;             nil t)
;;            (rainbow-mode 1))
;;      (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((((class color) (min-colors 16777215)) (:background "#282828" :foreground "#fdf4c1")) (((class color) (min-colors 255)) (:background "#262626" :foreground "#ffffaf")))))

;;; init.el ends here

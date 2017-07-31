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
(require 'simo-org)
(require 'simo-programming)
(require 'simo-git)
(require 'simo-ui)

(when (eq system-type 'darwin)
  (message "Loading osx configuration file...")
  (require 'simo-osx))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (hiwin web-mode clojure-mode-extra-font-locking monokai-theme zenburn-theme dired-k zop-to-char yaml-mode which-key use-package undo-tree super-save rainbow-mode rainbow-delimiters pt powerline move-text markdown-mode magit imenu-anywhere iedit helm-projectile helm-descbinds helm-ag flycheck expand-region exec-path-from-shell elisp-slime-nav easy-kill diff-hl crux company clj-refactor avy anzu ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

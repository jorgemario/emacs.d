;;; simo-core.el --- Emacs core setup

;;; Commentary:
;; Base configuration.

;;; Code:

(require 'use-package)

(use-package diminish
  :ensure t)

(require 'diminish)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst simo-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p simo-savefile-dir)
  (make-directory simo-savefile-dir))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" simo-savefile-dir)
        recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; C-n insert new lines if the point is at the end o
(setq next-line-add-newlines t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(diminish 'auto-revert-mode)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; winner mode
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; extend the help commands
(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)
(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

;; easy keys to split window. Key based on ErgoEmacs keybinding
;; see http://ergoemacs.org/emacs/effective_emacs.html
(global-set-key (kbd "M-2") 'delete-window) ; close current pane
(global-set-key (kbd "M-3") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-4") 'split-window-below) ; split pane top/bottom
(global-set-key (kbd "M-5") 'split-window-right) ; split pane top/bottom

;; other frame
(global-set-key (kbd "M-`") 'other-frame)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; highlight the current line
(global-hl-line-mode +1)

;; delete extra spaces
(setq backward-delete-char-untabify-method 'hungry)

;; toggle line numbers
(global-set-key (kbd "C-x l") #'linum-mode)

(defun simo/kill-region ()
  "Kill region do nothing if no region."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)))

(global-set-key (kbd "C-w") #'simo/kill-region)

;; Narrow and widen region (toogle)
;; see TODO: Endless parenthesis link
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
   Dwim means: region, org-src-block, org-subtree, or
   defun, whichever applies first. Narrowing to
   org-src-block actually calls `org-edit-src-code'.

   With prefix P, don't widen, just narrow even if buffer
   is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen) (recenter))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (define-key LaTeX-mode-map "\C-xn"
              nil)))

(defun copy-line (arg)
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun double-line (arg)
  "copy line and place it below the original"
  (interactive "p")
  (copy-line arg)
  (yank)
  (move-end-of-line))

(global-set-key (kbd "C-c d") 'double-line)

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package ag
  :ensure t)

(use-package projectile
  :ensure t
  :pin melpa-stable
  :bind ("s-p" . projectile-command-map)
  :config
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (setq projectile-globally-ignored-files
        (append '(".pyc"
                  ".class"
                  "~")
                projectile-globally-ignored-files))
  (setq projectile-globally-ignored-directories
        (append '("node_modules"
                  "classes"
                  "figwheel_temp"
                  "js/compiled"
                  ".cljs_rhino_repl"
                  ".idea"
                  "cljsbuild")
                projectile-globally-ignored-directories))
  (projectile-global-mode))

(use-package pt
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  ;(setq-default abbrev-mode t)
  )

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" simo-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" simo-savefile-dir))
  (savehist-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package hiwin
  :ensure t)

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

;; (use-package dired-k
;;   :ensure t
;;   :config
;;   (setq dired-k-style 'k.sh)
;;   (add-hook 'dired-initial-position-hook 'dired-k)
;;   (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package whitespace
  ;; :init
  ;; (dolist (hook '(prog-mode-hook text-mode-hook))
  ;;   (add-hook hook #'whitespace-mode))
  ;; (add-hook 'before-save-hook #'whitespace-cleanup)
  :diminish whitespace-mode
  :bind ("C-x w" . whitespace-mode)
  :config
  (setq whitespace-line-column 100) ;; limit line length
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '((space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          ))
  (setq whitespace-style '(tabs empty trailing newline newline-mark)))

(defun simo/recentf ()
  "Invoke the appropriate recentf action."
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'helm-projectile-recentf)
    (call-interactively 'helm-recentf)))

;; Config borrowed from prelude.
;; See https://github.com/bbatsov/prelude/blob/master/modules/prelude-helm.el
(use-package helm
  :ensure t
  :pin melpa-stable
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (require 'helm-eshell)

          (use-package helm-projectile
            :ensure t
            :pin melpa-stable
            :commands helm-projectile
            :bind ("C-c p h" . helm-projectile))

          (setq projectile-completion-system 'helm)

          (use-package helm-ag
            :defer 10
            :ensure t
            :pin melpa-stable)

          (use-package helm-descbinds
            :ensure t
            :pin melpa-stable)

          (when (executable-find "curl")
            (setq helm-google-suggest-use-curl-p t))

          ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
          ;; discussion of these options.
          (setq helm-split-window-in-side-p           t
                helm-buffers-fuzzy-matching           t
                helm-move-to-line-cycle-in-source     t
                helm-ff-search-library-in-sexp        t
                helm-ff-file-name-history-use-recentf t)

          ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
          ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
          ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
          (global-unset-key (kbd "C-x c"))

          ;; use helm to list eshell history
          (add-hook 'eshell-mode-hook
                    #'(lambda ()
                        (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

          (substitute-key-definition 'find-tag 'helm-etags-select global-map)

          ;; enable modes
          (helm-descbinds-mode)
          (helm-mode 1)
          (helm-projectile-on))

  :bind (("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("M-p" . helm-projectile)
         ("M-o" . simo/recentf)
         ("C-`" . helm-resume)
         ("M-x" . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-h f" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         :map helm-command-map
         ("o" . helm-occur)
         ("g" . helm-do-grep)
         ("C-c w" . helm-wikipedia-suggest)
         ("SPC" . helm-all-mark-rings)
         :map isearch-mode-map
         ("C-o" . helm-occur-from-isearch)
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history)
                                        ;:map shell-mode-map
                                        ;("C-c C-l" . helm-comint-input-ring)
         ))

;(key-chord-mode 1)
;(key-chord-define-global "fm" 'helm-mini)
;(key-chord-define-global "lm" 'helm-do-ag-this-file)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t)

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package flyspell
  :config
  (bind-key "C-;" nil flyspell-mode-map)
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;; Mark and edit all copies of the marked region simultaneously.
;; Note: flyspell and iedit both register the key-bind C-;
;; We want to use the iedit keybinding.
(use-package iedit
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package super-save
  :ensure t
  :diminish super-save-mode
  :config
  (super-save-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ;("C-c s" . crux-ispell-word-then-abbrev)
         ))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package beginend
  :ensure t
  :config
  (add-hook 'dired-load-hook 'beginend-dired-mode))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

;; cursor start as bar (box used to indicate god mode)
(setq cursor-type 'bar)

;; god mode
(defun simo/update-cursor ()
  "Function to toggle the cursor type."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'simo/update-cursor)
(add-hook 'god-mode-disabled-hook 'simo/update-cursor)

(use-package god-mode
  :ensure t
  :disabled t
  :bind ("<escape>" . god-mode-all)
  :init
  (god-mode)
  :config
  (global-set-key (kbd "<escape>") 'god-mode-all)
  (define-key god-local-mode-map (kbd ".") 'repeat)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (defun god-update-cursor ()
    "Update my cursor."
    (setq cursor-type
          (if god-local-mode
              'box
            'bar)))
  ;;(add-hook 'god-mode-enabled-hook 'god-update-cursor)
  ;;(add-hook 'god-mode-disabled-hook 'god-update-cursor)
  (add-to-list 'god-exempt-major-modes 'sauron-mode)
  (add-to-list 'god-exempt-major-modes 'eshell-mode)
  (add-to-list 'god-exempt-major-modes 'org-agenda-mode)
  (add-to-list 'god-exempt-major-modes 'mingus-playlist-mode)
  (add-to-list 'god-exempt-major-modes 'mingus-browse-mode)
  (add-to-list 'god-exempt-major-modes 'twittering-mode)
  (add-to-list 'god-exempt-major-modes 'Man-mode)
  (add-to-list 'god-exempt-major-modes 'proced-mode)
  (add-to-list 'god-exempt-major-modes 'gnus-summary-mode)
  (add-to-list 'god-exempt-major-modes 'gnus-article-mode)
  (add-to-list 'god-exempt-major-modes 'gnus-group-mode)
  (add-to-list 'god-exempt-major-modes 'elfeed-search-mode)
  (add-to-list 'god-exempt-major-modes 'haskell-interactive-mode)
  (add-to-list 'god-exempt-major-modes 'epresent-mode)
  (add-to-list 'god-exempt-major-modes 'compilation-mode)
  (add-to-list 'god-exempt-major-modes 'Custom-mode)
  ;; Finally, a fix for key-translation-map by redefining the
  ;; `key-string-after-consuming-key' method, courtesy of
  ;; https://github.com/chrisdone/god-mode/issues/75
  (defun key-string-after-consuming-key (key key-string-so-far)
    "Interpret god-mode special keys for key (consumes more keys
if appropriate). Append to keysequence."
    (let ((key-consumed t) next-modifier next-key)
      (message key-string-so-far)
      (setq next-modifier
            (cond
             ((string= key god-literal-key)
              (setq god-literal-sequence t)
              "")
             (god-literal-sequence
              (setq key-consumed nil)
              "")
             ((and
               (stringp key)
               (not (eq nil (assoc key god-mod-alist)))
               (not (eq nil key)))
              (cdr (assoc key god-mod-alist)))
             (t
              (setq key-consumed nil)
              (cdr (assoc nil god-mod-alist))
              )))
      (setq next-key
            (if key-consumed
                (god-mode-sanitized-key-string (read-event key-string-so-far))
              key))
      (let* ((literal-key-string (concat next-modifier next-key))
             (translation (lookup-key key-translation-map (kbd literal-key-string)))
             (next-interpreted-key-string (or translation literal-key-string)))
        (if key-string-so-far
            (concat key-string-so-far " " next-interpreted-key-string)
          next-interpreted-key-string)))))


(provide 'simo-core)

;;; simo-core.el ends here

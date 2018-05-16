;;; simo-org.el --- Org configurations

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(defun simo/org-mode-hook ()
  (flyspell-mode)
  (visual-line-mode))

(add-hook 'org-mode-hook 'simo/org-mode-hook)

(setq org-directory "~/Documents/org")

(setq org-default-notes-file (concat org-directory "/notes.org"))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file (concat org-directory "/todo.org"))
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file (concat org-directory "/notes.org"))
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file (concat org-directory "/todo-review.org"))
               "* TODO Review %c\n%U\n" :immediate-finish t))))

;; stop C-c C-c within code blocks from querying
(setq org-confirm-babel-evaluate nil)

;; fontify code blocks in org files
(setq org-src-fontify-natively t)

;; Babel language support for org-mode. Babel allows you to embed
;; code directly into org files, but you must first specify which
;; languages to allow. Emacs-lisp should be enabled by default... I
;; specify it anyways... because it makes me happy.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (sh . t)
   (emacs-lisp . t)
   (python . t)
(scheme . t)))

(provide 'simo-org)

;;; simo-org.el ends here

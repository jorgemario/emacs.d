;;; simo-magit.el --- Magit configuration

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package magit
  :ensure t
  :config
  (bind-key "C-x g" '(lambda ()
                       (interactive)
                       (save-buffer)
                       (magit-status))))

(global-set-key (kbd "C-x N") 'magit-next-commit)

(provide 'simo-git)

;;; simo-git.el ends here


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

(provide 'simo-git)

;;; simo-magit.el ends here

;;; simo-osx.el --- OSX specific configurations

;;; Commentary:

;;; Code:

;; swap command and alt keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(message "Command is now bound to META and Option is bound to SUPER.")

;; Fullscreen!
(setq ns-use-native-fullscreen nil) ; Not Lion style
(bind-key "<s-return>" 'toggle-frame-fullscreen)

;; disable the key that minimizes emacs to the dock because I don't
;; minimize my windows
(global-unset-key (kbd "C-z"))

(provide 'simo-osx)

;;; simo-osx.el ends here

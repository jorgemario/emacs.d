;;; simo-hydra.el --- Hydra modes

;;; Commentary:
;; Some useful hydra configurations

;;; Code:

(require 'use-package)

(use-package hydra
  :ensure t)

;; mini-vi
(global-set-key
 (kbd "C-z")
 (defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
                             :post (progn
                                     (set-cursor-color "#ffffff")
                                     (message
                                      "Thank you, come again.")))
      "vi"
      ("l" forward-char "forw")
      ("h" backward-char "back")
      ("j" next-line "next")
      ("k" previous-line "prev")
      ("a" crux-move-beginning-of-line "beg")
      ("e" move-end-of-line "beg")
      ("q" nil "quit")))


;; zoom
(defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

(provide 'simo-hydra)

;;; simo-hydra.el ends here

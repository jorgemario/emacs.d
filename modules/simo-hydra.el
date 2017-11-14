;;; simo-hydra.el --- Hydra modes

;;; Commentary:
;; Some useful hydra configurations

;;; Code:

(require 'use-package)
(require 'crux)

(use-package hydra
  :ensure t)

;; mini-vi
(defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
                         :post (progn
                                 (set-cursor-color "#ffffff")
                                 (message "Thank you, come again.")))
  "vi"
  ("l" forward-char "forw")
  ("h" backward-char "back")
  ("j" next-line "next")
  ("k" previous-line "prev")
  ("a" crux-move-beginning-of-line "beg")
  ("e" move-end-of-line "beg")
  ("q" nil "quit"))

(global-set-key (kbd "C-SPC") #'hydra-vi/body)

(defhydra hydra-window (:exit t :color orange :hint nil)
  "
 Split: _s_:split _H_:orizontal _V_:vertical
Delete: _o_nly  _da_ce  _x_window  _db_uffer  _df_rame
  Move: _S_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
;;; TODO(steveyang): when hjik is pressed after a command after a
;;; short threshold, automatically quit hydra map.
;;; If makes C-w l l works seamlessly
  ("h" windmove-left :exit t)
  ("j" windmove-down :exit t)
  ("k" windmove-up :exit t)
  ("l" windmove-right :exit t)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" cofi/smart-split)
  ("V" split-window-vertically)
  ("H" split-window-horizontally)
                                        ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("S" ace-swap-window)
  ("da" ace-delete-window)
  ("x" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
                                        ;("i" ace-maximize-window "ace-one" :color blue)
                                        ;("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump))

(global-set-key (kbd "C-x SPC") #'hydra-window/body)


;; zoom
(defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

(provide 'simo-hydra)

;;; simo-hydra.el ends here

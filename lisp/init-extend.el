;;; init-extend.el --- Making Emacs EXTEND

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(setq calendar-week-start-day 1)
(global-set-key (kbd "C-c l") 'calendar)

(global-set-key (kbd "C-c c") 'calc)

(global-set-key (kbd "C-h 4 m") 'man)
(global-set-key (kbd "C-h 4 w") 'woman)

(use-package vterm
  :ensure t
  :defer t
  :bind ("C-c t" . vterm))

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode))

(use-package wttrin
  :ensure t
  :defer t
  :init
  (setq wttrin-default-cities '("Indianapolis"))
  :bind ("C-c w" . wttrin))

(defvar games-map (make-sparse-keymap)
  "A keymap to which games can be added.")

(global-set-key (kbd "C-c g") games-map)

(use-package yahtzee
  :ensure t
  :defer t
  :bind (:map games-map
         ("y" . yahtzee)))

(use-package sudoku
  :ensure t
  :defer t
  :bind (:map games-map
         ("s" . sudoku)))

(use-package tetris
  :ensure t
  :defer t
  :bind (:map games-map
         ("t" . 'tetris)
         :map tetris-mode-map
         ("w" . tetris-move-bottom)
         ("a" . tetris-move-left)
         ("s" . tetris-mode-down)
         ("d" . tetris-move-right)
         ([left] . tetris-rotate-next)
         ([right] . tetris-rotate-prev)
         ([?\t] . tetris-pause-game)
         ("r" . tetris-start-game)
         ("e" . tetris-end-game)))

(use-package chess
  :ensure t
  :defer t
  :bind (:map games-map
         ("c" . chess)))

(use-package 2048-game
  :ensure t
  :defer t
  :bind (:map games-map
         ("2" . 2048-game)))

(provide 'init-extend)

;;; init-extend.el ends here

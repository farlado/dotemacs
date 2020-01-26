;;; early-package.el --- Package management

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(setq custom-file "/dev/null"
      package-selected-packages '(;; Core
                                  async
                                  use-package
                                  auto-package-update

                                  ;; Looks
                                  dashboard
                                  dracula-theme
                                  mood-line
                                  diminish
                                  rainbow-mode
                                  rainbow-delimiters

                                  ;; Desktop environment
                                  exwm
                                  exwm-edit
                                  exwm-mff
                                  dmenu
                                  minibuffer-line
                                  system-packages
                                  desktop-environment

                                  ;; Multimedia
                                  emms

                                  ;; Extra major modes
                                  graphviz-dot-mode
                                  markdown-mode

                                  ;; Functionality
                                  company
                                  company-emoji
                                  which-key
                                  smex
                                  buffer-move
                                  swiper
                                  popup-kill-ring
                                  hungry-delete
                                  avy
                                  sudo-edit

                                  ;; Programming
                                  magit
                                  haskell-mode
                                  highlight-indent-guides
                                  company-jedi
                                  flycheck
                                  avy-flycheck

                                  ;; org-mode
                                  toc-org
                                  org-bullets
                                  epresent

                                  ;; Other
                                  vterm
                                  nov
                                  wttrin

                                  ;; Games
                                  yahtzee
                                  sudoku
                                  chess
                                  2048-game))

(require 'package)
(defun package--save-selected-packages (&rest opt) nil)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(provide 'early-package)

;;; early-package.el ends here

;;; early-init.el --- Early Initialization of Farlado's Illiterate GNU Emacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; This is in spite of the fact so little is actually in this file.
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq load-prefer-newer t)

(setq-default apropos-do-all t)

(defvar startup/file-name-handler-alist file-name-handler-alist
  "Temporary storage for `file-name-handler-alist' during startup.")

(defun startup/revert-file-name-handler-alist ()
  "Revert `file-name-handler-alist' to its default value after startup."
  (setq file-name-handler-alist startup/file-name-handler-alist))

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun startup/reset-gc ()
  "Return garbage collection to normal parameters after startup."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/reset-gc)

(setq custom-file "/dev/null"
      package-selected-packages '(;; Core
                                  async
                                  use-package
                                  auto-package-update

                                  ;; Looks
                                  dashboard
                                  leuven-theme
                                  spaceline
                                  diminish
                                  rainbow-mode
                                  rainbow-delimiters

                                  ;; Desktop environment
                                  exwm
                                  dmenu
                                  minibuffer-line
                                  desktop-environment
                                  system-packages
                                  exwm-edit
                                  exwm-mff

                                  ;; Multimedia
                                  emms

                                  ;; Extra major modes
                                  graphviz-dot-mode
                                  markdown-mode

                                  ;; Functionality
                                  company
                                  company-emoji
                                  which-key
                                  ido-vertical-mode
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
                                  company-jedi
                                  flycheck
                                  avy-flycheck

                                  ;; org-mode
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

;;; early-init.el ends here

;;; pdumper.el --- Making a portable dump image

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(require 'package)
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp/xkb" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/wallpaper" user-emacs-directory))

(setq pdumper-load-path load-path
      pdumper-dumped t)

(dolist (package `(;; Core
                   async
                   use-package
                   server
                   auto-package-update

                   ;; Looks
                   dashboard
                   dracula-theme
                   mood-line
                   rainbow-mode
                   rainbow-delimiters

                   ;; Functionality
                   company
                   company-emoji
                   which-key
                   counsel
                   buffer-move

                   ;; Editing
                   graphviz-dot-mode
                   markdown-mode
                   flyspell
                   swiper
                   popup-kill-ring
                   hungry-delete
                   avy
                   sudo-edit

                   ;; Programming
                   haskell-mode
                   highlight-indent-guides
                   company-jedi
                   flycheck
                   avy-flycheck

                   ;; `org-mode'
                   org
                   toc-org
                   org-bullets
                   epresent
                   org-tempo

                   ;; Desktop Environment
                   exwm
                   exwm-randr
                   exwm-config
                   exwm-systemtray
                   ivy-posframe
                   dmenu
                   minibuffer-line
                   system-packages
                   desktop-environment
                   xkb
                   wallpaper

                   ;; Media
                   emms
                   emms-setup

                   ;; Other
                   nov
                   wttrin

                   ;; games
                   yahtzee
                   sudoku
                   tetris
                   chess
                   2048-game))
    (require package))

(load-theme 'dracula t t)

(dump-emacs-portable (expand-file-name "emacs.pdmp" user-emacs-directory))

;;; pdumper.el ends here

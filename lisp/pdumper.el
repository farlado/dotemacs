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

(add-to-list 'load-path (locate-user-emacs-file "lisp/xkb"))
(add-to-list 'load-path (locate-user-emacs-file "lisp/wallpaper"))

(setq pdumper-load-path load-path
      pdumper-dumped t)

(dolist (package `(;; Core
                   async
                   use-package
                   auto-package-update
                   server

                   ;; Looks
                   dashboard
                   dracula-theme
                   mood-line
                   page-break-lines
                   display-line-numbers
                   rainbow-mode
                   rainbow-delimiters

                   ;; Functionality
                   which-key
                   company
                   company-emoji
                   counsel
                   buffer-move
                   sudo-edit

                   ;; Editing
                   graphviz-dot-mode
                   markdown-mode
                   flyspell
                   swiper
                   popup-kill-ring
                   hungry-delete
                   avy

                   ;; Programming
                   haskell-mode
                   highlight-indent-guides
                   company-jedi
                   flycheck
                   flycheck-posframe
                   avy-flycheck

                   ;; `org-mode'
                   org
                   toc-org
                   org-bullets
                   epresent
                   org-tempo

                   ;; Other
                   nov
                   wttrin

                   ;; games
                   yahtzee
                   sudoku
                   tetris
                   chess
                   2048-game

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
                   wallpaper
                   xkb

                   ;; Media
                   emms
                   emms-setup))
    (require package))

(load-theme 'dracula t t)

(dump-emacs-portable (locate-user-emacs-file "emacs.pdmp"))

;;; pdumper.el ends here

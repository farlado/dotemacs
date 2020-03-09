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
                   dracula-theme
                   mood-line
                   page-break-lines
                   display-line-numbers
                   rainbow-mode
                   rainbow-delimiters
                   dashboard

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
                   flycheck-package
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

(require 'package)
(package-initialize)

(add-to-list 'load-path (locate-user-emacs-file "lisp/xkb"))

(setq pdumper-load-path load-path
      pdumper-dumped t)

(dolist (package `(;; Core
                   async
                   use-package
                   auto-package-update
                   server

                   ;; Looks
                   leuven-theme
                   spaceline
                   spaceline-config
                   diminish
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

                   ;; Media
                   emms
                   emms-setup

                   ;; Desktop Environment
                   exwm
                   exwm-randr
                   exwm-config
                   exwm-systemtray
                   dmenu
                   minibuffer-line
                   system-packages
                   desktop-environment
                   xkb))
    (require package))

(load-theme 'leuven t t)

(dump-emacs-portable (locate-user-emacs-file "emacs.pdmp"))

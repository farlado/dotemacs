;;; pdumper.el --- Making a portable dump image

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



;;; Commentary:

;; This file has been automatically tangled from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.



;;; Code:

(require 'package)
(package-initialize)

(setq pdumper-load-path load-path
      pdumper-dumped t)

(dolist (feature `(;; Core
                   use-package
                   async
                   auto-package-update
                   try

                   ;; Looks
                   dracula-theme
                   mood-line
                   dashboard
                   page-break-lines
                   display-line-numbers
                   paren
                   rainbow-mode
                   rainbow-delimiters

                   ;; Functionality
                   server
                   which-key
                   company
                   counsel
                   company-emoji
                   ibuffer
                   buffer-move
                   sudo-edit

                   ;; Editing
                   markdown-mode
                   graphviz-dot-mode
                   flyspell
                   swiper
                   autorevert
                   popup-kill-ring
                   hungry-delete
                   avy
                   elec-pair

                   ;; Programming
                   haskell-mode
                   lisp-mode
                   company-jedi
                   flycheck
                   flycheck-package
                   flycheck-posframe
                   avy-flycheck

                   ;; org
                   org
                   toc-org
                   org-bullets
                   epresent
                   org-tempo

                   ;; Other
                   nov
                   wdired
                   term
                   wttrin
                   emms
                   emms-setup

                   ;; games
                   yahtzee
                   sudoku
                   tetris
                   chess
                   2048-game

                   ;; Desktop Environment
                   exwm
                   exwm-xim
                   exwm-randr
                   exwm-config
                   exwm-systemtray
                   minibuffer-line
                   system-packages
                   desktop-environment
                   wallpaper))
    (require feature))

(load-theme 'dracula t t)

(dump-emacs-portable (locate-user-emacs-file "emacs.pdmp"))



;;; pdumper.el ends here

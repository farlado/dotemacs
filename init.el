;; Hide dumb things immediately
(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)

;; Determine whether to run Emacs as a desktop environment
(defvar run-emacs-as-desktop-environment?
  (not (= (shell-command "wmctrl -m  1> /dev/null 2> /dev/null") 0))
  "Determine whether or not to load Emacs as a desktop environment.")

;; Package manager
(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Most of the actual config is in here
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (vterm system-packages avy-flycheck 2048-game chess epresent graphviz-dot-mode sudoku wttrin desktop-environment sudo-edit buffer-move emms haskell-mode company-jedi pretty-mode flycheck leuven-theme magit org-bullets avy smex swiper auto-package-update nov use-package dmenu exwm company diminish popup-kill-ring spaceline nov dashboard rainbow-delimiters hungry-delete smart-hungry-delete rainbow-mode ido-vertical-mode which-key))))

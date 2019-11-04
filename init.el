;; get window fullscreen asap lol
(set-frame-parameter nil 'fullscreen 'fullboth)

;; package manager init
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; most of the actual config is in here
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Automatically created, don't touch these!
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "Iosevka")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sudo-edit buffer-move wttrin haskell-mode company-jedi pretty-mode flycheck leuven-theme magit fancy-battery org-bullets auto-package-update nov swiper use-package company emms diminish popup-kill-ring dmenu exwm spaceline dash dashboard rainbow-delimiters hungry-delete smart-hungry-delete rainbow-mode avy smex ido-vertical-mode which-key))))

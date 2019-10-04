;; package manager init
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; install use-package and theme if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; most of the actual config is in herw
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Automatically created, don't touch these!
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "Input Mono Compressed")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dracula-theme auto-package-update nov swiper use-package company emms diminish popup-kill-ring exwm-randr dmenu exwm spaceline dash dashboard rainbow-delimiters hungry-delete smart-hungry-delete switch-window rainbow-mode avy smex ido-vertical-mode which-key))))

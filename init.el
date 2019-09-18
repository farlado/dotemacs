;; package manager init
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; install use-package and theme if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'danneskjold-theme)
  (package-refresh-contents)
  (package-install 'danneskjold-theme))

;; most of the actual config is in here
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; automatically added: don't touch this!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (danneskjold)))
 '(custom-safe-themes
   (quote
    ("445e30faa31222f339df0a313675e79b37fc33603cc65279e1704893e19b4f37" default)))
 '(package-selected-packages (quote (danneskjold-theme which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "Input Mono Compressed")))))

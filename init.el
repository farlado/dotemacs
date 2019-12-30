;; Hide dumb stuff ASAP (kept in init.el for speed)
(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)

;;;;;;;;;;;;;;;;;;;;; These make Emacs load faster ;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist
  "Temporary storage for `file-name-handler-alist' during startup.")
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  "Revert `file-name-handler-alist' to its default value after startup."
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  "Return garbage collection to normal parameters after startup."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;; Package management ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
;; `package-initialize' doesn't have to be called as of Emacs 27
(when (< emacs-major-version 27)
  (package-initialize))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; The rest of the config is in an org-file
(org-babel-load-file (concat user-emacs-directory "config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode vterm system-packages avy-flycheck 2048-game chess epresent graphviz-dot-mode sudoku wttrin desktop-environment sudo-edit buffer-move emms haskell-mode company-jedi pretty-mode flycheck leuven-theme magit org-bullets avy smex swiper auto-package-update nov use-package dmenu exwm company diminish popup-kill-ring spaceline nov dashboard rainbow-delimiters hungry-delete smart-hungry-delete rainbow-mode ido-vertical-mode which-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

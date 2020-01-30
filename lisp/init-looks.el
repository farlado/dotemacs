;;; init-looks.el --- Making Emacs less ugly

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(defun farl-init/set-font ()
  "Set the font at startup."
  (when (member "Iosevka" (font-family-list))
    (set-face-attribute 'default nil :font "Iosevka" :height 100))
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)))

(use-package dracula-theme
  :ensure t
  :defer t
  :init
  (unless pdumper-dumped
    (load-theme 'dracula t t)))

(defun farl-init/fringes-theme ()
  "Make fringes match the color theme."
  (set-face-background 'fringe (face-attribute 'default :background))
  (fringe-mode 10))

(defun farl-init/window-divider-theme ()
  "Make window dividers match the theme."
  (setq window-divider-default-right-width 3)
  (let ((color (face-attribute 'mode-line :background)))
    (set-face-foreground 'window-divider-first-pixel color)
    (set-face-foreground 'window-divider-last-pixel color)
    (set-face-foreground 'window-divider color))
  (window-divider-mode 1))

(defun farl-init/line-numbers-theme ()
  "Make line numbers match the theme."
  (set-face-background 'line-number (face-attribute 'default :background)))

(defun farl-init/transparency ()
  "Apply transparency to the frame."
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'alpha 90))
  (add-to-list 'default-frame-alist '(alpha . 90)))

(defun farl-init/org-theme ()
  "Customize elements in `org-mode'."
  ;; Load `org-mode' if it isn't dumped
  (unless pdumper-dumped
    (require 'org))
  ;; Title
  (set-face-attribute 'org-document-title nil
                      :weight 'extra-bold :height 1.8)
  ;; Headers
  (set-face-attribute 'org-level-1 nil :height 1.3)
  (set-face-attribute 'org-level-2 nil :height 1.1)
  (set-face-attribute 'org-level-3 nil :height 1.0))

(defun farl-init/theme ()
  "Enable theme at startup, providing customizations for consistency."
  (enable-theme 'dracula)
  (when window-system
    (farl-init/fringes-theme)
    (farl-init/line-numbers-theme)
    (farl-init/window-divider-theme)
    (farl-init/transparency)
    (farl-init/org-theme)
    (global-rainbow-mode 1)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package mood-line
  :ensure t
  :defer t
  :init
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (mood-line-mode 1))

(setq display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

(line-number-mode 1)
(column-number-mode 1)

(global-page-break-lines-mode 1)

(global-display-line-numbers-mode 1)
(setq-default indicate-empty-lines t)

(dolist (hook '(Man-mode-hook
                nov-mode-hook
                help-mode-hook
                shell-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                snake-mode-hook
                tetris-mode-hook
                sudoku-mode-hook
                custom-mode-hook
                ibuffer-mode-hook
                epresent-mode-hook
                dashboard-mode-hook
                package-menu-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(defun farl-init/show-paren ()
  "Enable and customize `show-paren-mode'."
  (show-paren-mode 1)
  (set-face-attribute 'show-paren-match nil
                      :weight 'extra-bold
                      :underline t)
  (setq show-paren-style 'parentheses
        show-paren-delay 0))

(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (define-globalized-minor-mode global-rainbow-mode rainbow-mode rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(provide 'init-looks)

;;; init-looks.el ends here

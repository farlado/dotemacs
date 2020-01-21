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
  (set-face-attribute 'default nil :font "Iosevka"))

(set-face-attribute 'default nil
                    :height (if (<= (display-pixel-width) 1366)
                                100
                              180))

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

)

(use-package dracula-theme
  :ensure t
  :defer t
  :init
  (unless (or pdumper-dumped
	      (not window-system))
    (load-theme 'dracula t t)))

(defun farl-init/fringes-theme ()
  "Make fringes match the color theme."
  (set-face-background 'fringe (face-attribute 'default :background))
  (fringe-mode 10))

(defun farl-init/window-divider-theme ()
  "Make window dividers match the theme."
  (setq window-divider-default-right-width 3)
  (dolist (face '(window-divider-first-pixel
                  window-divider-last-pixel
                  window-divider))
    (set-face-foreground face (face-attribute 'mode-line :background)))
  (window-divider-mode 1))

(defun farl-init/line-numbers-theme ()
  "Make line numbers match the theme."
  (set-face-background 'line-number (face-attribute 'default :background)))

(defun farl-init/transparency ()
  "Apply transparency to the frame."
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'alpha 90))
  (add-to-list 'default-frame-alist '(alpha . 90)))

(defun farl-init/theme ()
  "Enable theme at startup, providing customizations for consistency."
  (enable-theme 'dracula)
  (farl-init/fringes-theme)
  (farl-init/window-divider-theme)
  (farl-init/line-numbers-theme)
  (farl-init/transparency))

(use-package mood-line
  :ensure t
  :defer t
  :init
  (setq mood-line-show-encoding-information t)
  (mood-line-mode 1))

(setq display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

(use-package diminish
  :ensure t
  :defer t
  :init
  (defun diminish-minor-modes ()
    "Diminish the minor modes in the list `minor-modes-to-diminish'."
    (dolist (mode minor-modes-to-diminish)
      (diminish mode)))
  (defvar minor-modes-to-diminish '(eldoc-mode
                                    subword-mode
                                    company-mode
                                    rainbow-mode
                                    flycheck-mode
                                    flyspell-mode
                                    which-key-mode
                                    auto-revert-mode
                                    visual-line-mode
                                    haskell-doc-mode
                                    flyspell-prog-mode
                                    hungry-delete-mode
                                    page-break-lines-mode
                                    desktop-environment-mode
                                    haskell-indentation-mode
                                    interactive-haskell-mode
                                    compilation-shell-minor-mode)
    "Minor modes to diminish using `diminish-minor-modes'.")
  (add-hook 'after-init-hook 'diminish-minor-modes))

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

(show-paren-mode 1)
(setq show-paren-style 'parenthesis
      show-paren-delay 0)

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

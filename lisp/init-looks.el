;;; init-looks.el --- Making Emacs less ugly

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka"))

(let* ((res (if (eq window-system 'x)
                (string-to-number
                 (shell-command-to-string
                  "xrandr | grep \\* | cut -d x -f 1 | sort -n | head -n 1"))
              (/ (display-pixel-width) (display-screens))))
       (size (if (<= res 1366) 100
               180)))
  (set-face-attribute 'default nil :height size))

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

(use-package leuven-theme
  :if window-system
  :ensure t
  :defer t
  :init
  (setq leuven-scale-org-agenda-structure t
        leuven-scale-outline-headlines t)
  (load-theme 'leuven t))

(set-face-background 'fringe (face-attribute 'default :background))
(fringe-mode 10)

(setq window-divider-default-right-width 3)
(dolist (face '(window-divider-first-pixel
                window-divider-last-pixel
                window-divider))
  (set-face-foreground face (face-attribute 'mode-line :background)))
(window-divider-mode 1)

(use-package spaceline
  :ensure t
  :defer t
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'wave
        spaceline-buffer-encoding-abbrev-p nil
        spaceline-buffer-size-p nil
        spaceline-line-column-p t
        column-number-indicator-zero-based nil)
  (if window-system
      (spaceline-emacs-theme)
    (spaceline-spacemacs-theme)))

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
  :if window-system
  :ensure t
  :defer t
  :init
  (define-globalized-minor-mode global-rainbow-mode rainbow-mode rainbow-mode)
  (global-rainbow-mode 1))

(use-package rainbow-delimiters
  :if window-system
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-looks)

;;; init-looks.el ends here

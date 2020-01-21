;;; init-defaults.el --- Making Emacs comfortable

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

(use-package company
  :ensure t
  :defer t
  :init
  (setq company-idle-delay 0.75
        company-minimum-prefix-length 3)
  (global-company-mode 1)
  :bind (:map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("SPC" . company-abort)))

(use-package company-emoji
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-emoji))

(setq confirm-kill-emacs 'yes-or-no-p)

(setq inhibit-compacting-font-caches t)

(setq scroll-margin 0
      auto-window-vscroll nil
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

(global-set-key (kbd "C-c d") 'cd)

(global-visual-line-mode 1)

(setq ring-bell-function 'ignore)

(use-package which-key
  :ensure t
  :defer t
  :init
  (which-key-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package ido-vertical-mode
  :ensure t
  :defer t
  :init
  (unless pdumper-dumped
    (require 'ido-vertical-mode))
  (setq ido-everywhere t
        ido-max-prospects 10
        ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-create-new-buffer 'always
        ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-mode 1)
  (ido-vertical-mode 1)
  (use-package smex
    :ensure t
    :defer t
    :bind (("M-x"    . smex)
           ("<menu>" . smex))))

(setq focus-follows-mouse t
      mouse-autoselect-window t)

(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t)

(defun dashboard-restart ()
  "Restart the dashboard buffer and switch to it."
  (interactive)
  (dashboard-insert-startupify-lists)
  (switch-to-buffer "*dashboard*"))

(global-set-key (kbd "C-c M-d") 'dashboard-restart)

(global-set-key (kbd "C-c b") 'balance-windows)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun kill-this-buffer-and-window ()
  "Kill the current buffer and delete the selected window.

This function has been altered to accommodate `exwm-mode'."
  (interactive)
  (let ((window-to-delete (selected-window))
        (buffer-to-kill (current-buffer))
        (delete-window-hook (lambda () (ignore-errors (delete-window)))))
    (unwind-protect
        (progn
          (add-hook 'kill-buffer-hook delete-window-hook t t)
          (if (kill-buffer (current-buffer))
              ;; If `delete-window' failed before, we repeat
              ;; it to regenerate the error in the echo area.
              (when (eq (selected-window) window-to-delete)
                (delete-window)))))))

(global-set-key (kbd "C-x C-k") 'kill-this-buffer-and-window)

(defun close-buffers-and-windows ()
  "Close every buffer and close all windows, then restart dashboard."
  (interactive)
  (unless (save-some-buffers)
    (when (yes-or-no-p "Really kill all buffers? ")
      (mapc 'kill-buffers (buffer-list))
      (delete-other-windows)
      (dashboard-restart))))

(global-set-key (kbd "C-x C-M-k") 'close-buffers-and-windows)

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(setq initial-scratch-message "")

(use-package buffer-move
  :ensure t
  :defer t
  :init
  (defvar buffer-move-and-windmove-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "w") 'windmove-up)
      (define-key map (kbd "a") 'windmove-left)
      (define-key map (kbd "s") 'windmove-down)
      (define-key map (kbd "d") 'windmove-right)
      (define-key map (kbd "C-w") 'buf-move-up)
      (define-key map (kbd "C-a") 'buf-move-left)
      (define-key map (kbd "C-s") 'buf-move-down)
      (define-key map (kbd "C-d") 'buf-move-right)
      map)
    "A keymap for `buffer-move' and `windmove' functions.")
  (global-set-key (kbd "C-x o") buffer-move-and-windmove-map))

(defun split-and-follow-vertical ()
  "Open a new window vertically."
  (interactive)
  (split-window-below)
  (other-window 1)
  (ibuffer))

(defun split-and-follow-horizontal ()
  "Open a new window horizontally."
  (interactive)
  (split-window-right)
  (other-window 1)
  (ibuffer))

(global-set-key (kbd "C-x 2") 'split-and-follow-vertical)
(global-set-key (kbd "C-x 3") 'split-and-follow-horizontal)

(global-set-key (kbd "C-x b") 'ibuffer)
(global-unset-key (kbd "C-x C-b"))

(defmacro user-emacs-file (file)
  "Find FILE in `user-emacs-directory'."
  (expand-file-name file user-emacs-directory))

(defmacro user-home-file (file)
  "Find FILE in the user's home directory."
  (expand-file-name file (getenv "HOME")))

(defmacro user-config-file (file)
  "Find a FILE in the user's $XDG_CONFIG_HOME"
  (expand-file-name file (getenv "XDG_CONFIG_HOME")))

(when (file-exists-p (user-config-file "literate-sysconfig.org"))
  (defun sys-config-visit ()
    "Open the literate system configuration"
    (interactive)
    (find-file (user-config-file "literate-sysconfig.org")))
  (global-set-key (kbd "C-c C-M-e") 'sys-config-visit))

(when (file-exists-p (user-config-file "literate-dotfiles.org"))
  (defun literate-dotfiles-visit ()
    "Open the literate dotfiles."
    (interactive)
    (find-file (user-config-file "literate-dotfiles.org")))

  (global-set-key (kbd "C-c M-e") 'literate-dotfiles-visit))

(defun config-visit ()
  "Open the configuration file."
  (interactive)
  (find-file (user-emacs-file "literate-emacs.org")))

(global-set-key (kbd "C-c e") 'config-visit)

(provide 'init-defaults)

;;; init-defaults.el ends here

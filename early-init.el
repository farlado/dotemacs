;;; early-init.el --- Early Initialization of Farlado's Illiterate GNU Emacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (getenv "_RUN_EXWM")
  (set-face-background 'default "#282a36"))

(defvar pdumper-dumped nil
  "Non-nil if a custom dump image was loaded.")

(defvar pdumper-load-path nil
  "Contains `load-path' if a custom dump image was loaded.")

(defun pdumper-require (feature &optional filename noerror)
  "Call `require' to load FEATURE if `pdumper-dumped' is nil.

FILENAME and NOERROR are also passed to `require'."
  (unless pdumper-dumped
    (require feature filename noerror)))

(defun pdumper-fix-scratch-buffer ()
  "Ensure the scratch buffer is properly loaded."
  (with-current-buffer "*scratch*"
    (lisp-interaction-mode)))

(when pdumper-dumped
  (add-hook 'after-init-hook #'pdumper-fix-scratch-buffer)
  (setq load-path pdumper-load-path)
  (global-font-lock-mode 1)
  (transient-mark-mode 1)
  (blink-cursor-mode 1))

(defun farl-init/compile-user-emacs-directory ()
  "Recompile all files in `user-emacs-directory'."
  (byte-recompile-directory user-emacs-directory 0))

(unless (file-exists-p (expand-file-name "init.elc" user-emacs-directory))
  (add-hook 'after-init-hook #'farl-init/compile-user-emacs-directory))

(setq load-prefer-newer t)

(setq-default apropos-do-all t)

(defvar startup/file-name-handler-alist file-name-handler-alist
  "Temporary storage for `file-name-handler-alist' during startup.")

(defun startup/revert-file-name-handler-alist ()
  "Revert `file-name-handler-alist' to its default value after startup."
  (setq file-name-handler-alist startup/file-name-handler-alist))

(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook #'startup/revert-file-name-handler-alist)

(defun garbage-collect-defer ()
  "Defer garbage collection."
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))

(defun garbage-collect-restore ()
  "Return garbage collection to normal parameters."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(garbage-collect-defer)
(add-hook 'emacs-startup-hook #'garbage-collect-restore)

(setq custom-file "/dev/null"
      package-selected-packages '(;; Core
                                  async
                                  use-package
                                  auto-package-update

                                  ;; Looks
                                  dashboard
                                  dracula-theme
                                  mood-line
                                  rainbow-mode
                                  rainbow-delimiters

                                  ;; Functionality
                                  which-key
                                  company
                                  company-emoji
                                  counsel
                                  buffer-move
                                  sudo-edit

                                  ;; Text Editing
                                  graphviz-dot-mode
                                  markdown-mode
                                  swiper
                                  popup-kill-ring
                                  hungry-delete
                                  avy

                                  ;; Programming
                                  magit
                                  haskell-mode
                                  highlight-indent-guides
                                  company-jedi
                                  flycheck
                                  flycheck-posframe
                                  avy-flycheck

                                  ;; `org-mode'
                                  toc-org
                                  org-bullets
                                  epresent

                                  ;; Extend
                                  vterm
                                  nov
                                  wttrin

                                  ;; Games
                                  yahtzee
                                  sudoku
                                  chess
                                  2048-game

                                  ;; Desktop Environment
                                  exwm
                                  ivy-posframe
                                  dmenu
                                  minibuffer-line
                                  system-packages
                                  desktop-environment

                                  ;; Other
                                  emms))

(require 'package)
(defun package--save-selected-packages (&rest opt) nil)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;;; early-init.el ends here

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

(when pdumper-dumped
  (setq load-path pdumper-load-path)
  (global-font-lock-mode)
  (transient-mark-mode)
  (blink-cursor-mode)
  (add-hook 'after-init-hook
            (lambda ()
              (save-excursion
                (switch-to-buffer "*scratch*")
                (lisp-interaction-mode)))))

(unless (file-exists-p (expand-file-name "init.elc" user-emacs-directory))
  (add-hook 'after-init-hook
            (lambda ()
              (byte-recompile-directory user-emacs-directory t))))

(setq load-prefer-newer t)

(setq-default apropos-do-all t)

(defvar startup/file-name-handler-alist file-name-handler-alist
  "Temporary storage for `file-name-handler-alist' during startup.")

(defun startup/revert-file-name-handler-alist ()
  "Revert `file-name-handler-alist' to its default value after startup."
  (setq file-name-handler-alist startup/file-name-handler-alist))

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun startup/reset-gc ()
  "Return garbage collection to normal parameters after startup."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/reset-gc)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'early-package)

;;; early-init.el ends here

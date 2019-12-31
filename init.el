;;;;;;;;;;; Hide dumb stuff ASAP (kept in init.el for speed) ;;;;;;;;;;;
(dolist (mode '(scroll-bar-mode menu-bar-mode
                tool-bar-mode tooltip-mode))
  (if (fboundp mode)
      (funcall mode -1)))
(setq use-dialog-box nil
      use-file-dialog nil)
(setq-default indicate-empty-lines t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; I don't use `customize' at all ;;;;;;;;;;;;;;;;;;;;
(setq custom-file "/dev/null")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defun package--save-selected-packages (&rest opt) nil)
(setq package-enable-at-startup nil
      package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
;; `package-initialize' doesn't have to be called as of Emacs 27
(when (< emacs-major-version 27)
  (package-initialize))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; Bootstrap async and use-package ;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'async)
  (package-refresh-contents)
  (package-install 'async))
(dired-async-mode 1)
(async-bytecomp-package-mode 1)
(setq async-bytecomp-allowed-packages '(all))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; The rest of the config is in an org-file ;;;;;;;;;;;;;;;
(org-babel-load-file (concat user-emacs-directory "config.org"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

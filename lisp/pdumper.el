;;; pdumper.el --- Starting Emacs FAST

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(require 'package)
(package-initialize)

(setq pdumper-load-path load-path
      pdumper-dumped t)

(dolist (package `(;; Packages that don't cause problems
                   ,@(remove
                      'vterm
                      package-selected-packages)

                   ;; Core
                   server

                   ;; Functionality
                   flyspell

                   ;; `org-mode'
                   org
                   org-tempo

                   ;; Desktop environment
                   exwm-randr
                   exwm-config
                   exwm-systemtray

                   ;; Media player
                   emms-setup))
  (require package))

(load-theme 'dracula t t)

(dump-emacs-portable (expand-file-name "emacs.pdmp" user-emacs-directory))

;;; pdumper.el ends here

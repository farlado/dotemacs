;;; init-package.el --- Package management cont'd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(unless (package-installed-p 'async)
  (package-refresh-contents)
  (package-install 'async))

(dired-async-mode 1)
(async-bytecomp-package-mode 1)
(setq async-bytecomp-allowed-packages '(all))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-compute-statistics t)

(use-package auto-package-update
  :ensure t
  :defer t
  :init
  (setq auto-package-update-interval 2
        auto-package-update-hide-results t
        auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(provide 'init-package)

;;; init-package.el ends here

;;; init-editor.el --- Making Emacs a good text editor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :init
  (require 'graphviz-dot-mode))

(use-package markdown-mode
  :ensure t
  :defer t)

(defun tangle-literate-program ()
  "Tangle a file if it's a literate programming file."
  (interactive)
  (when (and (equal major-mode 'org-mode)
             (string-match-p "literate" (buffer-file-name)))
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-literate-program)

(when (executable-find "aspell")
  (require 'flyspell)

  (setq ispell-program-name "aspell"
        ispell-dictionary "american")

  (add-hook 'flyspell-mode-hook 'flyspell-buffer)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package swiper
  :ensure t
  :defer t
  :bind ("C-s" . swiper))

(setq backup-inhibited t
      make-backup-files nil
      auto-save-default nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-remote-files t
      auto-revert-verbose nil)

(setq require-final-newline t)
(setq-default indent-tabs-mode nil)

(use-package popup-kill-ring
  :ensure t
  :defer t
  :bind ("M-y" . popup-kill-ring)
  :init
  (setq save-interprogram-paste-before-kill t
        mouse-drag-copy-region t
        mouse-yank-at-point t))

(delete-selection-mode 1)

(use-package hungry-delete
  :ensure t
  :defer t
  :init
  (global-hungry-delete-mode 1))

(use-package avy
  :ensure t
  :defer t
  :bind ("M-s" . avy-goto-char))

(global-subword-mode 1)

(setq electric-pair-pairs '((?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")))
(electric-pair-mode 1)
(minibuffer-electric-default-mode 1)

(defun whole-kill-word ()
  "Delete an entire word."
  (interactive)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "C-c DEL") 'whole-kill-word)

(use-package sudo-edit
  :ensure t
  :defer t
  :bind ("C-x C-M-f" . sudo-edit))

(setq inferior-lisp-program "sbcl")

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (setq haskell-stylish-on-save t)
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . turn-on-haskell-doc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template)))

(use-package company-jedi
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-jedi))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode 1))

(use-package avy-flycheck
  :ensure t
  :defer t
  :bind (:map prog-mode-map
         ("C-c C-'" . avy-flycheck-goto-error)))

(use-package toc-org
  :ensure t
  :defer t
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

(use-package org-bullets
  :if window-system
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode))

(use-package epresent
  :if window-system
  :ensure t
  :defer t
  :bind (:map org-mode-map
         ("C-c r" . epresent-run)))

(setq org-pretty-entities t
      org-src-fontify-natively t
      org-agenda-use-time-grid nil
      org-fontify-done-headline t
      org-src-tab-acts-natively t
      org-enforce-todo-dependencies t
      org-fontify-whole-heading-line t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-fontify-quote-and-verse-blocks t
      org-src-window-setup 'current-window
      org-highlight-latex-and-related '(latex)
      org-ellipsis (if window-system "⤵" "...")
      org-hide-emphasis-markers (when window-system t))

(org-babel-do-load-languages 'org-babel-load-languages '((dot . t)))
(setq org-confirm-babel-evaluate '(lambda (lang body) (not (string= lang "dot"))))

(require 'org-tempo)
(add-to-list 'org-modules 'org-tempo)
(setq org-structure-template-alist '(;; General blocks
                                     ("c" . "center")
                                     ("C" . "comment")
                                     ("e" . "example")
                                     ("q" . "quote")
                                     ("v" . "verse")

                                     ;; Export blocks
                                     ("a"   . "export ascii")
                                     ("h"   . "export html")
                                     ("css" . "export css")
                                     ("l"   . "export latex")

                                     ;; Code blocks
                                     ("s"   . "src")
                                     ("el"  . "src emacs-lisp")
                                     ("py"  . "src python")
                                     ("dot" . "src dot :cmdline -Kdot -Tpng :file")
                                     ("txt" . "src text :tangle"))
      org-tempo-keywords-alist '(;; Title/subtitle
                                 ("t"  . "title")
                                 ("st" . "subtitle")

                                 ;; Name/caption
                                 ("n"  . "name")
                                 ("ca" . "caption")

                                 ;; Property/startup
                                 ("p"  . "property")
                                 ("su" . "startup")

                                 ;; Other
                                 ("L" . "latex")
                                 ("H" . "html")
                                 ("A" . "ascii")
                                 ("i" . "index")))

(defun farl-org/disable-angle-bracket-syntax ()
  "Disable the angle bracket syntax added to `org-mode' in versions 9.2 and above."
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?> "."))
(add-hook 'org-mode-hook 'farl-org/disable-angle-bracket-syntax)

(when (file-exists-p "~/agenda.org")
  (setq org-agenda-files '("~/agenda.org"))

  (defun open-agenda ()
    "Open the agenda file."
    (interactive)
    (find-file "~/agenda.org"))

  (global-set-key (kbd "C-c M-a") 'org-agenda)
  (global-set-key (kbd "C-c s-a") 'open-agenda))

(setq org-src-window-setup 'current-window)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(provide 'init-editor)

;;; init-editor.el ends here

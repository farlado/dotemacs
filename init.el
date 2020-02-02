;;; init.el --- Initializing Farlado's Illiterate GNU Emacs

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

(unless pdumper-dumped
  (require 'use-package))
(setq use-package-compute-statistics t)

(use-package auto-package-update
  :ensure t
  :defer t
  :init
  (setq auto-package-update-interval 2
        auto-package-update-hide-results t
        auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(unless pdumper-dumped
  (require 'server))

(defun server-start-if-not-running ()
  "Call `server-start' if `server-running-p' returns nil."
  (interactive)
  (unless (server-running-p)
    (server-start)))

(add-hook 'after-init-hook 'server-start-if-not-running)

(tooltip-mode -1)
(setq use-dialog-box nil
      use-file-dialog nil)

(use-package dashboard
  :ensure t
  :defer t
  :init
  (setq dashboard-set-footer nil
        inhibit-startup-screen t
        dashboard-items '((recents . 10))
        dashboard-startup-banner 'logo
        initial-buffer-choice (lambda () (or (get-buffer "*dashboard*")
                                             (get-buffer "*scratch*")))
        dashboard-banner-logo-title "Welcome to Farlado's Illiterate GNU Emacs!")
  (dashboard-setup-startup-hook))

(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka" :height 100))

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

(use-package dracula-theme
  :ensure t
  :defer t
  :init
  (if pdumper-dumped
      (enable-theme 'dracula)
    (load-theme 'dracula t))
  (set-face-background 'fringe (face-attribute 'default :background))
  (fringe-mode 10)
  (setq window-divider-default-right-width 3)
  (let ((color (face-attribute 'mode-line :background)))
    (set-face-foreground 'window-divider-first-pixel color)
    (set-face-foreground 'window-divider-last-pixel color)
    (set-face-foreground 'window-divider color))
  (window-divider-mode 1)
  (set-face-background 'line-number (face-attribute 'default :background))
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'alpha 90))
  (add-to-list 'default-frame-alist '(alpha . 90))
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

(use-package mood-line
  :ensure t
  :defer t
  :init
  (mood-line-mode 1)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (setq display-time-24hr-format t)
  (display-time-mode 1)
  (display-battery-mode 1)
  (line-number-mode 1)
  (column-number-mode 1))

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
(set-face-attribute 'show-paren-match nil
                    :weight 'extra-bold
                    :underline t)
(setq show-paren-style 'parentheses
      show-paren-delay 0)

(use-package rainbow-mode
  :if window-system
  :ensure t
  :defer t
  :init
  (define-globalized-minor-mode global-rainbow-mode rainbow-mode rainbow-mode)
  :hook (after-init . global-rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

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

(setq make-pointer-invisible nil)

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

(unless pdumper-dumped
  (require 'ido))

(setq ido-everywhere t
      ido-max-prospects 10
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-create-new-buffer 'always)

(define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match)

(ido-mode 1)

(use-package smex
  :ensure t
  :defer t
  :bind (("M-x"    . smex)
         ("<menu>" . smex)))

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

(defun buffer-file-match (string)
  "Find STRING in `buffer-file-name'."
  (string-match-p string (buffer-file-name)))

(defmacro user-emacs-file (file)
  "Find FILE in `user-emacs-directory'."
  (expand-file-name file user-emacs-directory))

(defmacro user-home-file (file)
  "Find FILE in the user's home directory."
  (expand-file-name file (getenv "HOME")))

(defmacro user-config-file (file)
  "Find a FILE in the user's $XDG_CONFIG_HOME"
  (expand-file-name file (getenv "XDG_CONFIG_HOME")))

(when (file-exists-p (user-config-file "dotfiles/literate-sysconfig.org"))
  (defun sys-config-visit ()
    "Open the literate system configuration"
    (interactive)
    (find-file (user-config-file "dotfiles/literate-sysconfig.org")))

  (global-set-key (kbd "C-c C-M-e") 'sys-config-visit))

(when (file-exists-p (user-config-file "dotfiles/literate-dotfiles.org"))
  (defun literate-dotfiles-visit ()
    "Open the literate dotfiles."
    (interactive)
    (find-file (user-config-file "dotfiles/literate-dotfiles.org")))

  (global-set-key (kbd "C-c M-e") 'literate-dotfiles-visit))

(defun config-visit ()
  "Open the configuration file."
  (interactive)
  (find-file (user-emacs-file "literate-emacs.org")))

(global-set-key (kbd "C-c e") 'config-visit)

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :init
  (unless pdumper-dumped
    (require 'graphviz-dot-mode)))

(use-package markdown-mode
  :ensure t
  :defer t)

(defun tangle-literate-program ()
  "Tangle a file if it's a literate programming file."
  (interactive)
  (when (and (equal major-mode 'org-mode)
             (buffer-file-match "literate"))
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-literate-program -100)

(defun byte-compile-config-files ()
  "Byte-compile Emacs configuration files."
  (when (string-match-p "literate-emacs.org" (buffer-file-name))
    (byte-recompile-directory user-emacs-directory 0)))

(add-hook 'after-save-hook 'byte-compile-config-files 100)

(when (executable-find "aspell")
  (unless pdumper-dumped
    (require 'flyspell))

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
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template)))

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :init
  (setq highlight-indent-guides-method 'character)
  :hook (prog-mode . highlight-indent-guides-mode))

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
      org-hide-emphasis-markers window-system)

(org-babel-do-load-languages 'org-babel-load-languages '((dot . t)))

(setq org-confirm-babel-evaluate (lambda (lang body)
                                   (not (or (string= lang "dot")
                                            (buffer-file-match "literate.*.org$")))))

(unless pdumper-dumped
  (require 'org-tempo))
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
                                     ("sh"  . "src sh")
                                     ("cf"  . "src conf")
                                     ("cu"  . "src conf-unix")
                                     ("cs"  . "src conf-space")
                                     ("cx"  . "src conf-xdefaults")
                                     ("el"  . "src emacs-lisp")
                                     ("py"  . "src python")
                                     ("dot" . "src dot :cmdline -Kdot -Tpng :file")
                                     ("txt" . "src text :tangle"))
      org-tempo-keywords-alist '(;; Title/subtitle/author
                                 ("t"  . "title")
                                 ("st" . "subtitle")
                                 ("au" . "author")

                                 ;; Language
                                 ("la" . "language")

                                 ;; Name/caption
                                 ("n"  . "name")
                                 ("ca" . "caption")

                                 ;; Property/options/startup
                                 ("p"  . "property")
                                 ("o"  . "options")
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

(use-package vterm
  :ensure t
  :defer t
  :bind ("C-c t" . vterm))

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode))

(use-package wttrin
  :ensure t
  :defer t
  :init
  (setq wttrin-default-cities '("Indianapolis"))
  :bind ("C-c w" . wttrin))

(setq calendar-week-start-day 1)
(global-set-key (kbd "C-c l") 'calendar)

(global-set-key (kbd "C-c c") 'calc)

(global-set-key (kbd "C-h 4 m") 'man)
(global-set-key (kbd "C-h 4 w") 'woman)

(defvar games-map (make-sparse-keymap)
  "A keymap to which games can be added.")

(global-set-key (kbd "C-c g") games-map)

(use-package yahtzee
  :ensure t
  :defer t
  :bind (:map games-map
         ("y" . yahtzee)))

(use-package sudoku
  :ensure t
  :defer t
  :bind (:map games-map
         ("s" . sudoku)))

(use-package tetris
  :ensure t
  :defer t
  :bind (:map games-map
         ("t" . 'tetris)
         :map tetris-mode-map
         ("w" . tetris-move-bottom)
         ("a" . tetris-move-left)
         ("s" . tetris-mode-down)
         ("d" . tetris-move-right)
         ([left] . tetris-rotate-next)
         ([right] . tetris-rotate-prev)
         ([?\t] . tetris-pause-game)
         ("r" . tetris-start-game)
         ("e" . tetris-end-game)))

(use-package chess
  :ensure t
  :defer t
  :bind (:map games-map
         ("c" . chess)))

(use-package 2048-game
  :ensure t
  :defer t
  :bind (:map games-map
         ("2" . 2048-game)))

(use-package exwm
  :if (getenv "_RUN_EXWM")
  :ensure t
  :defer t
  :init
  (setenv "_RUN_EXWM")
  (unless pdumper-dumped
    (require 'exwm)
    (require 'exwm-randr)
    (require 'exwm-config)
    (require 'exwm-systemtray))
  (setq exwm-floating-border-width window-divider-default-right-width
        exwm-floating-border-color (face-attribute 'mode-line :background))
  (defun farl-exwm/name-buffer-after-window-title ()
    "Rename the current `exwm-mode' buffer after the X window's title."
    (exwm-workspace-rename-buffer exwm-title))
  
  (add-hook 'exwm-update-title-hook 'farl-exwm/name-buffer-after-window-title)
  (use-package exwm-edit
    :ensure t
    :defer t
    :init
    (unless pdumper-dumped
      (require 'exwm-edit)))
  (use-package exwm-mff
    :ensure t
    :defer t
    :hook (exwm-init . exwm-mff-mode))
  (use-package dmenu
    :ensure t
    :defer t
    :init
    (setq dmenu-prompt-string "s-x "))
  (setq exwm-workspace-number 10)
  (setq exwm-randr-workspace-monitor-plist '(0 "DP2-2"
                                             1 "DP2-1"
                                             2 "DP2-3"
                                             3 "DP2-2"
                                             4 "DP2-1"
                                             5 "DP2-3"
                                             6 "DP2-2"
                                             7 "DP2-1"
                                             8 "DP2-3"
                                             9 "DP2-2"))
  (setq exwm-manage-configurations '(((string= exwm-class-name "Steam")
                                      floating-mode-line nil
                                      workspace 9)
                                     ((string= exwm-class-name "TelegramDesktop")
                                      workspace 8)
                                     ((string= exwm-class-name "discord")
                                      workspace 7)
                                     ((or (string-match-p "libreoffice" exwm-class-name)
                                          (string= exwm-class-name "MuseScore3")
                                          (string= exwm-class-name "Gimp"))
                                      workspace 6)
                                     ((string= exwm-title "Event Tester")
                                      floating-mode-line nil
                                      floating t)))
  (defvar farl-exwm/workspace-names ["1"
                                     "2"
                                     "3"
                                     "4"
                                     "5"
                                     "6"
                                     "office"
                                     "discord"
                                     "telegram"
                                     "games"]
    "The names assigned to workspaces through `exwm-workspace-index-map'.")
  
  (setq exwm-workspace-index-map (lambda (index) (elt farl-exwm/workspace-names index)))
  (defun farl-exwm/list-workspaces ()
    "List EXWM workspaces."
    (exwm-workspace--update-switch-history)
    (elt exwm-workspace--switch-history
         (exwm-workspace--position exwm-workspace--current)))
  
  (use-package minibuffer-line
    :ensure t
    :defer t
    :init
    (minibuffer-line-mode 1)
    (set-face-attribute 'minibuffer-line nil :inherit 'default)
    (setq minibuffer-line-format '((:eval (farl-exwm/list-workspaces))))
    (add-hook 'exwm-workspace-switch-hook 'minibuffer-line--update))
  (defun get-connected-monitors ()
    "Return a list of the currently connected monitors."
    (split-string
     (shell-command-to-string
      "xrandr | grep ' connected ' | awk '{print $1}'")))
  (defun display-setup-x230 ()
    "Set up the connected monitors on a ThinkPad X230."
    (let ((monitors (get-connected-monitors))
          (possible '("LVDS1"
                      "VGA1")))
      (dolist (monitor possible)
        (if (member monitor monitors)
            (start-process "xrandr" nil "xrandr"
                           "--output" monitor
                           "--mode" "1366x768"
                           "--pos" "0x0")
          (start-process "xrandr" nil "xrandr"
                         "--output" monitor
                         "--off")))))
  (defun display-setup-w541 ()
    "Set up the connected monitors on a ThinkPad W541."
    (let* ((connected-monitors (get-connected-monitors))
           (docked-p (member "DP2-1" connected-monitors))
           (possible-monitors '("eDP1"
                                "VGA1"
                                "DP2-1"
                                "DP2-2"
                                "DP2-3")))
      (dolist (monitor possible-monitors)
        (if (and (member monitor connected-monitors)
                 (not (and docked-p (string= "eDP1" monitor))))
            (progn
              (start-process "xrandr" nil "xrandr"
                             "--output" monitor
                             ;; Any enabled monitor needs a resolution.
                             "--mode" "1920x1080"
                             ;; DP2-1 and DP2-3 are rotated.
                             "--rotate" (if (string= "DP2-1" monitor)
                                            "left"
                                          (if (string= "DP2-3" monitor)
                                              "right"
                                            "normal"))
                             ;; Every enabled monitor needs a position.
                             "--pos" (if (string-match-p "1" monitor)
                                         "0x0"
                                       (if (string= monitor "DP2-2")
                                           "1080x0"
                                         "3000x0")))
              ;; Setting a monitor as primary occurs outside enabling it.
              ;; This is due to how `start-process' takes arguments.
              (when (or (string= "DP2-2" monitor)
                        (string= "eDP1" monitor))
                (start-process "xrandr" nil "xrandr"
                               "--output" monitor
                               "--primary")))
          (start-process "xrandr" nil "xrandr"
                         "--output" monitor
                         "--off")))))
  (defun peripheral-setup ()
    "Configure peripherals I connect to my dock."
    ;; Trackball
    (let ((trackball-id (shell-command-to-string
                         (concat "xinput | grep ELECOM | head -n 1 | sed -r "
                                 "'s/.*id=([0-9]+).*/\\1/' | tr '\\n' ' '"))))
      (start-process-shell-command
       "Trackball Setup" nil (concat "xinput set-prop " trackball-id
                                     "'libinput Button Scrolling Button' 10"))
      (start-process-shell-command
       "Trackball Setup" nil (concat "xinput set-prop " trackball-id
                                     "'libinput Scroll Method Enabled' 0 0 1"))
      (start-process-shell-command
       "Trackball Setup" nil (concat "xinput set-button-map " trackball-id
                                     "1 2 3 4 5 6 7 8 9 2 1 2")))
    ;; Keyboard
    (start-process "Keyboard Setup" nil "setxkbmap"
                   "-option" "ctrl:nocaps"))
  (defun set-wallpaper ()
    "Set the wallpaper."
    (start-process "Wallpaper" nil "feh"
                   "--no-fehbg" "--bg-fill"
                   (user-config-file ".wallpaper.png")))
  (defun display-and-dock-setup ()
    "Configure displays and dock if applicable."
    (interactive)
    (if (member "LVDS1" (get-connected-monitors))
        (display-setup-x230)
      (progn
        (display-setup-w541)
        (peripheral-setup)))
    (set-wallpaper))
  
  (add-hook 'exwm-randr-screen-change-hook 'display-and-dock-setup)
  (defun run-gimp ()
    "Start GIMP."
    (interactive)
    (start-process "GIMP" nil "gimp"))
  (defun run-steam ()
    "Start Steam."
    (interactive)
    (start-process "Steam" nil "steam"))
  (defun run-firefox ()
    "Start Firefox."
    (interactive)
    (start-process "Firefox" nil "firefox"))
  (defun run-discord ()
    "Start Discord."
    (interactive)
    (start-process "Discord" nil "discord"))
  (defun run-telegram ()
    "Start Telegram."
    (interactive)
    (start-process "Telegram" nil "telegram-desktop"))
  (defun run-musescore ()
    "Start MuseScore."
    (interactive)
    (start-process "MuseScore" nil "musescore"))
  (defun run-libreoffice ()
    "Start LibreOffice."
    (interactive)
    (start-process "LibreOffice" nil "libreoffice"))
  (defun run-transmission ()
    "Start Transmission."
    (interactive)
    (start-process "Transmission" nil "transmission-gtk"))
  (use-package system-packages
    :ensure t
    :defer t
    :init
    (when (executable-find "yay")
      (require 'system-packages)
      (add-to-list 'system-packages-supported-package-managers
                   '(yay .
                         ((default-sudo . nil)
                          (install . "yay -S")
                          (search . "yay -Ss")
                          (uninstall . "yay -Rs")
                          (update . "yay -Syu")
                          (clean-cache . "yay -Sc")
                          (log . "car /var/log/pacman.log")
                          (get-info . "yay -Qi")
                          (get-info-remote . "yay -Si")
                          (list-files-provided-by . "yay -Ql")
                          (verify-all-packages . "yay -Qkk")
                          (verify-all-dependencies . "yay -Dk")
                          (remove-orphaned . "yay -Rns $(yay -Qtdq)")
                          (list-installed-packages . "yay -Qe")
                          (list-installed-packages-all . "yay -Q")
                          (list-dependencies-of . "yay -Qi")
                          (noconfirm . "--noconfirm"))))
      (setq system-packages-use-sudo nil
            system-packages-package-manager 'yay))
    (setq system-packages-noconfirm t)
    :bind (("C-c p i" . system-packages-install)
           ("C-c p e" . system-packages-ensure)
           ("C-c p u" . system-packages-update)
           ("C-c p r" . system-packages-uninstall)
           ("C-c p o" . system-packages-remove-orphaned)
           ("C-c p c" . system-packages-clean-cache)
           ("C-c p l" . system-packages-log)
           ("C-c p s" . system-packages-search)
           ("C-c p g" . system-packages-get-info)
           ("C-c p d" . system-packages-list-dependencies-of)
           ("C-c p f" . system-packages-list-files-provided-by)
           ("C-c p p" . system-packages-list-installed-packages)
           ("C-c p f" . system-packages-verify-all-dependencies)
           ("C-c p v" . system-packages-verify-all-packages)))
  (use-package desktop-environment
    :ensure t
    :defer t
    :init
    (desktop-environment-mode 1))
  (setq desktop-environment-brightness-normal-increment "5%+"
        desktop-environment-brightness-normal-decrement "5%-")
  (setq desktop-environment-volume-toggle-command
        (concat "[ \"$(amixer set Master toggle | grep off)\" ] "
                "&& echo Volume is now muted. | tr '\n' ' ' "
                "|| echo Volume is now unmuted. | tr '\n' ' '")
        desktop-environment-volume-toggle-microphone-command
        (concat "[ \"$(amixer set Capture toggle | grep off)\" ] "
                "&& echo Microphone is now muted. | tr '\n' ' ' "
                "|| echo Microphone is now unmuted | tr '\n' ' '"))
  (setq desktop-environment-screenlock-command (concat "i3lock -nmk "
                                                       "--color=000000 "
                                                       "--timecolor=ffffffff "
                                                       "--datecolor=ffffffff "
                                                       "--wrongcolor=ffffffff "
                                                       "--ringcolor=00000000 "
                                                       "--insidecolor=00000000 "
                                                       "--keyhlcolor=00000000 "
                                                       "--bshlcolor=00000000 "
                                                       "--separatorcolor=00000000 "
                                                       "--ringvercolor=00000000 "
                                                       "--insidevercolor=00000000 "
                                                       "--linecolor=00000000 "
                                                       "--ringwrongcolor=00000000 "
                                                       "--insidewrongcolor=00000000 "
                                                       "--timestr=%H:%M "
                                                       "--datestr='%a %d %b' "
                                                       "--time-font=Iosevka "
                                                       "--date-font=Iosevka "
                                                       "--wrong-font=Iosevka "
                                                       "--timesize=128 "
                                                       "--datesize=64 "
                                                       "--wrongsize=32 "
                                                       "--time-align 0 "
                                                       "--date-align 0 "
                                                       "--wrong-align 0 "
                                                       "--indpos=-10:-10 "
                                                       "--timepos=200:125 "
                                                       "--datepos=200:215 "
                                                       "--wrongpos=200:155 "
                                                       "--locktext='' "
                                                       "--lockfailedtext='' "
                                                       "--noinputtext='' "
                                                       " --veriftext='' "
                                                       "--wrongtext='WRONG' "
                                                       "--force-clock "
                                                       "--radius 1 "
                                                       "--ring-width 1 "))
  ;; Storing to clipboard
  (define-key desktop-environment-mode-map (kbd "<print>")
    'farl-de/desktop-environment-screenshot-part-clip)
  (define-key desktop-environment-mode-map (kbd "<S-print>")
    'farl-de/desktop-environment-screenshot-clip)
  
  ;; Storing to file
  (define-key desktop-environment-mode-map (kbd "<C-print>")
    'farl-de/desktop-environment-screenshot-part)
  (define-key desktop-environment-mode-map (kbd "<C-S-print>")
    'farl-de/desktop-environment-screenshot)
  (setq desktop-environment-screenshot-directory "~/screenshots")
  (setq desktop-environment-screenshot-command
        "FILENAME=$(date +'%Y-%m-%d-%H:%M:%S').png && maim $FILENAME"
        desktop-environment-screenshot-partial-command
        "FILENAME=$(date +'%Y-%m-%d-%H:%M:%S').png && maim -s $FILENAME")
  (defun farl-de/desktop-environment-screenshot ()
    "Take a screenshot and store it in a file."
    (interactive)
    (desktop-environment-screenshot)
    (message "Screenshot saved in ~/screenshots."))
  
  (defun farl-de/desktop-environment-screenshot-part ()
    "Take a capture of a portion of the screen and store it in a file."
    (interactive)
    (desktop-environment-screenshot-part)
    (message "Screenshot saved in ~/screenshots."))
  
  (defun farl-de/desktop-environment-screenshot-clip ()
    "Take a screenshot and put it in the clipboard."
    (interactive)
    (shell-command
     (concat desktop-environment-screenshot-command
             " && xclip $FILENAME -selection clipboard "
             "-t image/png &> /dev/null && rm $FILENAME"))
    (message "Screenshot copied to clipboard."))
  
  (defun farl-de/desktop-environment-screenshot-part-clip ()
    "Take a shot of a portion of the screen and put it in the clipboard."
    (interactive)
    (shell-command
     (concat desktop-environment-screenshot-partial-command
             " && xclip $FILENAME -selection clipboard "
             "-t image/png &> /dev/null && rm $FILENAME"))
    (message "Screenshot copied to clipboard."))
  (defun monitor-settings ()
    "Open arandr to configure monitors."
    (interactive)
    (start-process "Monitor Settings" nil "arandr"))
  (defun network-settings ()
    "Open a NetworkManager connection editor."
    (interactive)
    (start-process "Network Settings" nil "nm-connection-editor")
    (async-shell-command "nmcli dev wifi list" "*Wi-Fi Networks*"))
  (defun volume-settings ()
    "Open pavucontrol to adjust volume."
    (interactive)
    (start-process "Volume Mixer" nil "pavucontrol"))
  (defun audio-loopback ()
    "Loop desktop audio into a null sink alongside the primary input."
    (interactive)
    (dolist (command '(;; Create null sink `loop'
                       "pacmd load-module module-null-sink sink_name=loop"
                       "pacmd update-sink-proplist loop device.description=loop"
                       ;; Create null sink `out'
                       "pacmd load-module module-null-sink sink_name=out"
                       "pacmd update-sink-proplist out device.description=out"
                       ;; Loop `loop' to primary output
                       "pacmd load-module module-loopback source=loop.monitor"
                       ;; Pipe it into `out'
                       "pacmd load-module module-loopback source=loop.monitor sink=out"
                       ;; Loop primary input into `out'
                       "pacmd load-module module-loopback sink=out"))
      (shell-command command))
    ;; Run `pavucontrol' and then unload the modules after it completes
    (start-process-shell-command
     "Audio Loop" nil (concat "pavucontrol;"
                              "pacmd unload-module module-null-sink;"
                              "pacmd unload-module module-loopback")))
  (defvar keyboard-layout-1 "us"
    "The first of three keyboard layouts to cycle through.
  
  Set to nil for one less keyboard layout.")
  
  (defvar keyboard-layout-2 "epo"
    "The second of three keyboard layouts to cycle through.
  
  Set to nil for one less keyboard layout.")
  
  (defvar keyboard-layout-3 "de"
    "The third of three keyboard layouts to cycle through.
  
  Set to nil for one less keyboard layout.")
  (defun get-keyboard-layout ()
    "Get the current keyboard layout."
    (shell-command-to-string
     "setxkbmap -query | grep -oP 'layout:\\s*\\K(\\w+)' | tr '\n' ' ' | sed 's/ //'"))
  
  (defun set-keyboard-layout (&optional layout)
    "Set the keyboard layout to LAYOUT."
    (interactive)
    (let ((layout (or layout (read-string "Enter keyboard layout: "))))
      (start-process "Keyboard layout" nil "setxkbmap"
                     layout "-option" "ctrl:nocaps")
      (message "Keyboard layout is now: %s" layout)))
  
  (defun cycle-keyboard-layout ()
    "Cycle between `keyboard-layout-1', `keyboard-layout-2', and `keyboard-layout-3'."
    (interactive)
    (let* ((current-layout (get-keyboard-layout))
           (new-layout (if (string= current-layout keyboard-layout-1)
                           (or keyboard-layout-2 keyboard-layout-3)
                         (if (string= current-layout keyboard-layout-2)
                             (or keyboard-layout-3 keyboard-layout-1)
                           (or keyboard-layout-1 keyboard-layout-2)))))
      (if new-layout
          (set-keyboard-layout new-layout)
        (message "No keyboard layouts selected."))))
  
  (defun cycle-keyboard-layout-reverse ()
    "Cycle between `keyboard-layout-1', `keyboard-layout-2', and `keyboard-layout-3' in reverse."
    (interactive)
    (let* ((current-layout (get-keyboard-layout))
           (new-layout (if (string= current-layout keyboard-layout-3)
                           (or keyboard-layout-2 keyboard-layout-1)
                         (if (string= current-layout keyboard-layout-2)
                             (or keyboard-layout-1 keyboard-layout-3)
                           (or keyboard-layout-3 keyboard-layout-2)))))
      (if new-layout
          (set-keyboard-layout new-layout)
        (message "No keyboard layouts selected."))))
  (defun suspend-computer ()
    (interactive)
    (and (yes-or-no-p "Really suspend? ")
         (start-process "Suspend" nil "systemctl"
                        "suspend" "-i")))
  
  (global-set-key (kbd "C-x C-M-s") 'suspend-computer)
  (defun save-buffers-reboot (&optional arg)
    "Offer to save each buffer, then shut down the computer.
  This function is literally just a copycat of `save-buffers-kill-emacs'.
  With prefix ARG, silently save all file-visiting buffers without asking.
  If there are active processes where `process-query-on-exit-flag'
  returns non-nil and `confirm-kill-processes' is non-nil,
  asks whether processes should be killed.
  Runs the members of `kill-emacs-query-functions' in turn and stops
  if any returns nil.  If `confirm-kill-emacs' is non-nil, calls it.
  Instead of just killing Emacs, shuts down the system."
    (interactive "P")
    ;; Don't use save-some-buffers-default-predicate, because we want
    ;; to ask about all the buffers before killing Emacs.
    (save-some-buffers arg t)
    (let ((confirm confirm-kill-emacs))
      (and
       (or (not (memq t (mapcar (function
                                 (lambda (buf) (and (buffer-file-name buf)
                                                    (buffer-modified-p buf))))
                                (buffer-list))))
           (progn (setq confirm nil)
                  (yes-or-no-p "Modified buffers exist; reboot anyway? ")))
       (or (not (fboundp 'process-list))
           ;; process-list is not defined on MSDOS.
           (not confirm-kill-processes)
           (let ((processes (process-list))
                 active)
             (while processes
               (and (memq (process-status (car processes)) '(run stop open listen))
                    (process-query-on-exit-flag (car processes))
                    (setq active t))
               (setq processes (cdr processes)))
             (or (not active)
                 (with-current-buffer-window
                  (get-buffer-create "*Process List*") nil
                  #'(lambda (window _value)
                      (with-selected-window window
                        (unwind-protect
                            (progn
                              (setq confirm nil)
                              (yes-or-no-p (concat "Active processes exist; kill "
                                                   "them and reboot anyway? ")))
                          (when (window-live-p window)
                            (quit-restore-window window 'kill)))))
                  (list-processes t)))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm)
           (funcall confirm "Really reboot? "))
       (shell-command "reboot")
       (kill-emacs))))
  
  (global-set-key (kbd "C-x C-M-r") 'save-buffers-reboot)
  (defun save-buffers-shut-down (&optional arg)
    "Offer to save each buffer, then shut down the computer.
  This function is literally just a copycat of `save-buffers-kill-emacs'.
  With prefix ARG, silently save all file-visiting buffers without asking.
  If there are active processes where `process-query-on-exit-flag'
  returns non-nil and `confirm-kill-processes' is non-nil,
  asks whether processes should be killed.
  Runs the members of `kill-emacs-query-functions' in turn and stops
  if any returns nil.  If `confirm-kill-emacs' is non-nil, calls it.
  Instead of just killing Emacs, shuts down the system."
    (interactive "P")
    ;; Don't use save-some-buffers-default-predicate, because we want
    ;; to ask about all the buffers before killing Emacs.
    (save-some-buffers arg t)
    (let ((confirm confirm-kill-emacs))
      (and
       (or (not (memq t (mapcar (function
                                 (lambda (buf) (and (buffer-file-name buf)
                                                    (buffer-modified-p buf))))
                                (buffer-list))))
           (progn (setq confirm nil)
                  (yes-or-no-p "Modified buffers exist; shut down anyway? ")))
       (or (not (fboundp 'process-list))
           ;; process-list is not defined on MSDOS.
           (not confirm-kill-processes)
           (let ((processes (process-list))
                 active)
             (while processes
               (and (memq (process-status (car processes)) '(run stop open listen))
                    (process-query-on-exit-flag (car processes))
                    (setq active t))
               (setq processes (cdr processes)))
             (or (not active)
                 (with-current-buffer-window
                  (get-buffer-create "*Process List*") nil
                  #'(lambda (window _value)
                      (with-selected-window window
                        (unwind-protect
                            (progn
                              (setq confirm nil)
                              (yes-or-no-p (concat "Active processes exist; kill "
                                                   "them and shut down anyway? ")))
                          (when (window-live-p window)
                            (quit-restore-window window 'kill)))))
                  (list-processes t)))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm)
           (funcall confirm "Really shut down? "))
       (shell-command "shutdown now")
       (kill-emacs))))
  
  (global-set-key (kbd "C-x C-M-c") 'save-buffers-shut-down)
  (setq exwm-input-global-keys `(;; Switching workspace focus
                                 ;; s-1 for 1, s-2 for 2, etc...
                                 ,@(mapcar
                                    (lambda (i)
                                      `(,(kbd (format "s-%d" (% (1+ i) 10))) .
                                        (lambda ()
                                          (interactive)
                                          (exwm-workspace-switch-create ,i))))
                                    (number-sequence 0 9))
  
                                 ;; Switching window to a workspace
                                 ;; This was annoying to get working
                                 ;; s-! for 1, s-@ for 2, etc...
                                 ,@(mapcar
                                    (lambda (i)
                                      `(,(kbd (format "s-%s" (nth i '("!" "@"
                                                                      "#" "$"
                                                                      "%" "^"
                                                                      "&" "*"
                                                                      "(" ")")))) .
                                        (lambda ()
                                          (interactive)
                                          (exwm-workspace-move-window ,i))))
                                    (number-sequence 0 9))
  
                                 ;; Window size adjustment
                                 (,(kbd "C-s-w") . shrink-window)
                                 (,(kbd "C-s-s") . enlarge-window)
                                 (,(kbd "C-s-a") . shrink-window-horizontally)
                                 (,(kbd "C-s-d") . enlarge-window-horizontally)
  
                                 ;; Opening programs
                                 ([XF86Calculator] . calc)
                                 ([s-return]       . vterm)
                                 ([?\s-g]          . run-gimp)
                                 ([?\s-s]          . run-steam)
                                 ([?\s-f]          . run-firefox)
                                 ([?\s-d]          . run-discord)
                                 ([?\s-t]          . run-telegram)
                                 ([?\s-m]          . run-musescore)
                                 ([?\s-b]          . run-libreoffice)
                                 ([?\s-o]          . run-transmission)
                                 ([?\s-r]          . monitor-settings)
                                 ([?\s-n]          . network-settings)
                                 ([?\s-v]          . volume-settings)
  
                                 ;; Other desktop environment things
                                 ([menu]            . smex)
                                 ([?\s-x]           . dmenu)
                                 ([s-tab]           . audio-loopback)
                                 ([?\s- ]           . cycle-keyboard-layout)
                                 ([s-backspace]     . cycle-keyboard-layout-reverse)
                                 ([XF86ScreenSaver] . desktop-environment-lock-screen)
  
                                 ;; Controlling EMMS
                                 ([XF86AudioNext] . emms-next)
                                 ([XF86AudioPrev] . emms-previous)
                                 ([XF86AudioPlay] . emms-pause)
                                 ([XF86AudioStop] . emms-stop)))
  (setq exwm-input-simulation-keys '(;; Navigation
                                     ([?\C-b] . [left])
                                     ([?\C-f] . [right])
                                     ([?\C-p] . [up])
                                     ([?\C-n] . [down])
  
                                     ([?\M-b] . [C-left])
                                     ([?\M-f] . [C-right])
                                     ([?\M-p] . [C-up])
                                     ([?\M-n] . [C-down])
  
                                     ([?\C-a] . [home])
                                     ([?\C-e] . [end])
                                     ([?\C-v] . [next])
                                     ([?\M-v] . [prior])
  
                                     ;; Copy/Paste
                                     ([?\C-w] . [?\C-x])
                                     ([?\M-w] . [?\C-c])
                                     ([?\C-y] . [?\C-v])
                                     ([?\C-s] . [?\C-f])
                                     ([?\C-\/] . [?\C-z])
  
                                     ;; Other
                                     ([?\C-d] . [delete])
                                     ([?\C-k] . [S-end delete])
                                     ([?\C-g] . [escape])))
  
  ;; I can't do sequences above, so these are separate
  (defun farl-exwm/C-s ()
    "Pass C-s to the EXWM window."
    (interactive)
    (execute-kbd-macro (kbd "C-q C-s")))
  
  (defun farl-exwm/C-k ()
    "Pass C-k to the EXWM window."
    (interactive)
    (execute-kbd-macro (kbd "C-q C-k")))
  
  (define-key exwm-mode-map (kbd "C-x C-s") 'farl-exwm/C-s)
  (define-key exwm-mode-map (kbd "C-c C-l") 'farl-exwm/C-k)
  (define-key exwm-mode-map (kbd "C-c C-q") nil)
  (define-key exwm-mode-map (kbd "C-q") 'exwm-input-send-next-key)
  (define-key exwm-mode-map (kbd "C-c C-t C-f") nil)
  (define-key exwm-mode-map (kbd "C-c C-t C-v") nil)
  (define-key exwm-mode-map (kbd "C-c C-t C-m") nil)
  (define-key exwm-mode-map (kbd "C-c C-f") nil)
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (setenv "XDG_CURRENT_DESKTOP" "emacs")
  (setenv "GTK2_RC_FILES" (user-config-file "gtk-2.0/gtkrc"))
  (setenv "QT_QPA_PLATFORMTHEME" "gtk2")
  (setenv "_JAVA_AWT_WM_NONREPARENTING" "1")
  (start-process "Hide Cursor" nil "xbanish")
  (start-process "Disable Blanking" nil "xset"
                 "s" "off" "-dpms")
  (start-process "Trackpad Setup" nil "xinput"
                 "disable" (shell-command-to-string
                            (concat "xinput | grep Synap | head -n 1 | "
                                    "sed -r 's/.*id=([0-9]+).*/\\1/' | "
                                    "tr '\n' ' ' | sed 's/ //'")))
  (start-process "Keyboard Layout" nil "setxkbmap"
                 "us" "-option" "ctrl:nocaps")
  (start-process "Compositor" nil "xcompmgr")
  (start-process "Fallback Cursor" nil "xsetroot"
                 "-cursor_name" "left_ptr")
  (exwm-enable)
  (exwm-config-ido)
  (exwm-randr-enable)
  (exwm-systemtray-enable))

(use-package emms
  :if (executable-find "mpd")
  :ensure t
  :defer t
  :init
  (unless pdumper-dumped
    (require 'emms-setup))
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5
        emms-player-list '(emms-player-mpd)
        emms-info-functions '(emms-info mpd)
        emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6601"
        mpc-host "localhost:6601")
  
  (setenv "MPD_HOST" "localhost")
  (setenv "MPD_PORT" "6601")
  
  (defvar emms-map
    (let ((map (make-sparse-keymap)))
      ;; Opening playlist and music browser
      (define-key map (kbd "v") 'emms)
      (define-key map (kbd "b") 'emms-smart-browse)
      ;; Track navigation
      (define-key map (kbd "n n") 'emms-next)
      (define-key map (kbd "n p") 'emms-previous)
      (define-key map (kbd "p")   'emms-pause)
      (define-key map (kbd "s")   'emms-stop)
      ;; Repeat/shuffle
      (define-key map (kbd "t C-r") 'emms-toggle-repeat-track)
      (define-key map (kbd "t r")   'emms-toggle-repeat-playlist)
      (define-key map (kbd "t s")   'farl-emms/shuffle-with-message)
      ;; Refreshing various things
      (define-key map (kbd "r c") 'emms-player-mpd-update-all-reset-cache)
      (define-key map (kbd "r d") 'mpd/update-database)
      ;; `mpd'-specific functions
      (define-key map (kbd "d s") 'mpd/start-music-daemon)
      (define-key map (kbd "d q") 'mpd/kill-music-daemon)
      (define-key map (kbd "d u") 'mpd/update-database)
      map)
    "A keymap for controlling `emms'.")
  (global-set-key (kbd "C-c a") emms-map))

;;; init.el ends here

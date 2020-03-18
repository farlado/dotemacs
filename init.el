(unless (package-installed-p 'async)
  (package-refresh-contents)
  (package-install 'async))

(dired-async-mode 1)
(async-bytecomp-package-mode 1)
(setq async-bytecomp-allowed-packages '(all))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(pdumper-require 'use-package)
(setq use-package-compute-statistics t)

(use-package auto-package-update
  :ensure t
  :defer t
  :custom ((auto-package-update-interval 2)
           (auto-package-update-hide-results t)
           (auto-package-update-delete-old-versions t))
  :hook (after-init . auto-package-update-maybe))

(pdumper-require 'server)
(unless (server-running-p)
  (server-start))

(tooltip-mode -1)
(setq use-dialog-box nil
      use-file-dialog nil)

(use-package leuven-theme
  :if window-system
  :ensure t
  :defer t
  :init
  (if pdumper-dumped
      (enable-theme 'leuven)
    (load-theme 'leuven t))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (when (member "Iosevka" (font-family-list))
    (set-face-attribute 'default nil
                        :font "Iosevka"
                        :height 100))
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol
                      (font-spec :family "Noto Color Emoji")
                      nil 'prepend))
  (setq inhibit-compacting-font-caches t)
  (set-face-background 'fringe (face-background 'default))
  (fringe-mode 10)
  (set-face-background 'line-number (face-background 'default))
  (setq window-divider-default-right-width 3)
  (let ((color (face-background 'mode-line)))
    (dolist (face '(window-divider-first-pixel
                    window-divider-last-pixel
                    window-divider))
      (set-face-foreground face color)))
  (window-divider-mode 1)
  (pdumper-require 'org)
  (set-face-attribute 'org-level-1 nil
                      :height 1.3)
  (set-face-attribute 'org-level-2 nil
                      :height 1.1)
  (set-face-attribute 'org-level-3 nil
                      :height 1.0)
  (set-face-attribute 'org-document-title nil
                      :weight 'extra-bold
                      :height 1.8))

(use-package mood-line
  :ensure t
  :defer t
  :init
  (mood-line-mode 1)
  (defun mood-line-segment-major-mode ()
    "Displays the curent major mode in the mode-line."
    (propertize "%m " 'face 'mode-line-buffer-id))
  (line-number-mode 1)
  (column-number-mode 1)
  (display-time-mode 1)
  (display-battery-mode 1)
  :custom-face (mood-line-status-info ((t (:inherit mode-line) (:background nil))))
  :custom-face (mood-line-unimportant ((t (:inherit mode-line) (:background nil))))
  :custom-face (mood-line-status-neutral ((t (:inherit mode-line) (:background nil))))
  :custom ((display-time-24hr-format t)
           (display-time-day-and-date t)
           (display-time-format "%a %m/%d %H:%M")))

(global-visual-line-mode 1)

(use-package page-break-lines
  :ensure t
  :defer t
  :hook (after-init . global-page-break-lines-mode))

(use-package display-line-numbers
  :defer t
  :custom (indicate-empty-lines t)
  :hook ((text-mode
          prog-mode
          conf-mode) . display-line-numbers-mode))

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
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
  :ensure t
  :defer t
  :init
  (defun dashboard-immortal ()
    "Make the dashboard buffer immortal."
    (emacs-lock-mode 'kill))
  (defun dashboard-or-scratch ()
    "Open either dashboard or the scratch buffer."
    (or (get-buffer "*dashboard*")
        (get-buffer "*scratch*")))
  (dashboard-setup-startup-hook)
  :custom ((inhibit-start-screen t)
           (dashboard-set-footer nil)
           (dashboard-startup-banner (locate-user-emacs-file "logo.png"))
           (dashboard-items '((recents . 10)))
           (initial-buffer-choice #'dashboard-or-scratch)
           (dashboard-banner-logo-title
            "Welcome to Farlado's Illiterate GNU Emacs!"))
  :hook (dashboard-mode . dashboard-immortal))

(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

(setq disabled-command-function nil)

(add-hook 'minibuffer-setup-hook #'garbage-collect-defer)
(add-hook 'minibuffer-exit-hook #'garbage-collect-restore)

(setq confirm-kill-emacs #'yes-or-no-p)

(setq scroll-margin 0
      auto-window-vscroll nil
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p #'y-or-n-p
  "Use `y-or-n-p' instead of a yes/no prompt.")

(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode))

(use-package company
  :ensure t
  :defer t
  :custom ((company-idle-delay 0.75)
           (company-minimum-prefix-length 3))
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("SPC" . company-abort)))

(use-package company-emoji
  :after company
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends #'company-emoji))

(use-package counsel
  :ensure t
  :defer t
  :init
  (defun farl-init/ivy-mode ()
    "Start `ivy-mode' while disabling `ido-mode'."
    (ivy-mode 1)
    (ido-mode -1)
    (counsel-mode 1)
    (setq ivy-initial-inputs-alist nil))
  :hook (after-init . farl-init/ivy-mode)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c d" . counsel-cd)))

(defun buffer-file-match (string)
  "Find STRING in variable `buffer-file-name'."
  (string-match-p string buffer-file-name))

(defmacro user-home-file (file)
  "Find FILE in the user's home directory."
  (expand-file-name file (getenv "HOME")))

(defmacro user-config-file (file)
  "Find a FILE in the user's $XDG_CONFIG_HOME directory."
  (expand-file-name file (getenv "XDG_CONFIG_HOME")))

(use-package ibuffer
  :defer t
  :init
  (defun farl-ibuffer/use-default-filter-group ()
    "Switch to the intended filter group."
    (ibuffer-switch-to-saved-filter-groups "default"))
  :custom ((ibuffer-saved-filter-groups
            (quote (("default"
                     ("firefox" (name . "Firefox$"))
                     ("exwm" (or (mode . exwm-mode)
                                 (name . "^\\*XELB-DEBUG\\*$")))
                     ("emms" (or (mode . emms-mode)
                                 (mode . emms-browser-mode)
                                 (mode . emms-playlist-mode)))
                     ("vterm" (mode . vterm-mode))
                     ("magit" (name . "^magit.*:"))
                     ("dired" (mode . dired-mode))
                     ("elisp" (mode . emacs-lisp-mode))
                     ("emacs" (or (name . "^\\*dashboard\\*$")
                                  (name . "^\\*scratch\\*$")
                                  (name . "^\\*Messages\\*$")
                                  (name . "^\\*Backtrace\\*$")
                                  (name . "^\\*Compile-Log\\*$")
                                  (name . "^\\*Shell.*Output\\*$")))))))
           (uniquify-buffer-name-style 'forward)
           (uniquify-after-kill-buffer-p t))
  :hook (ibuffer-mode . farl-ibuffer/use-default-filter-group)
  :bind (("C-x b" . ibuffer)
         ("C-x C-b" . nil)))

(use-package buffer-move
  :ensure t
  :defer t
  :init
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
  :bind (("C-x 2" . split-and-follow-vertical)
         ("C-x 3" . split-and-follow-horizontal))
  :custom ((focus-follows-mouse t)
           (mouse-autoselect-window t))
  :bind (("C-x o" . nil)
         ("C-x o w" . windmove-up)
         ("C-x o a" . windmove-left)
         ("C-x o s" . windmove-down)
         ("C-x o d" . windmove-right)
         ("C-x o C-w" . buf-move-up)
         ("C-x o C-a" . buf-move-left)
         ("C-x o C-s" . buf-move-down)
         ("C-x o C-d" . buf-move-right)))

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(setq initial-scratch-message "")

(defun config-visit ()
  "Open the configuration file."
  (interactive)
  (find-file (locate-user-emacs-file "literate-emacs.org")))

(global-set-key (kbd "C-c e") #'config-visit)

(defun literate-dotfiles-visit ()
  "Open the literate dotfiles."
  (interactive)
  (find-file (user-config-file "dotfiles/literate-dotfiles.org")))

(when (file-exists-p (user-config-file "dotfiles/literate-dotfiles.org"))
  (global-set-key (kbd "C-c M-e") #'literate-dotfiles-visit))

(defun sys-config-visit ()
  "Open the literate system configuration"
  (interactive)
  (find-file (user-config-file "dotfiles/literate-sysconfig.org")))

(when (file-exists-p (user-config-file "dotfiles/literate-sysconfig.org"))
  (global-set-key (kbd "C-c C-M-e") #'sys-config-visit))

(global-set-key (kbd "C-c b") #'balance-windows)

(global-set-key (kbd "C-x k") #'kill-this-buffer)

(defun kill-this-buffer-and-window ()
  "Kill the current buffer and delete the selected window.

This function has been altered to accomodate `exwm-mode'."
  (interactive)
  (let ((window-to-delete (selected-window))
        (buffer-to-kill (current-buffer))
        (delete-window-hook (lambda ()
                              (ignore-errors
                                (delete-window)))))
    (unwind-protect
        (progn
          (add-hook 'kill-buffer-hook delete-window-hook t t)
          (if (kill-buffer (current-buffer))
              ;; If `delete-window' failed before, we repeat
              ;; it to regenerate the error in the echo area.
              (when (eq (selected-window) window-to-delete)
                (delete-window)))))))

(global-set-key (kbd "C-x C-k") #'kill-this-buffer-and-window)

(defun close-buffers-and-windows ()
  "Kill every buffer and close all windows, then restart dashboard."
  (interactive)
  (when (yes-or-no-p "Really kill all buffers? ")
    (save-some-buffers)
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows)))

(global-set-key (kbd "C-x C-M-k") #'close-buffers-and-windows)

(use-package sudo-edit
  :ensure t
  :defer t
  :bind ("C-x C-M-f" . sudo-edit))

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :mode ("\\.dot\\'" . graphviz-dot-mode))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

(defun tangle-literate-program ()
  "Tangle a file if it's a literate programming file."
  (interactive)
  (when (buffer-file-match "literate.*.org$")
    (org-babel-tangle)))

(add-hook 'after-save-hook #'tangle-literate-program -100)

(defun byte-compile-config-files ()
  "Byte-compile Emacs configuration files."
  (when (string-match-p "literate-emacs.org" (buffer-file-name))
    (byte-recompile-directory user-emacs-directory 0)))

(add-hook 'after-save-hook #'byte-compile-config-files 100)

(use-package flyspell
  :if (executable-find "aspell")
  :defer t
  :custom ((ispell-program-name "aspell")
           (ispell-dictionary "american"))
  :hook ((flyspell-mode . flyspell-buffer)
         ((prog-mode
           conf-mode) . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

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
  :custom ((save-interprogram-paste-before-kill t)
           (mouse-drag-copy-region t)
           (mouse-yank-at-point t))
  :bind ("M-y" . popup-kill-ring))

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

(setq inferior-lisp-program "sbcl")

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package haskell-mode
  :ensure t
  :defer t
  :custom (haskell-stylish-on-save t)
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template)))

(use-package highlight-indent-guides
  :if window-system
  :ensure t
  :defer t
  :custom (highlight-indent-guides-method 'character)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package company-jedi
  :after company
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-jedi))

(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-package
  :after flycheck
  :ensure t
  :defer t
  :init
  (flycheck-package-setup))

(use-package flycheck-posframe
  :if window-system
  :after flycheck
  :ensure t
  :defer t
  :custom (flycheck-posframe-position 'window-bottom-left-corner)
  :hook ((flycheck-mode . flycheck-posframe-mode)
         (flycheck-posframe-mode . flycheck-posframe-configure-pretty-defaults)))

(use-package avy-flycheck
  :ensure t
  :defer t
  :bind (:map prog-mode-map
         ("C-c C-'" . avy-flycheck-goto-error)))

(use-package org
  :defer t
  :init
  (use-package toc-org
    :ensure t
    :defer t
    :hook ((org-mode . toc-org-mode)
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
  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t)))
  (defun farl-org/confirm-babel-evaluate (lang body)
    "Don't ask to evaluate graphviz blocks or literate programming blocks."
    (not (or (string= lang "dot")
             (buffer-file-match "literate.*.org$"))))
  (use-package org-tempo
    :defer t
    :init
    (add-to-list 'org-modules 'org-tempo)
    :custom ((org-structure-template-alist '(;; General blocks
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
                                             ("cjp" . "src conf-javaprop")
                                             ("el"  . "src emacs-lisp")
                                             ("py"  . "src python")
                                             ("dot" . "src dot :file")
                                             ("txt" . "src text :tangle")))
             (org-tempo-keywords-alist '(;; Title/subtitle/author
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
                                         ("i" . "index")))))
  (defun farl-org/disable-angle-bracket-syntax ()
    "Disable angle bracket syntax."
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?> "."))
  (defun open-agenda-file ()
    "Open the agenda file."
    (interactive)
    (find-file (ivy-read
                "Open agenda: "
                (all-completions "" org-agenda-files))))
  
  (when (file-directory-p "~/agendas")
    (setq org-agenda-files (directory-files-recursively
                            (user-home-file "agendas")
                            ".org$" nil t t)))
  :custom ((org-pretty-entities t)
           (org-src-fontify-natively t)
           (org-agenda-use-time-grid nil)
           (org-fontify-done-headline t)
           (org-src-tab-acts-natively t)
           (org-enforce-todo-dependencies t)
           (org-fontify-whole-heading-line t)
           (org-agenda-skip-deadline-if-done t)
           (org-agenda-skip-scheduled-if-done t)
           (org-fontify-quote-and-verse-blocks t)
           (org-src-window-setup 'current-window)
           (org-highlight-latex-and-related '(latex))
           (org-ellipsis (if window-system "⤵" "..."))
           (org-hide-emphasis-markers window-system)
           (org-confirm-babel-evaluate #'farl-org/confirm-babel-evaluate))
  :hook ((org-mode . farl-org/disable-angle-bracket-syntax)
         (org-babel-after-execute . org-redisplay-inline-images))
  :bind (("C-c s-a" . open-agenda-file)
         ("C-c M-a" . org-agenda)))

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
  :custom (wttrin-default-cities '("Indianapolis"))
  :bind ("C-c w" . wttrin))

(setq calendar-week-start-day 1)
(global-set-key (kbd "C-c l") #'calendar)

(global-set-key (kbd "C-h 4 m") #'man)
(global-set-key (kbd "C-h 4 w") #'woman)

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

(use-package emms
  :if (executable-find "mpd")
  :ensure t
  :defer t
  :init
  (pdumper-require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (defun mpd/start-music-daemon ()
    "Start MPD, connect to it and sync the metadata cache"
    (interactive)
    (shell-command "mpd")
    (mpd/update-database)
    (emms-player-mpd-connect)
    (emms-cache-set-from-mpd-all)
    (message "MPD started!"))
  (defun mpd/kill-music-daemon ()
    "Stop playback and kill the music daemon."
    (interactive)
    (emms-stop)
    (call-process "killall" nil nil nil "mpd")
    (message "MPD killed!"))
  (defun mpd/update-database ()
    "Update the MPD database synchronously."
    (interactive)
    (call-process "mpc" nil nil nil "update")
    (message "MPD database updated!"))
  (defun farl-emms/shuffle-with-message ()
    "Shuffle the playlist and say so in the echo area."
    (interactive)
    (emms-shuffle)
    (message "Playlist has been shuffled."))
  (setenv "MPD_HOST" "localhost")
  (setenv "MPD_PORT" "6601")
  :custom ((emms-seek-seconds 5)
           (emms-player-list '(emms-player-mpd))
           (emms-info-functions '(emms-info mpd))
           (emms-completing-read #'ivy-completing-read)
           (emms-player-mpd-server-name "localhost")
           (emms-player-mpd-server-port "6601"))
  :bind (;; Opening playlist and music browser
         ("C-c a v" . emms)
         ("C-c a b" . emms-smart-browse)
  
         ;; Track navigation
         ("C-c a C-n" . emms-next)
         ("C-c a C-p" . emms-previous)
         ("C-c a p" . emms-pause)
         ("C-c a C-s" . emms-stop)
  
         ;; Repeat/shuffle
         ("C-c a C-r" . emms-toggle-repeat-track)
         ("C-c a r" . emms-toggle-repeat-playlist)
         ("C-c a s" . farl-emms/shuffle-with-message)
  
         ;; Refreshing the emms cache
         ("C-c a c" . emms-player-mpd-update-all-reset-cache)
  
         ;; mpd related functions
         ("C-c a d" . mpd/start-music-daemon)
         ("C-c a q" . mpd/kill-music-daemon)
         ("C-c a u" . mpd/update-database)))

(use-package exwm
  :if (getenv "_RUN_EXWM")
  :ensure t
  :defer t
  :init
  (setenv "_RUN_EXWM")
  (pdumper-require 'exwm)
  (pdumper-require 'exwm-randr)
  (pdumper-require 'exwm-config)
  (pdumper-require 'exwm-systemtray)
  (defun farl-exwm/name-buffer-after-window-title ()
    "Rename the current `exwm-mode' buffer after the X window's title."
    (exwm-workspace-rename-buffer exwm-title))
  (setq exwm-floating-border-width window-divider-default-right-width
        exwm-floating-border-color (face-background 'mode-line))
  (use-package dmenu
    :ensure t
    :defer t
    :custom (dmenu-prompt-string "s-x "))
  (use-package exwm-mff
    :ensure t
    :defer t
    :hook (exwm-init . exwm-mff-mode))
  (defcustom farl-exwm/workspace-names '("1"
                                         "2"
                                         "3"
                                         "4"
                                         "5"
                                         "6"
                                         "office"
                                         "discord"
                                         "telegram"
                                         "games")
    "The names assigned to workspaces through `exwm-workspace-index-map'."
    :tag "Workspace names"
    :group 'exwm
    :type 'list)
  
  (defun farl-exwm/workspace-index-map (index)
    "Return either a workspace name for a given INDEX or INDEX itself."
    (or (elt farl-exwm/workspace-names index) index))
  (use-package minibuffer-line
    :ensure t
    :defer t
    :init
    (defun farl-exwm/list-workspaces ()
      "List EXWM workspaces."
      (exwm-workspace--update-switch-history)
      (elt exwm-workspace--switch-history
           (exwm-workspace--position exwm-workspace--current)))
    :custom-face (minibuffer-line ((t (:inherit default))))
    :custom (minibuffer-line-format '((:eval (farl-exwm/list-workspaces))))
    :hook ((exwm-init . minibuffer-line-mode)
           (exwm-workspace-switch . minibuffer-line--update)))
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
                         (concat "xinput | grep ELECOM | head -n 1 "
                                 "| sed -r 's/.*id=([0-9]+).*/\\1/' | "
                                 "tr '\\n' ' '"))))
      (start-process-shell-command
       "Trackball Setup" nil (concat "xinput set-prop " trackball-id
                                     "'libinput Button Scrolling Button' "
                                     "10"))
      (start-process-shell-command
       "Trackball Setup" nil (concat "xinput set-prop " trackball-id
                                     "'libinput Scroll Method Enabled' "
                                     "0 0 1"))
      (start-process-shell-command
       "Trackball Setup" nil (concat "xinput set-button-map " trackball-id
                                     "1 2 3 4 5 6 7 8 9 2 1 2")))
    ;; Keyboard
    (start-process "Keyboard Setup" nil "setxkbmap"
                   "-option" "ctrl:nocaps"))
  (defun display-and-dock-setup ()
    "Configure displays and dock if applicable."
    (interactive)
    (unless (get-process "Monitor Settings")
      (if (member "LVDS1" (get-connected-monitors))
          (display-setup-x230)
        (progn
          (display-setup-w541)
          (peripheral-setup)))))
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
      (pdumper-require 'system-packages)
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
    :custom (system-packages-noconfirm t)
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
    :custom ((desktop-environment-update-exwm-global-keys :prefix)
             (desktop-environment-brightness-normal-increment "5%+")
             (desktop-environment-brightness-normal-decrement "5%-")
             (desktop-environment-volume-toggle-command
              (concat "[ \"$(amixer set Master toggle | grep off)\" ] "
                      "&& echo Volume is now muted. | tr '\n' ' ' "
                      "|| echo Volume is now unmuted. | tr '\n' ' '"))
             (desktop-environment-volume-toggle-microphone-command
              (concat "[ \"$(amixer set Capture toggle | grep off)\" ] "
                      "&& echo Microphone is now muted. | tr '\n' ' ' "
                      "|| echo Microphone is now unmuted | tr '\n' ' '"))
             (desktop-environment-screenlock-command
              (concat "i3lock -nmk --color=000000 --timecolor=ffffffff "
                      " --datecolor=ffffffff --wrongcolor=ffffffff "
                      "--ringcolor=00000000 --insidecolor=00000000 "
                      "--keyhlcolor=00000000 --bshlcolor=00000000 "
                      "--separatorcolor=00000000 --ringvercolor=00000000 "
                      "--insidevercolor=00000000 --linecolor=00000000 "
                      "--ringwrongcolor=00000000 --insidewrongcolor=00000000 "
                      "--timestr=%H:%M --datestr='%a %d %b' --time-font=Iosevka "
                      "--date-font=Iosevka --wrong-font=Iosevka --timesize=128 "
                      "--datesize=64 --wrongsize=32 --time-align 0 --date-align 0 "
                      "--wrong-align 0 --indpos=-10:-10 --timepos=200:125 "
                      "--datepos=200:215 --wrongpos=200:155 --locktext='' "
                      "--lockfailedtext='' --noinputtext='' --veriftext='' "
                      "--wrongtext='WRONG' --force-clock --radius 1 --ring-width 1 "))
             (desktop-environment-screenshot-directory "~/screenshots")
             (desktop-environment-screenshot-command
              "FILENAME=$(date +'%Y-%m-%d-%H:%M:%S').png && maim $FILENAME")
             (desktop-environment-screenshot-partial-command
              "FILENAME=$(date +'%Y-%m-%d-%H:%M:%S').png && maim -s $FILENAME"))
    :hook (exwm-init . desktop-environment-mode)
    :bind (:map desktop-environment-mode-map
           ("<XF86ScreenSaver>" . desktop-environment-lock-screen)
           ("<print>" . farl-de/desktop-environment-screenshot-part-clip)
           ("<S-print>" . farl-de/desktop-environment-screenshot-clip)
           ("<C-print>" . farl-de/desktop-environment-screenshot-part)
           ("<C-S-print>" . farl-de/desktop-environment-screenshot)))
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
    (dolist (command
             '(;; Create null sink `loop'
               "load-module module-null-sink sink_name=loop"
               "update-sink-proplist loop device.description=loop"
               ;; Create null sink `out'
               "load-module module-null-sink sink_name=out"
               "update-sink-proplist out device.description=out"
               ;; Loop `loop' to primary output
               "load-module module-loopback source=loop.monitor"
               ;; Pipe it into `out'
               "load-module module-loopback source=loop.monitor sink=out"
               ;; Loop primary input into `out'
               "load-module module-loopback sink=out"))
      (shell-command (concat "pacmd " command)))
    ;; Run `pavucontrol' and then unload the modules after it completes
    (start-process-shell-command
     "Audio Loop" nil (concat "pavucontrol;"
                              "pacmd unload-module module-null-sink;"
                              "pacmd unload-module module-loopback")))
  (use-package xkb
    :load-path "lisp/xkb"
    :defer t
    :custom ((xkb-cycle-layouts '("us"
                                  "epo"
                                  "de"))
             (xkb-options '("ctrl:nocaps")))
    :hook (exwm-init . xkb-cycle-mode))
  (defun shut-down--computer ()
    "Shut down the computer."
    (shell-command "shutdown now"))
  
  (defun shut-down-computer ()
    "Shut down the computer."
    (interactive)
    (add-hook 'kill-emacs-hook #'shut-down--computer)
    (save-buffers-kill-emacs)
    (remove-hook 'kill-emacs-hook #'shut-down--computer))
  (defun reboot--computer ()
    "Run the reboot command."
    (shell-command "reboot"))
  
  (defun reboot-computer ()
    "Reboot the computer."
    (interactive)
    (add-hook 'kill-emacs-hook #'reboot--computer)
    (save-buffers-kill-emacs)
    (remove-hook 'kill-emacs-hook #'reboot--computer))
  (defun suspend-computer ()
    (interactive)
    (when (yes-or-no-p "Really suspend? ")
      (shell-command "systemctl suspend -i")))
  (defun farl-exwm/C-s ()
    "Pass C-s to the EXWM window."
    (interactive)
    (execute-kbd-macro (kbd "C-q C-s")))
  
  (defun farl-exwm/C-k ()
    "Pass C-k to the EXWM window."
    (interactive)
    (execute-kbd-macro (kbd "C-q C-k")))
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (setenv "XDG_CURRENT_DESKTOP" "emacs")
  (setenv "GTK2_RC_FILES" (user-config-file "gtk-2.0/gtkrc"))
  (setenv "QT_QPA_PLATFORMTHEME" "gtk2")
  (setenv "_JAVA_AWT_WM_NONREPARENTING" "1")
  (start-process "Disable Blanking" nil "xset"
                 "s" "off" "-dpms")
  (start-process "Keyboard Layout" nil "setxkbmap"
                 "us" "-option" "ctrl:nocaps")
  (start-process "Trackpad Setup" nil "xinput"
                 "disable" (shell-command-to-string
                            (concat "xinput | grep Synap | head -n 1 | "
                                    "sed -r 's/.*id=([0-9]+).*/\\1/' | "
                                    "tr '\n' ' ' | sed 's/ //'")))
  (start-process "Compositor" nil "xcompmgr")
  (start-process "Fallback Cursor" nil "xsetroot"
                 "-cursor_name" "left_ptr")
  (start-process "Mouse banisher" nil "xbanish")
  (exwm-enable)
  (exwm-config-ido)
  (exwm-randr-enable)
  (exwm-systemtray-enable)
  (defun farl-exwm/on-logout ()
    "Run this when logging out as part of `kill-emacs-hook'."
    (shell-command "hsetroot -solid '#000000'"))
  :custom ((exwm-replace t)
           (exwm-workspace-number 10)
           (exwm-randr-workspace-monitor-plist '(0 "DP2-2"
                                                 1 "DP2-1"
                                                 2 "DP2-3"
                                                 3 "DP2-2"
                                                 4 "DP2-1"
                                                 5 "DP2-3"
                                                 6 "DP2-2"
                                                 7 "DP2-1"
                                                 8 "DP2-3"
                                                 9 "DP2-2"))
           (exwm-manage-configurations '(((string= exwm-class-name "Steam")
                                          workspace 9)
                                         ((string= exwm-class-name "hl2_linux")
                                          floating-mode-line nil)
                                         ((string= exwm-class-name "TelegramDesktop")
                                          workspace 8)
                                         ((string= exwm-class-name "discord")
                                          workspace 7)
                                         ((or (string-match-p "libreoffice"
                                                              exwm-class-name)
                                              (string= exwm-class-name "MuseScore3")
                                              (string= exwm-class-name "Gimp"))
                                          workspace 6)
                                         ((string= exwm-title "Event Tester")
                                          floating-mode-line nil
                                          floating t)))
           (exwm-workspace-index-map #'farl-exwm/workspace-index-map)
           (exwm-input-global-keys `(;; Switching workspace focus
                                     ;; s-1 for 1, s-2 for 2, etc...
                                     ,@(mapcar
                                        (lambda (i)
                                          `(,(kbd (format "s-%d" (% (1+ i) 10)))
                                            .
                                            (lambda ()
                                              (interactive)
                                              (exwm-workspace-switch-create ,i))))
                                        (number-sequence 0 9))
           
                                     ;; Switching window to a workspace
                                     ;; This was annoying to get working
                                     ;; s-! for 1, s-@ for 2, etc...
                                     ,@(mapcar
                                        (lambda (i)
                                          `(,(kbd (format "s-%s" (nth i '("!"
                                                                          "@"
                                                                          "#"
                                                                          "$"
                                                                          "%"
                                                                          "^"
                                                                          "&"
                                                                          "*"
                                                                          "("
                                                                          ")"))))
                                            .
                                            (lambda ()
                                              (interactive)
                                              (exwm-workspace-move-window ,i))))
                                        (number-sequence 0 9))
           
                                     ;; Toggle how input is sent to X windows
                                     ([?\s-q] . exwm-input-toggle-keyboard)
           
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
                                     ([?\s-x]           . dmenu)
                                     ([s-tab]           . audio-loopback)
                                     ([?\s-w]           . xkb-set-layout)
           
                                     ;; Controlling EMMS
                                     ([XF86AudioNext] . emms-next)
                                     ([XF86AudioPrev] . emms-previous)
                                     ([XF86AudioPlay] . emms-pause)
                                     ([XF86AudioStop] . emms-stop)))
           (exwm-input-simulation-keys `(;; Navigation
                                         ([?\M-<] . [C-home])
                                         ([?\M->] . [C-end])
                                         ([?\C-a] . [home])
                                         ([?\C-e] . [end])
                                         ([?\C-v] . [next])
                                         ([?\M-v] . [prior])
           
                                         ([?\C-b] . [left])
                                         ([?\C-f] . [right])
                                         ([?\C-p] . [up])
                                         ([?\C-n] . [down])
           
                                         ([?\M-b] . [C-left])
                                         ([?\M-f] . [C-right])
                                         ([?\M-n] . [C-down])
                                         ([?\M-p] . [C-up])
           
                                         ;; Selecting via navigation
                                         (,(kbd "C-S-b") . [S-left])
                                         (,(kbd "C-S-f") . [S-right])
                                         (,(kbd "C-S-n") . [S-down])
                                         (,(kbd "C-S-p") . [S-up])
           
                                         ;; Copy/Paste
                                         ([?\C-w] . [?\C-x])
                                         ([?\M-w] . [?\C-c])
                                         ([?\C-y] . [?\C-v])
                                         ([?\C-s] . [?\C-f])
                                         ([?\C-\/] . [?\C-z])
           
                                         ;; Other
                                         ([?\C-d] . [delete])
                                         ([?\M-d] . [C-delete])
                                         ([?\C-k] . [S-end delete])
                                         ([?\C-g] . [escape]))))
  :hook ((exwm-update-title . farl-exwm/name-buffer-after-window-title)
         (exwm-randr-screen-change . display-and-dock-setup)
         (kill-emacs . farl-exwm/on-logout))
  :bind (("C-x C-M-c" . shut-down-computer)
         ("C-x C-M-r" . reboot-computer)
         ("C-x C-M-s" . suspend-computer)
         :map exwm-mode-map
         ("C-x C-s" . farl-exwm/C-s)
         ("C-c C-l" . farl-exwm/C-k)
         ("C-q" . exwm-input-send-next-key)
         ("C-c C-q" . nil)
         ("C-c C-f" . nil)
         ("C-c C-t C-f" . nil)
         ("C-c C-t C-v" . nil)
         ("C-c C-t C-m" . nil)))

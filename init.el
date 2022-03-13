;;; init.el --- Initializing Farlado's Illiterate GNU Emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



;;; Commentary:

;; This file has been automatically tangled from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.



;;; Code:

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(pdumper-require 'use-package)
(setq use-package-compute-statistics t)

(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

(use-package auto-package-update
  :ensure t
  :defer t
  :custom ((auto-package-update-interval 2)
           (auto-package-update-hide-results t)
           (auto-package-update-delete-old-versions t))
  :hook (after-init . auto-package-update-maybe))

(use-package try
  :ensure t
  :defer t)

(use-package dracula-theme
  :if window-system
  :ensure t
  :defer t
  :init
  (if pdumper-dumped
      (enable-theme 'dracula)
    (load-theme 'dracula t))
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
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'alpha 90))
  (add-to-list 'default-frame-alist '(alpha . 90)))

(use-package mood-line
  :ensure t
  :defer t
  :init
  (mood-line-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (display-time-mode 1)
  (display-battery-mode 1)
  :custom ((display-time-format "%a %m/%d %H:%M")
           (display-time-day-and-date t)
           (display-time-24hr-format t)))

(use-package dashboard
  :ensure t
  :defer t
  :init
  (defun dashboard-or-scratch ()
    "Open either dashboard or the scratch buffer."
    (or (get-buffer "*dashboard*")
        (get-buffer "*scratch*")))
  (defun dashboard-immortal ()
    "Make the dashboard buffer immortal."
    (emacs-lock-mode 'kill))
  (dashboard-setup-startup-hook)
  :custom ((inhibit-start-screen t)
           (dashboard-set-footer nil)
           (dashboard-startup-banner (locate-user-emacs-file "logo.png"))
           (dashboard-items '((recents . 10)))
           (initial-buffer-choice #'dashboard-or-scratch)
           (dashboard-banner-logo-title
            "Welcome to Farlado's Illiterate GNU Emacs!"))
  :hook (dashboard-mode . dashboard-immortal))

(global-visual-line-mode 1)

(setq-default fill-column 80)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

(tooltip-mode -1)
(setq use-dialog-box nil
      use-file-dialog nil)

(setq-default cursor-type 'bar)

(use-package page-break-lines
  :ensure t
  :defer t
  :hook (after-init . global-page-break-lines-mode))

(use-package display-line-numbers
  :defer t
  :custom ((indicate-empty-lines t)
           (display-line-numbers-type 'relative))
  :hook ((text-mode
          prog-mode
          conf-mode) . display-line-numbers-mode))

(use-package paren
  :defer t
  :init
  (show-paren-mode 1)
  :custom-face (show-paren-match ((t (:weight extra-bold
                                      :underline t))))
  :custom ((show-paren-style 'parentheses)
           (show-paren-delay 0.00000001)))

(use-package rainbow-mode
  :if window-system
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(pdumper-require 'server)
(unless (server-running-p)
  (server-start))

(setq disabled-command-function nil)

(add-hook 'minibuffer-setup-hook #'garbage-collect-defer)
(add-hook 'minibuffer-exit-hook #'garbage-collect-restore)

(setq confirm-kill-emacs #'yes-or-no-p)

(setq scroll-margin 0
      auto-window-vscroll nil
      scroll-preserve-screen-position 1
      scroll-conservatively most-positive-fixnum
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p #'y-or-n-p
  "Use `y-or-n-p' instead of a yes/no prompt.")

(use-package which-key
  :ensure t
  :defer t
  :custom (echo-keystrokes 0.00000001)
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

(use-package counsel
  :ensure t
  :defer t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package company-emoji
  :after company
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends #'company-emoji))

(use-package ibuffer
  :defer t
  :init
  (defun farl-ibuffer/use-default-filter-group ()
    "Switch to the intended filter group."
    (ibuffer-switch-to-saved-filter-groups "default"))
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  :custom ((ibuffer-saved-filter-groups
            (quote (("default"
                     ("exwm" (and (not (name . "Firefo[x<>1-9]+$"))
                                  (or (name . "^\\*system-packages\\*$")
                                      (name . "^\\*Wi-Fi Networks\\*$")
                                      (name . "^\\*XELB-DEBUG\\*$")
                                      (mode . exwm-mode))))
                     ("firefox" (name . "Firefo[x<>1-9]+$"))
                     ("emms" (or (mode . emms-playlist-mode)
                                 (mode . emms-browser-mode)
                                 (mode . emms-mode)))
                     ("ebooks" (mode . nov-mode))
                     ("magit" (name . "^magit.*:"))
                     ("dired" (or (mode . dired-mode)
                                  (mode . wdired-mode)))
                     ("elisp" (mode . emacs-lisp-mode))
                     ("haskell" (mode . haskell-mode))
                     ("python" (mode . python-mode))
                     ("org"   (mode . org-mode))
                     ("term" (mode . term-mode))
                     ("emacs" (or (name . "^\\*package.*results\\*$")
                                  (name . "^\\*Shell.*Output\\*$")
                                  (name . "^\\*Compile-Log\\*$")
                                  (name . "^\\*Completions\\*$")
                                  (name . "^\\*Backtrace\\*$")
                                  (name . "^\\*dashboard\\*$")
                                  (name . "^\\*Messages\\*$")
                                  (name . "^\\*scratch\\*$")
                                  (name . "^\\*info\\*$")
                                  (name . "^\\*Help\\*$")))))))
           (uniquify-buffer-name-style 'forward)
           (uniquify-after-kill-buffer-p t)
           (initial-scratch-message ""))
  :hook (ibuffer-mode . farl-ibuffer/use-default-filter-group)
  :bind (("C-x b" . ibuffer)
         ("C-x C-b" . nil)
         ("C-x k" . kill-this-buffer)))

(use-package buffer-move
  :ensure t
  :defer t
  :init
  (defun split-and-follow-below ()
    "Open a new window vertically."
    (interactive)
    (split-window-below)
    (other-window 1)
    (ibuffer))
  (defun split-and-follow-right ()
    "Open a new window horizontally."
    (interactive)
    (split-window-right)
    (other-window 1)
    (ibuffer))
  (defun kill-this-buffer-and-window ()
    "Perform `kill-buffer-and-window'.  Altered to accomodate `exwm-mode'."
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
  (defun kill-all-buffers-and-windows ()
    "Kill all buffers and windows."
    (interactive)
    (when (yes-or-no-p "Really kill all buffers and windows? ")
      (save-some-buffers)
      (mapc 'kill-buffer (buffer-list))
      (delete-other-windows)))
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
         ("C-x o C-d" . buf-move-right)
         ("C-x 2" . split-and-follow-below)
         ("C-x 3" . split-and-follow-right)
         ("C-c b" . balance-windows)
         ("C-x 4 0" . kill-this-buffer-and-window)
         ("C-x 4 q" . kill-all-buffers-and-windows)))

(global-set-key (kbd "C-c d") #'cd)

(defun config-visit ()
  "Open the configuration file."
  (interactive)
  (find-file (locate-user-emacs-file "literate-emacs.org")))

(global-set-key (kbd "C-c e") #'config-visit)

(defun literate-dotfiles-visit ()
  "Open the literate dotfiles."
  (interactive)
  (find-file "~/.config/dotfiles/literate-dotfiles.org"))

(when (file-exists-p "~/.config/dotfiles/literate-dotfiles.org")
  (global-set-key (kbd "C-c M-e") #'literate-dotfiles-visit))

(defun sys-config-visit ()
  "Open the literate system configuration."
  (interactive)
  (find-file "~/.config/dotfiles/literate-sysconfig.org"))

(when (file-exists-p "~/.config/dotfiles/literate-sysconfig.org")
  (global-set-key (kbd "C-c C-M-e") #'sys-config-visit))

(use-package sudo-edit
  :ensure t
  :defer t
  :bind ("C-x C-M-f" . sudo-edit))

(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :mode ("\\.dot\\'" . graphviz-dot-mode))

(defun tangle-literate-program ()
  "Tangle a file if it's a literate programming file."
  (interactive)
  (when (string-match-p "literate.*.org$" buffer-file-name)
    (org-babel-tangle)))

(add-hook 'after-save-hook #'tangle-literate-program -100)

(defun byte-compile-config-files ()
  "Byte-compile Emacs configuration files."
  (when (string-match-p "literate-emacs.org" (buffer-file-name))
    (byte-recompile-directory user-emacs-directory 0)))

(add-hook 'after-save-hook #'byte-compile-config-files 100)

(global-set-key (kbd "C-=") #'count-words)

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
      auto-save-default nil
      auto-save-list-file-prefix nil)

(use-package autorevert
  :defer t
  :init
  (global-auto-revert-mode 1)
  :custom ((global-auto-revert-non-file-buffers t)
           (auto-revert-remote-files t)
           (auto-revert-verbose nil)))

(setq require-final-newline t)
(setq-default indent-tabs-mode nil
              tab-width 4)

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

(use-package elec-pair
  :defer t
  :init
  (electric-pair-mode 1)
  (minibuffer-electric-default-mode 1)
  :custom (electric-pair-pairs '((?\{ . ?\})
                                 (?\( . ?\))
                                 (?\[ . ?\])
                                 (?\" . ?\"))))

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

(use-package lisp-mode
  :defer t
  :custom (inferior-lisp-program "sbcl"))

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
  :custom ((posframe-mouse-banish nil)
           (flycheck-posframe-position 'window-bottom-left-corner))
  :hook ((flycheck-mode . flycheck-posframe-mode)
         (flycheck-posframe-mode . flycheck-posframe-configure-pretty-defaults)))

(use-package avy-flycheck
  :after flycheck
  :ensure t
  :defer t
  :bind (:map prog-mode-map
         ("C-c C-'" . avy-flycheck-goto-error)))

(use-package org
  :defer t
  :init
  (pdumper-require 'org)
  (set-face-attribute 'org-document-title nil
                      :weight 'extra-bold
                      :height 1.8)
  (set-face-attribute 'org-level-1 nil
                      :height 1.3)
  (set-face-attribute 'org-level-2 nil
                      :height 1.1)
  (set-face-attribute 'org-level-3 nil
                      :height 1.0)
  (set-face-attribute 'org-code nil
                      :inherit 'font-lock-string-face)
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
             (string-match-p "literate.*.org$" buffer-file-name))))
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

(setq calendar-week-start-day 1)
(global-set-key (kbd "C-c l") #'calendar)

(use-package nov
  :ensure t
  :defer t
  :custom (nov-text-width 80)
  :mode ("\\.epub\\'" . nov-mode))

(use-package wdired
  :defer t
  :custom ((dired-listing-switches "-alh --group-directories-first")
           (wdired-allow-to-change-permissions t)))

(use-package term
  :defer t
  :init
  (defun farl-term/use-shell (force-bash)
    "Force `term' to use the default shell, ignoring FORCE-BASH."
    (interactive (list (getenv "SHELL"))))
  (advice-add 'ansi-term :before #'farl-term/use-shell)
  :bind ("C-c t" . ansi-term))

(global-set-key (kbd "C-h 4 m") #'man)
(global-set-key (kbd "C-h 4 w") #'woman)

(use-package wttrin
  :ensure t
  :defer t
  :custom (wttrin-default-cities '("Indianapolis"))
  :bind ("C-c w" . wttrin))

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

(global-unset-key (kbd "C-c g"))

(use-package yahtzee
  :ensure t
  :defer t
  :bind ("C-c g y" . yahtzee))

(use-package sudoku
  :ensure t
  :defer t
  :bind ("C-c g s" . sudoku))

(use-package tetris
  :defer t
  :bind (("C-c g t" . 'tetris)
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
  :bind ("C-c g c" . chess))

(use-package 2048-game
  :ensure t
  :defer t
  :bind ("C-c g 2" . 2048-game))

(use-package exwm
  :if (getenv "_RUN_EXWM")
  :ensure t
  :defer t
  :init
  (setenv "_RUN_EXWM")
  (pdumper-require 'exwm)
  (pdumper-require 'exwm-xim)
  (pdumper-require 'exwm-randr)
  (pdumper-require 'exwm-config)
  (pdumper-require 'exwm-systemtray)
  (defun farl-exwm/name-buffer-after-window-title ()
    "Rename the current `exwm-mode' buffer after the X window's title."
    (exwm-workspace-rename-buffer exwm-title))
  (setq exwm-floating-border-width window-divider-default-right-width
        exwm-floating-border-color (face-background 'mode-line))
  (defvar farl-exwm/workspace-names '("" "" "" "" ""
                                      "" "" "" "" "")
    "The names assigned to workspaces through `exwm-workspace-index-map'.")
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
  (defun farl-exwm/workspace-next ()
    "Move forward one workspace."
    (interactive)
    (if (< exwm-workspace-current-index (1- exwm-workspace-number))
        (exwm-workspace-switch (1+ exwm-workspace-current-index))
      (message "No next workspace.")))
  
  (defun farl-exwm/workspace-prev ()
    "Move to the previous workspace."
    (interactive)
    (if (> exwm-workspace-current-index 0)
        (exwm-workspace-switch (1- exwm-workspace-current-index))
      (message "No previous workspace.")))
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
                             "--rotate" (if (string= "DP2-2" monitor)
                                            "left"
                                          (if (string= "DP2-3" monitor)
                                              "right"
                                            "normal"))
                             ;; Every enabled monitor needs a position.
                             "--pos" (if (string-match-p "DP2-3" monitor)
                                         "3000x0"
                                       (if (string-match-p "DP2-1" monitor)
                                           "1080x0"
                                         "0x0")))
              ;; Setting a monitor as primary occurs outside enabling it.
              ;; This is due to how `start-process' takes arguments.
              (when (or (string= "DP2-1" monitor)
                        (string= "eDP1" monitor))
                (start-process "xrandr" nil "xrandr"
                               "--output" monitor
                               "--primary")))
          (start-process "xrandr" nil "xrandr"
                         "--output" monitor
                         "--off")))))
  (defun peripheral-setup ()
    "Configure peripherals I connect to my dock."
    (interactive)
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
  (use-package desktop-environment
    :ensure t
    :defer t
    :init
    (defun farl-de/screenshot ()
      "Take a screenshot and store it in a file."
      (interactive)
      (desktop-environment-screenshot)
      (message "Screenshot saved in ~/screenshots."))
    
    (defun farl-de/screenshot-part ()
      "Take a capture of a portion of the screen and store it in a file."
      (interactive)
      (desktop-environment-screenshot-part)
      (message "Screenshot saved in ~/screenshots."))
    
    (defun farl-de/screenshot-clip ()
      "Take a screenshot and put it in the clipboard."
      (interactive)
      (shell-command
       (concat desktop-environment-screenshot-command
               " && xclip $FILENAME -selection clipboard "
               "-t image/png &> /dev/null && rm $FILENAME"))
      (message "Screenshot copied to clipboard."))
    
    (defun farl-de/screenshot-part-clip ()
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
              (concat "i3lock -nk --color=000000 --timecolor=ffffffff "
                      "--datecolor=ffffffff --wrongcolor=ffffffff "
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
                      "--wrongtext='WRONG' --force-clock --radius 1 --ring-width 1 "
                      "--pass-media-keys --pass-screen-keys --pass-power-keys "))
             (desktop-environment-screenshot-directory "~/screenshots")
             (desktop-environment-screenshot-command
              "FILENAME=$(date +'%Y-%m-%d-%H:%M:%S').png && maim $FILENAME")
             (desktop-environment-screenshot-partial-command
              "FILENAME=$(date +'%Y-%m-%d-%H:%M:%S').png && maim -s $FILENAME"))
    :hook (exwm-init . desktop-environment-mode)
    :bind (:map desktop-environment-mode-map
           ("<XF86ScreenSaver>" . desktop-environment-lock-screen)
           ("<print>" . farl-de/screenshot-part-clip)
           ("<S-print>" . farl-de/screenshot-clip)
           ("<C-print>" . farl-de/screenshot-part)
           ("<C-S-print>" . farl-de/screenshot)))
  (use-package wallpaper
    :ensure t
    :defer t
    :hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
           (exwm-init . wallpaper-cycle-mode)))
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
  (defun shut-down-computer ()
    "Shut down the computer."
    (interactive)
    (let ((shut-down (lambda ()
                       (shell-command "shutdown now"))))
      (add-hook 'kill-emacs-hook shut-down)
      (save-buffers-kill-emacs)
      (remove-hook 'kill-emacs-hook shut-down)))
  (defun reboot-computer ()
    "Reboot the computer."
    (interactive)
    (let ((reboot (lambda ()
                    (shell-command "reboot"))))
      (add-hook 'kill-emacs-hook reboot)
      (save-buffers-kill-emacs)
      (remove-hook 'kill-emacs-hook reboot)))
  (defun suspend-computer ()
    (interactive)
    (when (yes-or-no-p "Really suspend? ")
      (start-process "suspend" nil "systemctl"
                     "suspend" "-i")))
  (defun farl-exwm/C-s ()
    "Pass C-s to the EXWM window."
    (interactive)
    (execute-kbd-macro (kbd "C-q C-s")))
  (defun farl-exwm/C-k ()
    "Pass C-k to the EXWM window."
    (interactive)
    (execute-kbd-macro (kbd "C-q C-k")))
  (defun farl-exwm/C-a ()
    "Pass C-a to the EXWM window."
    (interactive)
    (execute-kbd-macro (kbd "C-q C-a")))
  (defun farl-exwm/C-o ()
    "Pass the equivalent of C-o to the EXWM window."
    (interactive)
    (execute-kbd-macro (kbd "<S-return> C-b")))
  (push ?\C-\\ exwm-input-prefix-keys)
  (defun farl-exwm/on-startup ()
    "Start EXWM and related processes."
    (set-frame-parameter nil 'fullscreen 'fullboth)
    (setenv "XDG_CURRENT_DESKTOP" "emacs")
    (setenv "GTK2_RC_FILES" (expand-file-name "~/.config/gtk-2.0/gtkrc"))
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
    (start-process "Notifications" nil "dunst")
    (exwm-enable)
    (exwm-xim-enable)
    (exwm-randr-enable)
    (exwm-systemtray-enable))
  (defun farl-exwm/on-logout ()
    "Run this when logging out as part of `kill-emacs-hook'."
    (start-process "Root window" nil "hsetroot"
                   "-solid" "'#000000'"))
  :custom ((exwm-replace t)
           (org-agenda-files (when (file-directory-p "~/agendas")
                               (directory-files-recursively
                                "~/agendas" ".org$" nil)))
           (exwm-workspace-index-map #'farl-exwm/workspace-index-map)
           (exwm-workspace-number 10)
           (exwm-randr-workspace-monitor-plist '(0 "DP2-1"
                                                 1 "DP2-2"
                                                 2 "DP2-3"
                                                 3 "DP2-1"
                                                 4 "DP2-2"
                                                 5 "DP2-3"
                                                 6 "DP2-1"
                                                 7 "DP2-2"
                                                 8 "DP2-3"
                                                 9 "DP2-1"))
           (exwm-manage-configurations '(((string= exwm-class-name "Steam")
                                          workspace 9
                                          floating t)
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
           
                                     ;; Change workspace focus by relation
                                     ([?\s-q] . farl-exwm/workspace-prev)
                                     ([?\s-e] . farl-exwm/workspace-next)
           
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
                                     ([?\s-w] . exwm-input-toggle-keyboard)
           
                                     ;; Window size adjustment
                                     (,(kbd "C-s-w") . shrink-window)
                                     (,(kbd "C-s-s") . enlarge-window)
                                     (,(kbd "C-s-a") . shrink-window-horizontally)
                                     (,(kbd "C-s-d") . enlarge-window-horizontally)
           
                                     ;; Opening programs
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
                                     ([s-return]       . ansi-term)
                                     ([XF86Calculator] . calc)
           
                                     ;; Other desktop environment things
                                     ([?\s-x] . counsel-linux-app)
                                     ([s-tab] . audio-loopback)
                                     ([menu]  . counsel-M-x)
           
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
         (after-init . farl-exwm/on-startup)
         (kill-emacs . farl-exwm/on-logout))
  :bind (("C-x C-M-c" . shut-down-computer)
         ("C-x C-M-r" . reboot-computer)
         ("C-x C-M-s" . suspend-computer)
         :map exwm-mode-map
         ("C-c C-l" . farl-exwm/C-k)
         ("C-x C-s" . farl-exwm/C-s)
         ("C-x h" . farl-exwm/C-a)
         ("C-o" . farl-exwm/C-o)
         ("C-q" . exwm-input-send-next-key)
         ("C-c C-q" . nil)
         ("C-c C-f" . nil)
         ("C-c C-t C-f" . nil)
         ("C-c C-t C-v" . nil)
         ("C-c C-t C-m" . nil)))



;;; init.el ends here

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

(require 'use-package)
(setq use-package-compute-statistics t
      use-package-always-ensure t
      use-package-always-defer t)

(use-package async
  :custom ((direct-async-mode t)
           (async-bytecomp-package-mode t)
           (async-bytecomp-allowed-packages '(all))))

(use-package system-packages
  :init
  (use-package use-package-ensure-system-package)
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
                        (list-installed-packkages-all . "yay -Q")
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

(use-package auto-package-update
  :custom ((auto-package-update-interval 2)
           (auto-package-update-hide-results t)
           (auto-package-update-delete-old-versions t))
  :hook (after-init . auto-package-update-maybe))

(use-package try)

(use-package dracula-theme
  :if window-system
  :init
  (load-theme 'dracula t)
  (tooltip-mode -1)
  (setq use-dialog-box nil
	use-file-dialog nil)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (unless (member "Iosevka" (font-family-list))
    (system-packages-install "ttf-iosevka"))
  (when (member "Iosevka" (font-family-list))
    (set-face-attribute 'default nil
                        :font "Iosevka"
                        :height 100))
  (unless (member "Noto Color Emoji" (font-family-list))
    (system-packages-install "noto-fonts-emoji"))
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol
                      (font-spec :family "Noto Color Emoji")
                      nil 'prepend))
  (setq inhibit-compacting-font-caches t)
  (set-face-background 'fringe (face-background 'default))
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
  :init
  (mood-line-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (display-time-mode 1)
  :custom ((display-time-format "%a %m/%d %H:%M")
           (display-time-day-and-date t)
           (display-time-24hr-format t)))

(use-package dashboard
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

(require 'server)
(unless (server-running-p)
  (server-start))

(setq disabled-command-function nil)

(add-hook 'minibuffer-setup-hook #'farl-init/garbage-collect-defer)
(add-hook 'minibuffer-exit-hook #'farl-init/garbage-collect-restore)

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
  "Use `y-or-n-p' instead of `yes-or-no-p')

(use-package which-key
  :custom (echo-keystrokes 0.00000001)
  :hook (after-init . which-key-mode))

(use-package company
  :custom ((company-idle-delay 0.75)
           (company-minimum-prefix-length 3))
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("SPC" . company-abort)))

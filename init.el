;;; init.el --- Initializing Emacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil
      use-file-dialog nil)

(setq custom-file "/dev/null")

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

(setq package-selected-packages
      '(async use-package auto-package-update dashboard exwm dmenu desktop-environment
        system-packages emms avy ido-vertical-mode buffer-move sudo-edit 2048-game
        graphviz-dot-mode vterm nov wttrin chess sudoku smex which-key popup-kill-ring
        swiper hungry-delete magit flycheck avy-flycheck company-jedi haskell-mode
        markdown-mode org-bullets epresent leuven-theme pretty-mode rainbow-mode
        rainbow-delimiters spaceline diminish))

(require 'package)
(defun package--save-selected-packages (&rest opt) nil)

(setq package-enable-at-startup nil
      package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")))

(when (< emacs-major-version 27)
  (package-initialize))

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
        auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

(require 'server)

(defun server-start-if-not-running ()
  "Call `server-start' if `server-running-p' returns nil."
  (unless (server-running-p)
    (server-start)))

(add-hook 'after-init-hook 'server-start-if-not-running)

(use-package dashboard
  :ensure t
  :defer t
  :init
  (setq dashboard-set-footer nil
        inhibit-startup-screen t
        dashboard-items '((recents . 10))
        dashboard-startup-banner 'official
        initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-banner-logo-title "Welcome to Farlado's Illiterate GNU Emacs!")
  (dashboard-setup-startup-hook))

(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka"))

(let* ((res (if (eq window-system 'x)
                (string-to-number
                 (shell-command-to-string
                  (concat "xrandr | grep \\* | "
                          "cut -d x -f 1 | "
                          "sort -n | head -n 1")))
              (/ (display-pixel-width) (display-screens))))
       (size (if (<= res 1366) 100
               (if (<= res 1920) 150
                 (if (<= res 2560) 180
                   (if (<= res 3840) 220
                     240))))))
  (set-face-attribute 'default nil :height size))

(use-package leuven-theme
  :if window-system
  :ensure t
  :defer t
  :init
  (setq leuven-scale-org-agenda-structure t
        leuven-scale-outline-headlines t)
  (load-theme 'leuven t))

(setq window-divider-default-right-width 3)
(dolist (face '(window-divider-first-pixel
                window-divider-last-pixel
                window-divider))
  (set-face-foreground face (face-attribute 'mode-line :background)))
(window-divider-mode 1)
(fringe-mode 10)

(use-package spaceline
  :ensure t
  :defer t
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'wave
        spaceline-buffer-encoding-abbrev-p nil
        spaceline-buffer-size-p nil
        spaceline-line-column-p t
        column-number-indicator-zero-based nil)
  (if window-system
      (spaceline-emacs-theme)
    (spaceline-spacemacs-theme)))

(setq display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

(use-package diminish
  :ensure t
  :defer t
  :init
  (defun diminish-minor-modes ()
    "Diminish the minor modes in the list `minor-modes-to-diminish'."
    (dolist (mode minor-modes-to-diminish)
      (diminish mode)))
  (defvar minor-modes-to-diminish '(eldoc-mode
                                    subword-mode
                                    company-mode
                                    rainbow-mode
                                    flycheck-mode
                                    flyspell-mode
                                    which-key-mode
                                    auto-revert-mode
                                    visual-line-mode
                                    haskell-doc-mode
                                    flyspell-prog-mode
                                    hungry-delete-mode
                                    page-break-lines-mode
                                    desktop-environment-mode
                                    haskell-indentation-mode
                                    interactive-haskell-mode
                                    compilation-shell-minor-mode)
    "Minor modes to diminish using `diminish-minor-modes'.")
  (add-hook 'after-init-hook 'diminish-minor-modes))

(global-page-break-lines-mode 1)

(global-display-line-numbers-mode 1)
(setq-default indicate-empty-lines t)

(dolist (hook '(Man-mode-hook
                nov-mode-hook
                shell-mode-hook
                vterm-mode-hook
                shell-mode-hook
                snake-mode-hook
                tetris-mode-hook
                sudoku-mode-hook
                custom-mode-hook
                ibuffer-mode-hook
                epresent-mode-hook
                dashboard-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(show-paren-mode 1)
(setq show-paren-style 'parenthesis
      show-paren-delay 0)

(use-package pretty-mode
  :if window-system
  :ensure t
  :defer t
  :init
  (global-pretty-mode 1)
  (pretty-activate-groups '(:nil
                            :sets
                            :logic
                            :greek
                            :types
                            :other
                            :arrows
                            :ordering
                            :equality
                            :function
                            :undefined
                            :arithmetic
                            :parentheses
                            :punctuation
                            :quantifiers
                            :sub-and-superscripts)))

(use-package rainbow-mode
  :if window-system
  :ensure t
  :defer t
  :init
  (define-globalized-minor-mode global-rainbow-mode rainbow-mode rainbow-mode)
  (global-rainbow-mode 1))

(use-package rainbow-delimiters
  :if window-system
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(when (and (eq window-system 'x)
         (= (shell-command "wmctrl -m  1> /dev/null 2> /dev/null") 1))
  (set-frame-parameter nil 'fullscreen 'fullboth)

(use-package exwm
  :ensure t
  :defer t
  :init
  (require 'exwm)
  (require 'exwm-randr)
  (require 'exwm-config)
  (require 'exwm-systemtray))

(use-package dmenu
  :ensure t
  :defer t)

(use-package desktop-environment
  :ensure t
  :defer t
  :init
  (desktop-environment-mode 1))

(setq desktop-environment-volume-toggle-command
      (concat "[ \"$(amixer set Master toggle | grep off)\" ] "
              "&& echo Volume is now muted. | tr '\n' ' ' "
              "|| echo Volume is now unmuted. | tr '\n' ' '")
      desktop-environment-volume-toggle-microphone-command
      (concat "[ \"$(amixer set Capture toggle | grep off)\" ] "
              "&& echo Microphone is now muted. | tr '\n' ' ' "
              "|| echo Microphone is now unmuted | tr '\n' ' '"))

(setq desktop-environment-brightness-normal-increment "5%+"
      desktop-environment-brightness-normal-decrement "5%-")

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
  (shell-command (concat desktop-environment-screenshot-command
                         " && xclip $FILENAME -selection clipboard "
                         "-t image/png &> /dev/null && rm $FILENAME"))
  (message "Screenshot copied to clipboard."))

(defun farl-de/desktop-environment-screenshot-part-clip ()
  "Take a shot of a portion of the screen and put it in the clipboard."
  (interactive)
  (shell-command (concat desktop-environment-screenshot-partial-command
                         " && xclip $FILENAME -selection clipboard "
                         "-t image/png &> /dev/null && rm $FILENAME"))
  (message "Screenshot copied to clipboard."))

(setq desktop-environment-screenlock-command
      (concat "i3lock -nmk --color=000000 --timecolor=ffffffff --datecolor=ffffffff "
              "--wrongcolor=ffffffff --ringcolor=00000000 --insidecolor=00000000 "
              "--keyhlcolor=00000000 --bshlcolor=00000000 --separatorcolor=00000000 "
              "--ringvercolor=00000000 --insidevercolor=00000000 --linecolor=00000000 "
              "--ringwrongcolor=00000000 --insidewrongcolor=00000000 --timestr=%H:%M "
              "--datestr='%a %d %b' --time-font=Iosevka --date-font=Iosevka "
              "--wrong-font=Iosevka --timesize=128 --datesize=64 --wrongsize=32 "
              "--time-align 0 --date-align 0 --wrong-align 0 --indpos=-10:-10 "
              "--timepos=200:125 --datepos=200:215 --wrongpos=200:155 --locktext='' "
              "--lockfailedtext='' --noinputtext='' --radius 1 --ring-width 1 "
              " --veriftext='' --wrongtext='WRONG' --force-clock"))

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

(setq exwm-workspace-number 3
      exwm-workspace-index-map (lambda (index)
                                 (let ((named-workspaces ["1" "2" "3"]))
                                   (if (< index (length named-workspaces))
                                       (elt named-workspaces index)
                                     (number-to-string index))))
      exwm-randr-workspace-output-plist '(0 "LVDS1"
                                          0 "LVDS-1"
                                          0 "eDP-1-1"
                                          0 "DP-1-2-2"
                                          1 "DP-1-2-1"
                                          2 "DP-1-2-3")
      exwm-manage-configurations '(((string= exwm-class-name "Steam")
                                    floating-mode-line nil
                                    workspace 0
                                    floating t)
                                   ((string= exwm-instance-name "telegram-desktop")
                                    workspace 2)
                                   ((string= exwm-class-name "discord")
                                    workspace 1)
                                   ((string= exwm-title "Event Tester")
                                    floating-mode-line nil
                                    floating t)))

(defun farl-exwm/workspace-0 ()
  "Switch to EXWM workspace 0."
  (interactive)
  (exwm-workspace-switch-create 0))

(defun farl-exwm/workspace-1 ()
  "Switch to EXWM workspace 1."
  (interactive)
  (exwm-workspace-switch-create 1))

(defun farl-exwm/workspace-2 ()
  "Switch to EXWM workspace 2."
  (interactive)
  (exwm-workspace-switch-create 2))

(defun display-and-dock-setup ()
  "Configure monitors and peripherals."
  ;; Monitors (works on both my X230 and my W541)
  (let* ((connected-monitors (split-string
                              (shell-command-to-string
                               "xrandr | grep ' connected' | awk '{print $1}'")))
         (possible-monitors (if (member "LVDS-1" connected-monitors)
                                '("LVDS-1" "VGA-1")
                              (if (member "LVDS1" connected-monitors)
                                  '("LVDS1" "VGA1")
                                '("eDP-1-1" "DP-1-2-1" "DP-1-2-2"
                                  "DP-1-2-3" "VGA-1-1"))))
         (command "xrandr "))
    (dolist (monitor possible-monitors)
      (if (and (member monitor connected-monitors)
             (not (and (eq monitor "eDP-1-1")
                   (member "DP-1-2-1" connected-monitors))))
          (let* ((output (concat "--output " monitor " "))
                 (primary (when (or (eq monitor "LVDS-1")
                                   (eq monitor "eDP-1-1")
                                   (eq monitor "DP-1-2-2"))
                            "--primary "))
                 (rate (when (eq monitor "DP-1-2-2")
                         "--rate 75 "))
                 (res (concat "--mode " (if (or (eq monitor "LVDS-1")
                                               (eq monitor "VGA-1"))
                                            "1366x768 "
                                          "1920x1080 ")))
                 (rotate (when (or (eq monitor "DP-1-2-1")
                                  (eq monitor "DP-1-2-3"))
                           (concat "--rotate " (if (eq monitor "DP-1-2-1")
                                                   "left "
                                                 "right "))))
                 (pos (concat "--pos " (if (not (or (eq monitor "DP-1-2-2")
                                                 (eq monitor "DP-1-2-3")))
                                           "0x0 "
                                         (if (eq monitor "DP-1-2-2")
                                             "1080x0 "
                                           "3000x0 ")))))
            (setq command (concat command output primary rate res rotate pos)))
        (setq command (concat command "--output " monitor " --off "))))
    (start-process-shell-command
     "Display Setup" nil command))
  ;; Disable trackpad
  (start-process-shell-command
   "Disable trackpad" nil (concat "xinput disable $(xinput list | "
                                "grep Synaptics | head -n 1 | "
                                "sed -r 's/.*id=([0-9]+).*/\\1/')"))
  ;; Configure trackball
  (let* ((trackball-id (shell-command-to-string
                        (concat "xinput | grep ELECOM | head -n 1 | sed -r "
                                "'s/.*id=([0-9]+).*/\\1/' | tr '\\n' ' '"))))
    (dolist (command '("'libinput Button Scrolling Button' 10"
                       "'libinput Scroll Method Enabled' 0 0 1"))
      (start-process-shell-command
       "Trackball Setup" nil (concat "sleep 2 && xinput set-prop "
                                   trackball-id command)))
    (start-process-shell-command
     "Trackball Setup" nil (concat "sleep 2 && xinput set-button-map " trackball-id
                                 "1 2 3 4 5 6 7 8 9 2 1 2")))
  ;; Keyboard
  (start-process-shell-command
   "Keyboard Setup" nil "setxkbmap -option ctrl:nocaps"))

(add-hook 'exwm-randr-screen-change-hook 'display-and-dock-setup)
(exwm-randr-enable)

(setq exwm-floating-border-width 3
      exwm-floating-border-color (face-attribute 'mode-line :background))

(add-hook 'exwm-update-title-hook
          (lambda () (exwm-workspace-rename-buffer exwm-title)))

(setenv "_JAVA_AWT_WM_NONREPARENTING" "1")

(defun run-gimp ()
  "Start GIMP."
  (interactive)
  (start-process-shell-command
   "GIMP" nil "gimp"))

(defun run-steam ()
  "Start Steam."
  (interactive)
  (start-process-shell-command
   "Steam" nil "steam"))

(defun run-firefox ()
  "Start Firefox."
  (interactive)
  (start-process-shell-command
   "Firefox" nil "firefox"))

(defun run-discord ()
  "Start Discord."
  (interactive)
  (start-process-shell-command
   "Discord" nil "discord"))

(defun run-telegram ()
  "Start Telegram."
  (interactive)
  (start-process-shell-command
   "Telegram" nil "telegram-desktop"))

(defun run-musescore ()
  "Start MuseScore."
  (interactive)
  (start-process-shell-command
   "MuseScore" nil "musescore"))

(defun run-libreoffice ()
  "Start LibreOffice."
  (interactive)
  (start-process-shell-command
   "LibreOffice" nil "libreoffice"))

(defun run-transmission ()
  "Start Transmission."
  (interactive)
  (start-process-shell-command
   "Transmission" nil "transmission-gtk"))

(defun monitor-settings ()
  "Open arandr to configure monitors."
  (interactive)
  (start-process-shell-command
   "Monitor Settings" nil "arandr"))

(defun network-settings ()
  "Open a NetworkManager connection editor."
  (interactive)
  (start-process-shell-command
   "Connections" nil "nm-connection-editor")
  (async-shell-command "nmcli dev wifi list"))

(defun volume-settings ()
  "Open pavucontrol to adjust volume."
  (interactive)
  (start-process-shell-command
   "Volume Mixer" nil "pavucontrol"))

(defun audio-loopback ()
  "Loop desktop audio into a null sink alongside the primary input."
  (interactive)
  ;; Create two modules: `loop' and `out'
  (dolist (sink '("loop"
                  "out"))
    (shell-command (concat "pacmd load-module module-null-sink sink_name=" sink))
    (shell-command (concat "pacmd update-sink-proplist "
                           sink " device.description=" sink)))
  ;; Loop `loop' to primary output, pipe it to `out', loop primary into to `out'
  (dolist (command '("sink=out"
                     "source=loop.monitor"
                     "source=loop.monitor sink=out"))
    (shell-command (concat "pacmd load-module module-loopback " command)))
  ;; Run `pavucontrol' and then unload the modules after it completes
  (start-process-shell-command
   "Audio Loop" nil (concat "pavucontrol && "
                          "pacmd unload-module module-null-sink && "
                          "pacmd unload-module module-loopback")))

(defgroup keyboard-layout nil
  "Keyboard layouts to cycle through."
  :group 'environment)

(defcustom keyboard-layout-1 "us"
  "The first of three keyboard layouts to cycle through.
Set to nil to have one less keyboard layout."
  :group 'keyboard-layout
  :type 'string)

(defcustom keyboard-layout-2 "epo"
  "The second of three keyboard layouts to cycle through.
Set to nil to have one less keyboard layout."
  :group 'keyboard-layout
  :type 'string)

(defcustom keyboard-layout-3 "de"
  "The third of three keyboard layouts to cycle through.
Set to nil to have one less keyboard layout."
  :group 'keyboard-layout
  :type 'string)

(defun get-keyboard-layout ()
  "Get the current keyboard layout."
  (shell-command-to-string
   (concat "setxkbmap -query | "
           "grep -oP 'layout:\\s*\\K(\\w+)' | "
           "tr '\n' ' ' | sed 's/ //'")))

(defun set-keyboard-layout (&optional layout)
  "Set the keyboard layout to LAYOUT."
  (interactive)
  (let ((layout (or layout (read-string "Enter keyboard layout: "))))
    (shell-command (concat "setxkbmap " layout " -option ctrl:nocaps"))
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
  (when (yes-or-no-p "Really suspend? ")
    (shell-command "systemctl suspend -i")))

(global-set-key (kbd "C-x C-M-s") 'suspend-computer)

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

(setq exwm-input-global-keys
      `(;; Switching Workspaces
        ([?\s-q] . farl-exwm/workspace-1)
        ([?\s-w] . farl-exwm/workspace-0)
        ([?\s-e] . farl-exwm/workspace-2)

        ;; Opening X applications
        ([?\s-g]    . run-gimp)
        ([?\s-s]    . run-steam)
        ([?\s-f]    . run-firefox)
        ([?\s-d]    . run-discord)
        ([?\s-t]    . run-telegram)
        ([?\s-m]    . run-musescore)
        ([?\s-b]    . run-libreoffice)
        ([?\s-o]    . run-transmission)
        ([?\s-r]    . monitor-settings)
        ([?\s-n]    . network-settings)
        ([?\s-v]    . volume-settings)
        ([s-return] . vterm)

        ;; Other desktop environment things
        ([?\s-x]       . dmenu)
        ([menu]        . smex)
        ([?\s- ]       . cycle-keyboard-layout)
        ([s-backspace] . cycle-keyboard-layout-reverse)
        ([s-tab]       . audio-loopback)

        ;; Controlling EMMS
        ([XF86AudioNext] . emms-next)
        ([XF86AudioPrev] . emms-previous)
        ([XF86AudioPlay] . emms-pause)
        ([XF86AudioStop] . emms-stop)))

(setq exwm-input-simulation-keys
      '(;; Navigation
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

;; I can't do sequences above, so this is separate
(defun exwm-C-s ()
  "Pass C-s to the EXWM window."
  (interactive)
  (execute-kbd-macro (kbd "C-q C-s")))

(define-key exwm-mode-map (kbd "C-x C-s") 'exwm-C-s)

(define-key exwm-mode-map (kbd "C-c C-q") nil)
(define-key exwm-mode-map (kbd "C-q") 'exwm-input-send-next-key)

(dolist (key '("C-c C-t C-f"
               "C-c C-t C-v"
               "C-c C-t C-m"
               "C-c C-f"))
  (define-key exwm-mode-map (kbd key) nil))

(start-process-shell-command
 "Cursor Hiding" nil "xbanish")

(start-process-shell-command
 "Disable Blanking" nil "xset s off -dpms")

(start-process-shell-command
 "Keyboard Layout" nil "setxkbmap us -option ctrl:nocaps")

(start-process-shell-command
 "Fallback Cursor" nil "xsetroot -cursor_name left_ptr")

(start-process-shell-command
 "Compositor" nil "xcompmgr")

(exwm-enable)
(exwm-config-ido)
(exwm-systemtray-enable)

)

(when (executable-find "mpd")

(use-package emms
  :ensure t
  :defer t
  :init
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5
	emms-player-list '(emms-player-mpd)
	emms-info-functions '(emms-info mpd)
	emms-player-mpd-server-name "localhost"
	emms-player-mpd-server-port "6601"
	mpc-host "localhost:6601"))

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

(defvar emms-map
  (let ((map (make-sparse-keymap)))
    ;; Opening playlist and music browser
    (define-key map (kbd "v") 'emms)
    (define-key map (kbd "b") 'emms-smart-browse)
    ;; Track navigation
    (define-key map (kbd "n n") 'emms-next)
    (define-key map (kbd "n p") 'emms-previous)
    (define-key map (kbd "p") 'emms-pause)
    (define-key map (kbd "s") 'emms-stop)
    ;; Repeat/shuffle
    (define-key map (kbd "t C-r") 'emms-toggle-repeat-track)
    (define-key map (kbd "t r") 'emms-toggle-repeat-playlist)
    (define-key map (kbd "t s") 'farl-emms/shuffle-with-message)
    ;; Refreshing various things
    (define-key map (kbd "r c") 'emms-player-mpd-update-all-reset-cache)
    (define-key map (kbd "r d") 'mpd/update-database)
    ;; mpd specific functions
    (define-key map (kbd "d s") 'mpd/start-music-daemon)
    (define-key map (kbd "d q") 'mpd/kill-music-daemon)
    (define-key map (kbd "d u") 'mpd/update-database)
    map)
  "A keymap for controlling `emms'.")
(global-set-key (kbd "C-c a") emms-map)

)

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :init
  (require 'graphviz-dot-mode))

(use-package markdown-mode
  :ensure t
  :defer t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(dolist (key '("C-x C-z"
               "C-z"))
  (global-unset-key (kbd key)))

(setq confirm-kill-emacs 'yes-or-no-p)

(defun config-visit ()
  "Open the configuration file."
  (interactive)
  (find-file (concat user-emacs-directory "literate-emacs.org")))

(global-set-key (kbd "C-c e") 'config-visit)

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

(setq ring-bell-function 'ignore
      visible-bell t)

(use-package which-key
  :ensure t
  :defer t
  :init
  (which-key-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package ido-vertical-mode
  :ensure t
  :defer t
  :init
  (setq ido-everywhere t
        ido-max-prospects 10
        ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-create-new-buffer 'always
        ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-mode 1)
  (ido-vertical-mode 1)
  (use-package smex
    :ensure t
    :defer t
    :bind (("M-x"    . smex)
           ("<menu>" . smex))))

(defun tangle-literate-program ()
  "Tangle a file if it's a literate programming file."
  (interactive)
  (when (and (equal major-mode 'org-mode)
           (cl-search "literate" (buffer-file-name)))
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-literate-program)

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

This function has been altered to accommodate EXWM."
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

(setq make-backup-files nil
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
  :bind ("C-c g" . magit-status))

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
(setq org-confirm-babel-evaluate '(lambda (lang body) (not (eq lang "dot"))))

(dolist (shortcut
         '(("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
           ("py" "#+BEGIN_SRC python\n?\n#+END_SRC")
           ("dot" "#+BEGIN_SRC dot :file ?.png :cmdline -Kdot -Tpng\n\n#+END_SRC")
           ("t" "#+BEGIN_SRC text :tangle ?\n\n#+END_SRC")
           ("css" "#+BEGIN_SRC css\n?\n#+END_SRC")))
  (add-to-list 'org-structure-template-alist shortcut))

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

(setq calendar-week-start-day 1)
(global-set-key (kbd "C-c l") 'calendar)

(global-set-key (kbd "C-c c") 'calc)

(global-set-key (kbd "C-h 4 m") 'man)

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

(defvar games-map
  (make-sparse-keymap)
  "A keymap to which games can be added.")

(global-set-key (kbd "C-M-g") games-map)

(use-package chess
  :ensure t
  :defer t
  :bind (:map games-map
         ("c" . chess)))

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

(use-package 2048-game
  :ensure t
  :defer t
  :bind (:map games-map
         ("2" . 2048-game)))

;;; init.el ends here

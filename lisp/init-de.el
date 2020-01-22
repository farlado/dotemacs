;;; init-de.el --- Making Emacs a desktop environment

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(use-package exwm
  :ensure t
  :defer t
  :init
  (unless pdumper-dumped
    (require 'exwm)
    (require 'exwm-randr)
    (require 'exwm-config)
    (require 'exwm-systemtray))
  (setenv "_JAVA_AWT_WM_NONREPARENTING" "1"))

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
                                   ((string-match-p "[Tt]elegram" exwm-class-name)
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
                           "--mode" (if (string= "eDP1" monitor)
                                        "2880x1620"
                                      "1920x1080")
                           ;; Scale all monitor output to 3K.
                           "--scale-from" "2880x1620"
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
                                         "1620x0"
                                       "4500x0")))
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
    (dolist (command '("'libinput Button Scrolling Button' 10"
                       "'libinput Scroll Method Enabled' 0 0 1"))
      (start-process-shell-command
       "Trackball Setup" nil (concat "xinput set-prop "
                                     trackball-id command)))
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
   "Audio Loop" nil (concat "pavucontrol && "
                            "pacmd unload-module module-null-sink && "
                            "pacmd unload-module module-loopback")))

(defvar keyboard-layout-1 "us"
  "The first of three keyboard layouts to cycle through.

Set to nil to have one less keyboard layout.")

(defvar keyboard-layout-2 "epo"
  "The second of three keyboard layouts to cycle through.

Set to nil to have one less keyboard layout.")

(defvar keyboard-layout-3 "de"
  "The third of three keyboard layouts to cycle through.

Set to nil to have one less keyboard layout.")

(defun get-keyboard-layout ()
  "Get the current keyboard layout."
  (shell-command-to-string
   "setxkbmap -query | grep -oP 'layout:\\s*\\K(\\w+)' | tr '\n' ' ' | sed 's/ //'"))

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
  (and (yes-or-no-p "Really suspend? ")
       (shell-command "systemctl suspend -i")))

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

(provide 'init-de)

;;; init-de.el ends here

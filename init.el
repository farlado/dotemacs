;;; init.el --- Initializing Farlado's Illiterate GNU Emacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

(unless pdumper-dumped
  (require 'server))

(defun server-start-if-not-running ()
  "Call `server-start' if `server-running-p' returns nil."
  (unless (server-running-p)
    (server-start)))

(add-hook 'after-init-hook 'server-start-if-not-running)

(tooltip-mode -1)
(setq use-dialog-box nil
      use-file-dialog nil)

(require 'init-package)

(use-package dashboard
  :ensure t
  :defer t
  :init
  (setq dashboard-set-footer nil
        inhibit-startup-screen t
        dashboard-items '((recents . 10))
        dashboard-startup-banner 'official
        initial-buffer-choice (lambda () (or (get-buffer "*dashboard*")
                                             (get-buffer "*scratch*")))
        dashboard-banner-logo-title "Welcome to Farlado's Illiterate GNU Emacs!")
  (dashboard-setup-startup-hook))

(require 'init-looks)

(farl-init/theme)

(when window-system
  (farl-init/set-font))

(require 'init-defaults)

(require 'init-editor)

(when window-system
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (define-key org-mode-map (kbd "C-c r") 'epresent-run))

(setq org-ellipsis (if window-system "⤵" "...")
      org-hide-emphasis-markers (when window-system t))

(when (getenv "_RUN_EXWM")
  (setenv "_RUN_EXWM")
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (require 'init-de)

(dolist (var `(("XDG_CURRENT_DESKTOP" "emacs")
                ("GTK2_RC_FILES" ,(user-config-file "gtk-2.0/gtkrc"))
                ("QT_QPA_PLATFORMTHEME" "gtk2")))
  (setenv (car var) (car (last var))))

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
(exwm-systemtray-enable)

)

(when (executable-find "mpd")
  (setenv "MPD_HOST" "localhost")
  (setenv "MPD_PORT" "6601")
  (require 'init-media)
  (require 'emms-player-mpd)
  (emms-all))

(require 'init-extend)

;;; init.el ends here

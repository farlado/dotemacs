;;; init-media.el --- Making Emacs a media player

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been automatically generated from `literate-emacs.org'.
;; If you don't have a copy of that file, it is best not to use this file!
;; All relevant commentary is in `literate-emacs.org', not here.
;; There may not be any comments past this point.
;; Abandon all hope, ye who enter here.

;;; Code:

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
    ;; `mpd'-specific functions
    (define-key map (kbd "d s") 'mpd/start-music-daemon)
    (define-key map (kbd "d q") 'mpd/kill-music-daemon)
    (define-key map (kbd "d u") 'mpd/update-database)
    map)
  "A keymap for controlling `emms'.")
(global-set-key (kbd "C-c a") emms-map)

(provide 'init-media)

;;; init-media.el ends here

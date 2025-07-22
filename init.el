(setq-default dired-listing-switches "--all --color=auto --human-readable -l")

;; Set default program for opening media in dired
(setq dired-guess-shell-alist-user
      '(("\\.mkv" "mpv")
	("\\.mp4" "mpv")
	("\\.webm" "mpv")
	("\\.flac" "mpd")
	("\\.mp3" "mpd")
	("\\.ogg" "mpd")))

(set-face-attribute 'default nil :family "Monospace" :height 160)

(setq initial-scratch-message "")

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message () )

(setq epa-pinentry-mode 'loopback)

;; replace C-x C-b with ibuffer
(define-key (current-global-map) [remap list-buffers] 'ibuffer)

(defun my/focus ()
  ;; set margins
  (set-window-margins (selected-window) 25 25)
  
  ;; set fringe to white to make margin blend in
  (set-face-attribute 'fringe nil :background "white")

  ;; display relative line numbers
  (setq display-line-numbers 'relative))

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; taken from mastering emacs
;; easier to switch with 2 keys
(global-set-key (kbd "M-o") 'other-window)

;; add custom module directory to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/keenban/"))

;; load files from custom directory
(require 'keenban-denote)
(require 'keenban-edit)
(require 'keenban-git)
(require 'keenban-mail)
(require 'keenban-media)
(require 'keenban-minibuffer)
(require 'keenban-org)
(require 'keenban-prog)
(require 'keenban-irc)

;; taken from emacs from scratch
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

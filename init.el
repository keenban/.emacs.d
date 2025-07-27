(setq-default dired-listing-switches "--all --color=auto --human-readable -l")

;; Set default program for opening media in dired
(setq dired-guess-shell-alist-user
      '(("\\.mkv" "mpv")
	("\\.mp4" "mpv")
	("\\.webm" "mpv")
	("\\.flac" "mpd")
	("\\.mp3" "mpd")
	("\\.ogg" "mpd")))

(require 'consult)

;; Replace C-x b
(define-key (current-global-map) [remap switch-to-buffer] 'consult-buffer)

;; Replace C-x l
(define-key (current-global-map) [remap count-lines-page] 'consult-line)

(load-theme 'modus-vivendi-tinted)

(set-face-attribute 'default nil :height 160)

(setq initial-scratch-message "")

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message ())

(setq epa-pinentry-mode 'loopback)

(setq display-time-format "%R")
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode t)
(display-battery-mode t)

  (setq-default mode-line-format
                '("%e"
                  my-modeline-buffer-name
          	" %* "
          	my-modeline-major-mode
          	mode-line-format-right-align
  		my-modeline-misc))

  (defvar-local my-modeline-misc
      '(:eval
        (when (mode-line-window-selected-p)
  	mode-line-misc-info)))

  (put 'my-modeline-misc 'risky-local-variable t)

  (put 'my-modeline-time 'risky-local-variable t)

  (defvar-local my-modeline-buffer-name
      '(:eval
        (when (mode-line-window-selected-p)
          (propertize
  	 (format " %s " (buffer-name))
  	 'face 'my-modeline-face))))

  (put 'my-modeline-buffer-name 'risky-local-variable t)

  (defvar-local my-modeline-major-mode
    '(:eval (propertize 
             (capitalize 
              (replace-regexp-in-string "-mode$" "" 
               (symbol-name major-mode)))
             'face 'bold)))

  (put 'my-modeline-major-mode 'risky-local-variable t)

  (setq mode-line-right-align-edge 'right-fringe)

  (defface my-modeline-face
    '((t :background "#5f509f" :foreground "white" :inherit bold :box "000000"))
    "Face with a lavender background for use on the mode line.")

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; taken from mastering emacs
;; easier to switch with 2 keys
(global-set-key (kbd "M-o") 'other-window)

;; replace C-x C-b with ibuffer
(define-key (current-global-map) [remap list-buffers] 'ibuffer)

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

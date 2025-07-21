(setq-default dired-listing-switches "--all --color=auto --human-readable -l")

(set-face-attribute 'default nil :family "Monospace")
(set-face-attribute 'default nil :height 160)

(setq initial-scratch-message "")

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message () )

(setq epa-pinentry-mode 'loopback)

;; replace C-x C-b with ibuffer
(define-key (current-global-map) [remap list-buffers] 'ibuffer)

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
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

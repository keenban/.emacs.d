(setq-default dired-listing-switches "--all --color=auto --human-readable -l")

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; taken from mastering emacs
(global-set-key (kbd "M-o") 'other-window)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-c r") 'eval-region)

(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

(set-frame-font "JetBrains Mono 16" nil t)

;; add custom module directory to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/keenban/"))

;; load files from custom directory
(require 'keenban-hyperbole)
(require 'keenban-eat)
(require 'keenban-emms)
(require 'keenban-markdown)
(require 'keenban-yasnippet)
(require 'keenban-vertico)
(require 'keenban-magit)
(require 'keenban-which)
(require 'keenban-org)
(require 'keenban-denote)
(require 'keenban-mu4e)
(require 'keenban-eww)
(require 'keenban-edit)
(require 'keenban-gnus)
(require 'keenban-minibuffer)
(require 'keenban-scroll)

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

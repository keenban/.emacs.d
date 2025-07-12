;; taken from mastering emacs
(global-set-key (kbd "M-o") 'other-window)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-c r") 'eval-region)

(setq-default dired-listing-switches "--all --color=auto --human-readable -l")
(setq left-margin-width 5)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(set-frame-font "JetBrains Mono 16" nil t)

(column-number-mode)

;; add custom module directory to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/keenban/"))

;; load files from custom directory
(require 'keenban-hyperbole)
(require 'keenban-eat)
(require 'keenban-markdown-mode)
(require 'keenban-yasnippet)
(require 'keenban-vertico)
(require 'keenban-magit)
(require 'keenban-which-key)
(require 'keenban-org)
(require 'keenban-denote)
(require 'keenban-mu4e)
(require 'keenban-eww)
(require 'keenban-quick-edit)

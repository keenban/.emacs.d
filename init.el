;; -*- lexical-binding: t; -*-
(require 'emms)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native))
(emms-all)

(add-to-list 'erc-modules 'services)
(erc-update-modules)
(setq erc-prompt-for-nickserv-password nil
      erc-use-auth-source-for-nickserv-password t
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-autojoin-channels-alist '((Libera.Chat "#emacs" "#erc")))

(setq org-hide-leading-stars t
      org-insert-heading-respect-content t
      org-log-done t
      org-agenda-files '("~/media/doc/notes/20250707T180240--agenda__agenda.org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(define-abbrev org-mode-abbrev-table "kel" "#+BEGIN_SRC emacs-lisp")
(define-abbrev org-mode-abbrev-table "kend" "#+END_SRC")

(require 'denote)
(global-set-key (kbd "C-c n n") 'denote)
(global-set-key (kbd "C-c n r") 'denote-rename-file)
(global-set-key (kbd "C-c n g") 'denote-grep)
(global-set-key (kbd "C-c n d") 'denote-dired)
(setq denote-directory (expand-file-name "~/media/doc/notes/"))
(add-hook 'after-init-hook #'denote-rename-buffer-mode)

(require 'vertico)
(add-hook 'after-init-hook #'vertico-mode)

(require 'marginalia)
(add-hook 'after-init-hook #'marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides nil)

(require 'magit)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq dired-listing-switches "--all --color=auto --human-readable -l"
      dired-guess-shell-alist-user '(("\\.mkv" "mpv")
  				     ("\\.mp4" "mpv")
  				     ("\\.webm" "mpv")
  				     ("\\.flac" "mpd")
  				     ("\\.mp3" "mpd")
  				     ("\\.ogg" "mpd")))

(defun my/irc-ggn () 
  (interactive) 
  (erc-tls :server "irc.gazellegames.net" :port 7000))

(defun my/irc-libera ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697))

(global-set-key (kbd "C-c i g") #'my/irc-ggn)
(global-set-key (kbd "C-c i l") #'my/irc-libera)

(defun my/focus () (interactive)
  (set-window-margins (selected-window) 25 25)
  (set-face-attribute 'fringe nil :background "white")
  (setq display-line-numbers 'relative))

(define-key (current-global-map) [remap list-buffers] 'ibuffer)
(add-hook 'after-init-hook #'which-key-mode)
(add-hook 'after-init-hook #'savehist-mode)
(setq epa-pinentry-mode 'loopback)

(set-face-attribute 'default nil :family "Monospace" :height 160)
(set-face-attribute 'line-number nil :foreground "grey75")
(set-face-attribute 'org-document-title nil :height 1.75 :weight 'heavy)
(set-face-attribute 'org-hide nil :height 1.5)
(set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.375 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.25 :weight 'bold)
(set-face-attribute 'org-level-4 nil :height 1.125 :weight 'bold)

(defun open-init-file () (interactive) (find-file user-init-file))
(defun open-xinitrc () (interactive) (find-file "~/.xinitrc"))
(defun open-bashrc () (interactive) (find-file "~/.bashrc"))

(global-set-key (kbd "C-c e i") 'open-init-file)
(global-set-key (kbd "C-c e x") 'open-xinitrc)
(global-set-key (kbd "C-c e b") 'open-bashrc)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

(global-set-key (kbd "M-o") 'other-window)

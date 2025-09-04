(setq initial-scratch-message "")
(defun display-startup-echo-area-message ())

(setq epa-pinentry-mode 'loopback)

(setq display-time-format "%R")
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode t)
(display-battery-mode t)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

(use-package emms
  :ensure t
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
	  emms-info-functions '(emms-info-native)))

(setq-default dired-listing-switches "--all --color=auto --human-readable -l")

(setq dired-guess-shell-alist-user
      '(("\\.mkv" "mpv")
	("\\.mp4" "mpv")
	("\\.webm" "mpv")
	("\\.flac" "mpd")
	("\\.mp3" "mpd")
	("\\.ogg" "mpd")))

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq
   org-startup-folded t
   org-insert-heading-respect-content t
   org-hide-leading-stars t
   org-log-done t)

  (set-face-attribute 'org-document-title nil :height 1.75 :weight 'heavy)
  (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.375 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.25 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :height 1.125 :weight 'bold)
  (set-face-attribute 'org-level-5 nil :weight 'bold)

  (setq org-agenda-files '("~/media/doc/notes/20250707T180240--agenda__agenda_important_todo.org"))

    (define-abbrev org-mode-abbrev-table "myel" "#+BEGIN_SRC emacs-lisp")
  (define-abbrev org-mode-abbrev-table "mysh" "#+BEGIN_SRC shell")
  (define-abbrev org-mode-abbrev-table "myend" "#+END_SRC"))

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/media/doc/notes/"))
  (setq denote-rename-confirmations nil)
  (denote-rename-buffer-mode 1))

(use-package denote-journal
  :ensure t
  :commands ( denote-journal-new-entry
	      denote-journal-new-or-existing-entry
	      denote-journal-link-or-create-entry )
  :hook (calendar-mode . denote-journal-calendar-mode)
  :bind
  (("C-c n j" . denote-journal-new-or-existing-entry))
  :config
  (setq denote-journal-directory
	(expand-file-name "journal" denote-directory))
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

(use-package magit
  :ensure t)

(use-package nov
  :vc (:url "https://depp.brause.cc/nov.el.git")
  :config
  (setq nov-text-width 70)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package consult
  :ensure t
  :config
  (define-key (current-global-map) [remap switch-to-buffer] 'consult-buffer)
  (define-key (current-global-map) [remap count-lines-page] 'consult-line))

(define-key (current-global-map) [remap list-buffers] 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)

(setq ispell-program-name "/usr/bin/aspell")

(use-package modus-themes
  :config
  (setq modus-themes-headings
	'((0 . (variable-pitch bold 1.75))
	  (1 . (variable-pitch bold 1.5))
	  (1 . (variable-pitch bold 1.375))
	  (1 . (variable-pitch bold 1.25))
	  (1 . (variable-pitch bold 1.125))
          (2 . (bold))))
  (load-theme 'modus-vivendi-tinted))

(set-face-attribute 'default nil :height 160)

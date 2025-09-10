(setq epa-pinentry-mode 'loopback)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(set-face-attribute 'default nil :height 160)

(use-package modus-themes
  :ensure nil
  :init
  (setq modus-vivendi-tinted-palette-overrides
	'((fg-heading-0 magenta-cooler)
	  (fg-heading-1 magenta-cooler)
	  (fg-heading-2 magenta-cooler)
	  (fg-heading-3 magenta-cooler)
	  (fg-heading-4 magenta-cooler)
	  (fg-heading-5 magenta-cooler)
	  (fg-heading-6 magenta-cooler)
	  (fg-heading-7 magenta-cooler)
	  (fg-heading-8 magenta-cooler)))
  (setq modus-themes-headings
	(quote ((0 . (1.75))
		(1 . (1.5))
		(2 . (1.375))
		(3 . (1.25))
		(t . (1.125)))))
  :config
  (load-theme 'modus-vivendi-tinted t))

(global-set-key (kbd "M-o") 'other-window)

(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'which-key-mode)

(setq-default dired-listing-switches "--all --color=auto --human-readable -l")

(require 'use-package)
(setq use-package-always-ensure t)

(use-package vertico
  :hook after-init)

(use-package marginalia
  :hook after-init)

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package consult
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap count-lines-page] . consult-line)))


(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :config
  (setq org-startup-folded t
        org-insert-heading-respect-content t
        org-hide-leading-stars t
        org-log-done t
        org-agenda-files '("~/media/doc/notes/20250707T180240--agenda.org")))

(use-package org-capture
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
	'(("f" "Fleeting thoughts and ideas" entry
	   (file+headline "~/media/doc/notes/20250909T140227--inbox__important.org" "Fleeting")
	   "* %^{Title}\n:PROPERTIES:\n:CAPTURED: %U\n:CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n:END:\n\n%?"
	   :empty-lines-after 1))))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind (("C-c n n" . denote)
         ("C-c n r" . denote-rename-file)
         ("C-c n l" . denote-link)
         ("C-c n b" . denote-backlinks)
         ("C-c n d" . denote-dired)
         ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/media/doc/notes/")
        denote-rename-confirmations nil)
  (denote-rename-buffer-mode 1))

(use-package denote-journal
  :commands (denote-journal-new-entry
             denote-journal-new-or-existing-entry
             denote-journal-link-or-create-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :bind (("C-c n j" . denote-journal-new-or-existing-entry))
  :config
  (setq denote-journal-directory (expand-file-name "journal" denote-directory)
        denote-journal-keyword "journal"
        denote-journal-title-format 'day-date-month-year))

(use-package magit)

(use-package emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-native)))

(use-package nov
  :vc (:url "https://depp.brause.cc/nov.el.git")
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 70))



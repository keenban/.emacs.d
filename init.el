(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun ensure-package-installed (&rest packages)
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(ensure-package-installed
 'vertico 'marginalia 'orderless 'consult
 'magit 'emms 'denote 'denote-journal)

(unless (package-installed-p 'nov)
  (package-vc-install "https://depp.brause.cc/nov.el.git"))

(setq custom-file
      (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq epa-pinentry-mode 'loopback)

(set-face-attribute 'default nil :height 160)
(require-theme 'modus-themes)
(setq modus-themes-mixed-fonts t modus-themes-prompts
      '(italic bold))
(setq modus-themes-headings
      '((0 1.75) (1 1.5) (2 1.375) (3 1.25) (t 1.125)))
(setq modus-vivendi-tinted-palette-overrides
      '((bg-mode-line-active bg-lavender)
	(bg-paren-match bg-magenta-intense)
	(bg-prose-block-contents bg-dim)
	(bg-prose-block-delimiter bg-dim)
	(fg-prose-block-delimiter fg-main)
	(underline-err red-faint)
	(underline-warning yellow-faint)
	(underline-note cyan-faint)
	(fg-heading-0 magenta-cooler)
	(fg-heading-1 magenta-cooler)
	(fg-heading-2 magenta-cooler)
	(fg-heading-3 magenta-cooler)
	(fg-heading-4 magenta-cooler)
	(fg-heading-5 magenta-cooler)
	(fg-heading-6 magenta-cooler)
	(fg-heading-7 magenta-cooler)
	(fg-heading-8 magenta-cooler)
	(comment yellow-faint) (string green-warmer)))
(load-theme 'modus-vivendi-tinted t)

(require 'savehist)
(add-hook 'after-init-hook #'savehist-mode)

(require 'which-key)
(add-hook 'after-init-hook #'which-key-mode)

(require 'vertico)
(add-hook 'after-init-hook #'vertico-mode)

(require 'marginalia)
(add-hook 'after-init-hook #'marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides nil)

(require 'consult)
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap count-lines-page] 'consult-line)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-startup-folded t org-startup-indented t
      org-insert-heading-respect-content t
      org-hide-leading-stars t org-log-done t
      org-agenda-files
      '("~/media/doc/notes/20250707T180240--agenda.org"))
(setq org-capture-templates
'(("f" "Fleeting thoughts and ideas" entry
   (file+headline
    "~/media/doc/notes/20250909T140227--inbox__important.org"
    "Fleeting")
   "* %^{Title}\n:PROPERTIES:\n:CAPTURED: %U\n:CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n:END:\n\n%?"
   :empty-lines-after 1)))

(require 'denote)
(add-hook 'dired-mode-hook 'denote-dired-mode)
(global-set-key (kbd "C-c n n") 'denote)
(global-set-key (kbd "C-c n r") 'denote-rename-file)
(global-set-key (kbd "C-c n l") 'denote-link)
(global-set-key (kbd "C-c n b") 'denote-backlinks)
(global-set-key (kbd "C-c n d") 'denote-dired)
(global-set-key (kbd "C-c n g") 'denote-grep)
(setq denote-directory (expand-file-name "~/media/doc/notes/")
      denote-rename-confirmations nil)
(denote-rename-buffer-mode 1)

(require 'denote-journal)
(add-hook 'calendar-mode-hook 'denote-journal-calendar-mode)
(global-set-key (kbd "C-c n j") 'denote-journal-new-or-existing-entry)
(setq denote-journal-directory (expand-file-name "journal" denote-directory)
      denote-journal-keyword "journal"
      denote-journal-title-format 'day-date-month-year)

(require 'magit)

(require 'emms-setup)
(emms-all)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native))

(setq nov-text-width 70)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 70)

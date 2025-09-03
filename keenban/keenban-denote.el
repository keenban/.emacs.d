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

(use-package denote-org
  :ensure t)

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

(provide 'keenban-denote)

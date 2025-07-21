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
  (set-face-attribute 'org-level-1 nil :height 240)
  (set-face-attribute 'org-level-2 nil :height 220)
  (set-face-attribute 'org-level-3 nil :height 200)
  (set-face-attribute 'org-level-4 nil :height 180)
  (setq denote-directory (expand-file-name "~/media/doc/notes/"))
  (denote-rename-buffer-mode 1))
(use-package denote-org
  :ensure t)

(provide 'keenban-denote)

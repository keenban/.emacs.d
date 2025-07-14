(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq
   ;; start collapsed
   org-startup-folded t

   ;; respect headings
   org-insert-heading-respect-content t)

   (setq org-log-done t)
   (setq org-agenda-files '("~/media/doc/notes/20250707T180240--agenda__agenda.org"))
   (define-abbrev org-mode-abbrev-table ";el" "#+BEGIN_SRC emacs-lisp")
   (define-abbrev org-mode-abbrev-table ";end" "#+END_SRC"))

  (provide 'keenban-org)

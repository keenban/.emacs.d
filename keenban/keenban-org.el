(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-agenda-files '("~/media/doc/notes/20250707T180240--agenda__agenda.org"))
  (define-abbrev org-mode-abbrev-table ";el" "#+BEGIN_SRC emacs-lisp")
  (define-abbrev org-mode-abbrev-table ";end" "#+END_SRC"))

(provide 'keenban-org)

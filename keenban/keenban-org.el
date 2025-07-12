(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-agenda-files '("~/media/doc/notes/20250707T180240--agenda__agenda.org")))

(provide 'keenban-org)

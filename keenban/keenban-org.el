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

  ;; edit header faces
  (set-face-attribute 'org-document-title nil :height 1.75 :weight 'heavy)
  (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.375 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.25 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :height 1.125 :weight 'bold)

  ;; hide leading stars in header
  (setq org-hide-leading-stars t)

  (setq org-log-done t)
  (setq org-agenda-files '("~/media/doc/notes/20250707T180240--agenda__agenda.org"))
  (define-abbrev org-mode-abbrev-table "kel" "#+BEGIN_SRC emacs-lisp")
  (define-abbrev org-mode-abbrev-table "kend" "#+END_SRC"))

(provide 'keenban-org)

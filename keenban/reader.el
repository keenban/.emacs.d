(use-package nov
  :vc (:url "https://depp.brause.cc/nov.el.git")
  :config
  (setq nov-text-width 70)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package emms
  :ensure t
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
	  emms-info-functions '(emms-info-native)))

(provide 'keenban-emms)

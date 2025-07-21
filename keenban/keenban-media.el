(use-package emms
  :ensure t
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
	  emms-info-functions '(emms-info-native))
  (setq dired-guess-shell-alist-user
	  '(("\\.mkv" "mpv")))

(provide 'keenban-media)

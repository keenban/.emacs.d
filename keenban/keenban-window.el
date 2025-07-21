(use-package exwm
  ;; Do not try to install EXWM if it is not found.
  :ensure nil
  
  :config
  ;; Set the initial workspace number.
  (setq exwm-workspace-number 1)

  ;; Make class name the buffer name.
  (add-hook 'exwm-update-class-hook
	    (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Passthrough some keys to Emacs
  (setq exwm-input-prefix-keys
	'(?\C-x
	  ?\C-u
	  ?\C-h
	  ?\M-x
	  ?\M-`
	  ?\M-&
	  ?\M-:
	  ?\C-\M-j  ;; Buffer list
	  ?\C-\ ))  ;; Ctrl+Space
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Define workspace management keys
  (setq exwm-input-global-keys
	`(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
	  ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
	  ([?\s-&] . (lambda (cmd) ;; s-&: Launch application.
			 (interactive (list (read-shell-command "$ ")))
			 (start-process-shell-command cmd nil cmd)))
	  ([?\s-f] . (lambda () ;; s-f: Launch firefox.
			 (interactive)
			 (start-process-shell-command "firefox-bin" nil "firefox-bin")))
	  ([?\s-t] . (lambda () ;; s-t: Launch terminal.
			 (interactive)
			 (start-process-shell-command "st" nil "st")))
	    ;; s-N: Switch to certain workspace.
	    ,@(mapcar (lambda (i)
		`(,(kbd (format "s-%d" i)) .
	      (lambda ()
				  (interactive)
		(exwm-workspace-switch-create ,i))))
	      (number-sequence 0 9))))
  
  ;; Define brightness adjustment keys.
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
		(lambda () (interactive) (shell-command "xbacklight -dec 5")))
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
		(lambda () (interactive) (shell-command "xbacklight -inc 5")))

  ;; Define volume adjustment keys.
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
		(lambda () (interactive) (shell-command "pactl set-sink-volume @DEFAULT_SINK@ -5%")))
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
		(lambda () (interactive) (shell-command "pactl set-sink-volume @DEFAULT_SINK@ +5%")))
  (exwm-input-set-key (kbd "<XF86AudioMute>")
		(lambda () (interactive) (shell-command "pactl set-sink-mute @DEFAULT_SINK@ toggle")))

  ;; Start EXWM.
  (exwm-wm-mode))

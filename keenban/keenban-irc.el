(use-package erc
  :custom
  ;; Protect me from accidentally sending excess lines.
  (erc-inhibit-multiline-input t)
  (erc-send-whitespace-lines t)
  (erc-ask-about-multiline-input t)
  ;; Reconnect automatically using a fancy strategy.
  (erc-server-reconnect-function #'erc-server-delayed-check-reconnect)
  (erc-server-reconnect-timeout 30)
  
  ;; Show new buffers in the current window instead of a split.
  (erc-interactive-display 'buffer)

  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  
  ;; use authentication
  (add-to-list 'erc-modules 'services)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-use-auth-source-for-nickserv-password t)
  (add-to-list 'erc-nickserv-alist '(GGn nil nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))
  
  (defun irc-join-ggn ()
    (interactive)
    (erc-tls :server "irc.gazellegames.net" :port +7000 :nick "keenban"))

  (defun irc-join-red ()
    (interactive)
    (erc-tls :server "irc.scratch-network.net" :port +7000 :nick "keenban"))

  (defun irc-join-mam ()
    (interactive)
    (erc-tls :server "irc.myanonamouse.net" :port 6697 :nick "keenban"))

  :bind
  (("C-c i g" . irc-join-ggn)
   ("C-c i r" . irc-join-red)
   ("C-c i m" . irc-join-mam)))

(provide 'keenban-irc)

(defun tmp/irc-login (network nick)
  (message "hook has been called")
  (when (eq network 'server)
    (message "when has passed")))

(add-hook 'erc-nickserv-identified-hook 'tmp/irc-login)

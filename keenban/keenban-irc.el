(require 'erc)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; use services for authentication
(require 'erc-services)
(add-to-list 'erc-modules 'services)

;; use nickserv automatically, with credentials loaded from ~/.authinfo.gpg
(setq erc-prompt-for-nickserv-password nil)
(setq erc-use-auth-source-for-nickserv-password t)
(add-to-list 'erc-nickserv-alist '(GGn "NickServ!services@services.gazellegames.net" nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))

(defun my/irc-login (network nick)
  (when (eq network 'GGn)
    ;; need to get password decrypted 
    (erc-server-send (format "/msg Vertico ENTER #gazellegames %s" nick))))

(add-hook 'erc-nickserv-identified-hook 'my/irc-login)

(defun irc-join-ggn ()
  (interactive)
  (erc-tls :server "irc.gazellegames.net" :port +7000 :nick "keenban"))

(defun irc-join-red ()
  (interactive)
  (erc-tls :server "irc.scratch-network.net" :port +7000 :nick "keenban"))

(defun irc-join-mam ()
  (interactive)
  (erc-tls :server "irc.myanonamouse.net" :port 6697 :nick "keenban"))

(provide 'keenban-irc)

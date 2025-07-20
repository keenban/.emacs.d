;; basic requirements for irc, and credential authentication
(require 'erc)
(require 'erc-services)
(require 'auth-source)

;; Use services for authentication
(add-to-list 'erc-modules 'services)
(erc-update-modules)

;; disable nickserv prompt
(setq erc-prompt-for-nickserv-password nil)

;; use ~/.authsource.gpg for credentials
(setq erc-use-auth-source-for-nickserv-password t)

;; recognize GGn NickServ bot
(add-to-list 'erc-nickserv-alist 
                 '(GGn "NickServ!services@services.gazellegames.net" nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))

(defun my/irc-login (network nick)
  "Send ENTER command to Vertigo after successful NickServ identification."
  (when (eq network 'GGn)
    (let* ((creds (auth-source-search :host "irc.gazellegames.net"
                                      :user "Vertigo"
                                      :require '(:secret)
                                      :max 1))
           (secret (when creds (plist-get (car creds) :secret))))
      (when secret
        (erc-server-send 
         (format "PRIVMSG Vertigo :ENTER #gazellegames %s %s" 
                 nick 
                 (if (functionp secret) (funcall secret) secret)))))))

(add-hook 'erc-nickserv-identified-hook #'my/irc-login)

;; hide these messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; define user function to join server
(defun my/irc-ggn () 
  (interactive) 
  (erc-tls :server "irc.gazellegames.net" :port 7000))

(defun my/irc-libera ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697))

;; set key binding
(global-set-key (kbd "C-c i g") #'irc-ggn)
(global-set-key (kbd "C-c i l") #'irc-libera)

(provide 'keenban-irc)

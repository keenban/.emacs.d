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
                 '(Network "NickServ!services@services.network.net" nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))

(defun my/irc-login (network nick)
  "Send ENTER command to Vertigo after successful NickServ identification."
  (when (eq network 'Network)
    (let* ((creds (auth-source-search :host "irc.network.net"
                                      :user "Robot"
                                      :require '(:secret)
                                      :max 1))
           (secret (when creds (plist-get (car creds) :secret))))
      (when secret
        (erc-server-send 
         (format "PRIVMSG Robot :ENTER #channel %s %s" 
                 nick 
                 (if (functionp secret) (funcall secret) secret)))))))

(add-hook 'erc-nickserv-identified-hook #'my/irc-login)

;; hide these messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; define user function to join server
(defun my/irc-network () 
  (interactive) 
  (erc-tls :server "irc.network.net" :port 7000))

(defun my/irc-libera ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697))

;; set key binding
(global-set-key (kbd "C-c i g") #'my/irc-network)
(global-set-key (kbd "C-c i l") #'my/irc-libera)

(setq erc-autojoin-channels-alist '((Libera.Chat "#emacs" "#erc")))

(provide 'keenban-irc)

(require 'erc)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; use services for authentication
(require 'erc-services)
(add-to-list 'erc-modules 'services)

;; use nickserv automatically, with credentials loaded from ~/.authinfo.gpg
(setq erc-prompt-for-nickserv-password nil)
(setq erc-use-auth-source-for-nickserv-password t)
(add-to-list 'erc-nickserv-alist '(site "NickServ!services@services.site.net" nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))

(defun my/irc-login (network nick)
  (when (eq network 'site)
    ;; need to get password decrypted 
    (erc-server-send (format "/msg Bot ENTER #channel %s" nick))))

(add-hook 'erc-nickserv-identified-hook 'my/irc-login)

(provide 'keenban-irc)

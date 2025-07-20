(require 'erc)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; use services for authentication
(require 'erc-services)
(add-to-list 'erc-modules 'services)

;; use nickserv automatically, with credentials loaded from ~/.authinfo.gpg
(setq erc-prompt-for-nickserv-password nil)
(setq erc-use-auth-source-for-nickserv-password t)
(add-to-list 'erc-nickserv-alist '(GGn "NickServ!services@services.gazellegames.net" nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))

(defun tmp/irc-login (network nick)
  (message "hook has been called")
  (when (eq network 'GGn)
    (message "when has passed")))

(add-hook 'erc-nickserv-identified-hook 'tmp/irc-login)

(provide 'keenban-irc)

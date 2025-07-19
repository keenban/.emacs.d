(require 'erc)
(require 'erc-services)

;; hide annoying messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; use authentication
(add-to-list 'erc-modules 'services)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-use-auth-source-for-nickserv-password t)
(add-to-list 'erc-nickserv-alist '(GGn "services@services.gazellegames.net" nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))

(defun tmp/irc-login (network nick)
  (message "hook has been called")
  (when (eq network 'GGn)
    (message "when has passed")))

(add-hook 'erc-nickserv-identified-hook 'tmp/irc-login)

(provide 'keenban-irc)

;;; emms
(require 'emms)
(setq emms-player-list '(emms-player-mpv))
(setq emms-info-functions '(emms-info-native))
(emms-all)

;;; dired
(setq-default dired-listing-switches "--all --color=auto --human-readable -l")
(setq dired-guess-shell-alist-user
      '(("\\.mkv" "mpv")
  	("\\.mp4" "mpv")
  	("\\.webm" "mpv")
  	("\\.flac" "mpd")
  	("\\.mp3" "mpd")
  	("\\.ogg" "mpd")))

;;; erc
(require 'erc)
(require 'erc-services)
(require 'auth-source)
(add-to-list 'erc-modules 'services)
(erc-update-modules)

;;;; authentication
(setq erc-prompt-for-nickserv-password nil)
(setq erc-use-auth-source-for-nickserv-password t)

(add-to-list 'erc-nickserv-alist 
             '(GGn "NickServ!services@services.gazellegames.net" nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))

;;;; login
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


;;;; join
(defun my/irc-ggn () 
  (interactive) 
  (erc-tls :server "irc.gazellegames.net" :port 7000))

(defun my/irc-libera ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697))

;;;; keybinds
(global-set-key (kbd "C-c i g") #'my/irc-ggn)
(global-set-key (kbd "C-c i l") #'my/irc-libera)

;;;; miscellaneous
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-autojoin-channels-alist '((Libera.Chat "#emacs" "#erc")))

;;; org
(require 'org)

;;;; faces
(set-face-attribute 'org-document-title nil :height 1.75 :weight 'heavy)
(set-face-attribute 'org-hide nil :height 1.5)
(set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.375 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.25 :weight 'bold)
(set-face-attribute 'org-level-4 nil :height 1.125 :weight 'bold)

;;;; miscellaneous
(setq org-hide-leading-stars t)
(setq org-insert-heading-respect-content t)
(setq org-log-done t)
(setq org-agenda-files '("~/media/doc/notes/20250707T180240--agenda__agenda.org"))

;;;; abbreviations
(define-abbrev org-mode-abbrev-table "kel" "#+BEGIN_SRC emacs-lisp")
(define-abbrev org-mode-abbrev-table "kpy" "#+BEGIN_SRC python")
(define-abbrev org-mode-abbrev-table "kmd" "#+BEGIN_SRC markdown")
(define-abbrev org-mode-abbrev-table "ksrc" "#+BEGIN_SRC")
(define-abbrev org-mode-abbrev-table "kend" "#+END_SRC")

;;;; keybinds
(global-set-key (kbd "C-c a") 'org-agenda)

;;; denote
(require 'denote-org)
(require 'denote)

(global-set-key (kbd "C-c n n") 'denote)
(global-set-key (kbd "C-c n r") 'denote-rename-file)
(global-set-key (kbd "C-c n g") 'denote-grep)
(global-set-key (kbd "C-c n d") 'denote-dired)

(setq denote-directory (expand-file-name "~/media/doc/notes/"))

(add-hook 'after-init-hook #'denote-rename-buffer-mode)

;;; minibuffer
;; taken from protesilaos
;;;; vertico
(require 'vertico)
(add-hook 'after-init-hook #'vertico-mode)

;;;; marginalia
(require 'marginalia)
(add-hook 'after-init-hook #'marginalia-mode)

;;;; orderless
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides nil)

;;;; savehist
(require 'savehist)
(add-hook 'after-init-hook #'savehist-mode)

;;; which key
(require 'which-key)
(add-hook 'after-init-hook #'which-key-mode)

;;; magit
(require 'magit)

;;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; quick edit
(defun open-init-file () (interactive) (find-file user-init-file))
(defun open-xinitrc () (interactive) (find-file "~/.xinitrc"))
(defun open-bashrc () (interactive) (find-file "~/.bashrc"))

(global-set-key (kbd "C-c e i") 'open-init-file)
(global-set-key (kbd "C-c e x") 'open-xinitrc)
(global-set-key (kbd "C-c e b") 'open-bashrc)


;;; faces
(set-face-attribute 'default nil :family "Monospace" :height 160)
(set-face-attribute 'line-number nil :foreground "grey75")

;;; pinentry
(setq epa-pinentry-mode 'loopback)

;;; focus mode
(defun my/focus ()
(interactive)
  ;; set margins
  (set-window-margins (selected-window) 25 25)
  
  ;; set fringe to white to make margin blend in
  (set-face-attribute 'fringe nil :background "white")

  ;; display relative line numbers
  (setq display-line-numbers 'relative))


;;; miscellaneous
(define-key (current-global-map) [remap list-buffers] 'ibuffer)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

(global-set-key (kbd "M-o") 'other-window)

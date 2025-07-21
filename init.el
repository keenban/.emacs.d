;;   At times, I like to launch directly into Emacs when I start my graphical session.
;; This is where EXWM comes into play. I can start my X Server and load into Emacs.
;; This gives me the power of controlling other windows (terminal emulator, signal-desktop,
;; and web browser) through the unified interface of Emacs. It comes with the additional
;; benefit of a shared kill ring across all of my programs.

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

;; emms configuration for using emacs to play with mpv and mpd
(use-package emms
  :ensure t
  :defer t
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native))
  :config
  (emms-all)

;;   Here lays my configuration of the default file manager in Emacs: Dired.

;; I prefer Dired to show sizes in a human readable format (128K) rather than in bytes.
(setq-default dired-listing-switches "--all --color=auto --human-readable -l")

;;When using dired, I like to be able to open media files directly with mpv or mpd.
;; Set default program for opening media in dired
(setq dired-guess-shell-alist-user
      '(("\\.mkv" "mpv")
  	("\\.mp4" "mpv")
  	("\\.webm" "mpv")
  	("\\.flac" "mpd")
  	("\\.mp3" "mpd")
  	("\\.ogg" "mpd")))

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"

  :config
  ;; this is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail/gmail")

  ;; set folders
  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")

  ;; mailbox shortcuts
  (setq mu4e-maildir-shortcuts
	'(("/Inbox"             . ?i)
	  ("/[Gmail]/Sent Mail" . ?s)
	  ("/[Gmail]/Trash"     . ?t)
	  ("/[Gmail]/Drafts"    . ?d)
	  ("/[Gmail]/All Mail"  . ?a)))
  :bind
  ("C-c m" . mu4e))

;; This section contains all of the configuration for my Emacs IRC environment.

;; Here we will require "erc", one of two built-in IRC clients in Emacs.
;; In order to utilize the automatic nickserv identification fucntionalities,
;; we will have to bring in erc-services as an erc module.
;; Further, we should require auth-source in order to read encrypted credentials
;; that may be needed by other IRC bots.

;; basic requirements for irc, and credential authentication
(require 'erc)
(require 'erc-services)
(require 'auth-source)

;; Use services for authentication
(add-to-list 'erc-modules 'services)
(erc-update-modules)

;; We will now configure the automatic NickServ identification,
;; and allow it to recognize various formats provided by different servers.


;; disable nickserv prompt
(setq erc-prompt-for-nickserv-password nil)

;; use ~/.authsource.gpg for credentials
(setq erc-use-auth-source-for-nickserv-password t)

;; recognize GGn NickServ bot
(add-to-list 'erc-nickserv-alist 
             '(GGn "NickServ!services@services.gazellegames.net" nil "NickServ" "IDENTIFY" nil nil "Password accepted - you are now recognized."))

;; Some IRC Servers contain additional verification past NickServ.
;; This allows any further IRC bot to be authenticated with
;; credentials loaded from ~/.authinfo.gpg

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

;; Messages about users entering and leaving the chat tend to be
;; bothersome to me. I prefer to hide the messages entirely.

;; hide these messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; I like to join specific servers regularly. In order
;; to make it easier for myself, I like to set keybinds and
;; define functions to connect to the server.

;; define user function to join server
(defun my/irc-ggn () 
  (interactive) 
  (erc-tls :server "irc.gazellegames.net" :port 7000))

(defun my/irc-libera ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697))

;; set key binding
(global-set-key (kbd "C-c i g") #'my/irc-ggn)
(global-set-key (kbd "C-c i l") #'my/irc-libera)

;; When I join servers, I would like to automatically enter
;; the channels that are relevant to me.

(setq erc-autojoin-channels-alist '((Libera.Chat "#emacs" "#erc")))

;; Org mode is the apex of human invention. Here is my configuration.

(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq
   ;; start collapsed
   org-startup-folded t

   ;; respect headings
   org-insert-heading-respect-content t)

  ;; edit header faces
  (set-face-attribute 'org-document-title nil :height 1.75 :weight 'heavy)
  (set-face-attribute 'org-hide nil :height 1.5)
  (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.375 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.25 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :height 1.125 :weight 'bold)

  ;; hide leading stars in header
  (setq org-hide-leading-stars t)

  (setq org-log-done t)
  (setq org-agenda-files '("~/media/doc/notes/20250707T180240--agenda__agenda.org"))
  (define-abbrev org-mode-abbrev-table "kel" "#+BEGIN_SRC emacs-lisp")
  (define-abbrev org-mode-abbrev-table "kend" "#+END_SRC"))

;; Thank you protesilaos for denote. I tried to understand Org Roam but I am too dumb.

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/media/doc/notes/"))
  (denote-rename-buffer-mode 1))
(use-package denote-org
  :ensure t)

;; taken from protesilaos basic configuration
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package which-key
  :config
  (which-key-mode))

;; Magit is like the gates to heaven. I did not use version control enough before magit.
;; Now, it's just too easy.

(use-package magit
  :ensure t)

;; When editing emacs lisp, it's nice to have different colours for different list levels.
;; This assists in keeping track of which level you are nested into at any given time.

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(defun open-init-file () (interactive) (find-file "~/.emacs.d/keenban.org"))
(defun open-xinitrc () (interactive) (find-file "~/.xinitrc"))
(defun open-bashrc () (interactive) (find-file "~/.bashrc"))

(global-set-key (kbd "C-c e i") 'open-init-file)
(global-set-key (kbd "C-c e x") 'open-xinitrc)
(global-set-key (kbd "C-c e b") 'open-bashrc)

(set-face-attribute 'default nil :family "Monospace" :height 160)

(set-face-attribute 'line-number nil :foreground "grey75")

;; Sometimes, I prefer emacs to start in fullscreen mode.
;; This helps me stay focused and productive.

(toggle-frame-fullscreen)

(setq initial-scratch-message "")

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message () )

(setq epa-pinentry-mode 'loopback)

;; The default list-buffers menu bound to C-x C-b is dramatically
;; improved upon by the ibuffer menu.

;; replace C-x C-b with ibuffer
(define-key (current-global-map) [remap list-buffers] 'ibuffer)

;; When editing text files i like to have a little room to breathe on the left and right margins.
;; It is known that a medium line length improves comprehension and reading speed. [1]
;; 1. [[https://www.sciencedirect.com/science/article/abs/pii/S1071581901904586][The influence of reading speed and line length on the effectiveness of reading from screen]]

(defun my/focus ()
  ;; set margins
  (set-window-margins (selected-window) 25 25)
  
  ;; set fringe to white to make margin blend in
  (set-face-attribute 'fringe nil :background "white")

  ;; display relative line numbers
  (setq display-line-numbers 'relative))

;; move the custom file to a seperate file in emacs directory
;; this allows the init.el to be tangled to without overwriting custom set options

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; taken from mastering emacs
;; easier to switch with 2 keys
(global-set-key (kbd "M-o") 'other-window)

;; taken from emacs from scratch
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(require 'exwm)

(setq exwm-workspace-number 1)

(add-hook 'exwm-update-class-hook
	  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

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

(exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
                    (lambda () (interactive) (shell-command "xbacklight -dec 5")))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
                    (lambda () (interactive) (shell-command "xbacklight -inc 5")))

(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda () (interactive) (shell-command "pactl set-sink-volume @DEFAULT_SINK@ -5%")))
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda () (interactive) (shell-command "pactl set-sink-volume @DEFAULT_SINK@ +5%")))
(exwm-input-set-key (kbd "<XF86AudioMute>")
                    (lambda () (interactive) (shell-command "pactl set-sink-mute @DEFAULT_SINK@ toggle")))

(exwm-wm-mode)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package hyperbole)

(use-package eat
  :config
  (setq eat-term-terminfo "xterm-256color")
  :bind
  (("C-c RET" . eat)))

(use-package markdown-mode)

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets")
  :config
  (yas-global-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package magit)

(use-package which-key
  :config
  (which-key-mode))

(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq org-log-done t))

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
  (setq denote-directory (expand-file-name "~/dox/notes/"))
  (denote-rename-buffer-mode 1))
(use-package denote-org)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq sql-sqlite-program "/usr/bin/sqlite3") ;; for emacs < 29 or no sqlite built-in emacs

(use-package calibredb
  :defer t
  :config
  (setq calibredb-root-dir "~/dox/calibre")
  ;; for folder driver metadata: it should be .metadata.calibre
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/dox/calibre" (name . "Calibre")))) ;; with name
  :bind
  ("C-c b" . calibredb))

(use-package pdf-tools)

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)
  
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail/gmail")
  
  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")
  
  (setq mu4e-maildir-shortcuts
	'(("/Inbox"             . ?i)
          ("/[Gmail]/Sent Mail" . ?s)
          ("/[Gmail]/Trash"     . ?t)
          ("/[Gmail]/Drafts"    . ?d)
          ("/[Gmail]/All Mail"  . ?a))))

(use-package eww
  :ensure t
  :bind
  (("C-c w" . eww)))

(defun open-init-file () (interactive) (find-file user-init-file))
(defun open-xinitrc () (interactive) (find-file "~/.xinitrc"))
(defun open-bashrc () (interactive) (find-file "~/.bashrc"))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key [remap list-buffers] 'ibuffer)

(global-set-key (kbd "C-c e i") 'open-init-file)
(global-set-key (kbd "C-c e x") 'open-xinitrc)
(global-set-key (kbd "C-c e b") 'open-bashrc)

(setq-default dired-listing-switches "--all --color=auto --human-readable -l")
(set-frame-font "JetBrains Mono 14" nil t)
(setq-default org-agenda-files "~/dox/notes/20250707T180240--agenda__agenda.org")

(column-number-mode)

(use-package hyperbole
  :ensure t)

(use-package eat
  :ensure t
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
  :ensure t
  :init
  (vertico-mode))

(use-package magit
  :ensure t)

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

(use-package pdf-tools
  :ensure t)

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

(use-package eww
  :ensure t
  :bind
  (("C-c w" . eww)))

(defun open-init-file () (interactive) (find-file user-init-file))
(defun open-xinitrc () (interactive) (find-file "~/.xinitrc"))
(defun open-bashrc () (interactive) (find-file "~/.bashrc"))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-c r") 'eval-region)

(global-set-key (kbd "C-c e i") 'open-init-file)
(global-set-key (kbd "C-c e x") 'open-xinitrc)
(global-set-key (kbd "C-c e b") 'open-bashrc)

(setq org-agenda-files '("~/dox/notes/20250707T180240--agenda__agenda.org"))
(setq-default dired-listing-switches "--all --color=auto --human-readable -l")
(setq left-margin-width 5)
(set-frame-font "JetBrains Mono 14" nil t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(column-number-mode)

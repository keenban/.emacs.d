;; emulate a terminal
(use-package eat
  :ensure t
  :bind
  (("C-c RET" . eat)))
;; denote stuff
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

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))
;; org mode stuff
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(confirm-kill-processes nil)
 '(custom-enabled-themes '(ef-winter))
 '(custom-safe-themes t)
 '(display-battery-mode t)
 '(ef-themes-disable-other-themes t)
 '(ef-themes-mixed-fonts t)
 '(ef-themes-variable-pitch-ui t)
 '(inhibit-startup-buffer-menu nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice 'denote)
 '(org-agenda-files
   '("/home/keenban/dox/notes/20250628T231101--gift-ideas__marigiftcute.org"
     "/home/keenban/dox/notes/20250628T180606.org"
     "/home/keenban/dox/notes/20250628T144028--today__note.org"))
 '(org-fold-core-style 'overlays)
 '(org-modern-mode-hook '(org-modern-mode-set-explicitly))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(calibre company compat denote denote-journal denote-org dired-du eat
	     ef-themes ellama emms exwm hyperbole magit markdown-mode
	     org org-contacts org-modern osm pdf-tools pinentry
	     transcribe vertico yasnippet))
 '(tool-bar-mode nil)
 '(user-full-name "Keenan Salandy")
 '(user-mail-address "keenban@proton.me")
 '(which-key-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight regular :height 158 :width normal)))))

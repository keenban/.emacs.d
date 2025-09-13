;;; ---------------------------------------------------------------------------
;;; Package management
;;; ---------------------------------------------------------------------------
(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Helper: install a list of packages if they’re not already installed
(defun ensure-package-installed (&rest packages)
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;; Packages to ensure are present
(ensure-package-installed
 'vertico 'marginalia 'orderless 'consult
 'company 'magit 'emms 'denote 'denote-journal 'bbdb)

;;; ---------------------------------------------------------------------------
;;; Backups and auto-saves
;;; ---------------------------------------------------------------------------

;; Store all backups in ~/.emacs.d/tmp/backups/
(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Store auto-save files in ~/.emacs.d/tmp/auto-saves/
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/"
                                                  user-emacs-directory)
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;;; ---------------------------------------------------------------------------
;;; General UI/behavior tweaks
;;; ---------------------------------------------------------------------------

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq use-dialog-box nil)                 ;; Disable GUI dialog boxes
(setq initial-scratch-message nil)        ;; Empty *scratch* buffer
(setq inhibit-startup-echo-area-message "keenban") ;; Suppress startup message

;; Auto-reload buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; GPG pinentry in minibuffer
(setq epa-pinentry-mode 'loopback)

;;; ---------------------------------------------------------------------------
;;; Theme and appearance
;;; ---------------------------------------------------------------------------

(require-theme 'modus-themes)

(setq modus-themes-mixed-fonts t
      modus-themes-prompts '(italic bold)
      modus-themes-headings
      '((0 1.75) (1 1.5) (2 1.375) (3 1.25) (t 1.125))
      modus-vivendi-tinted-palette-overrides
      '((bg-mode-line-active bg-lavender)
        (bg-paren-match bg-magenta-intense)
        (bg-prose-block-contents bg-dim)
        (bg-prose-block-delimiter bg-dim)
        (fg-prose-block-delimiter fg-main)
        (underline-err red-faint)
        (underline-warning yellow-faint)
        (underline-note cyan-faint)
	(fringe unspecified)
        (fg-heading-0 magenta-cooler)
        (fg-heading-1 magenta-cooler)
        (fg-heading-2 magenta-cooler)
        (fg-heading-3 magenta-cooler)
        (fg-heading-4 magenta-cooler)
        (fg-heading-5 magenta-cooler)
        (fg-heading-6 magenta-cooler)
        (fg-heading-7 magenta-cooler)
        (fg-heading-8 magenta-cooler)
        (comment yellow-faint)
	(string green-warmer)))

(load-theme 'modus-vivendi-tinted t)

;; Margin toggle

(defun my-toggle-margins ()
  "Set margins in current buffer."
  (interactive)
  (if (or (> left-margin-width 0) (> right-margin-width 0))
      (progn
        (setq left-margin-width 0)
        (setq right-margin-width 0)
        (set-window-buffer (selected-window) (current-buffer)))
    (setq left-margin-width 26)
    (setq right-margin-width 26)
    (set-window-buffer (selected-window) (current-buffer))))

(global-set-key [f5] 'my-toggle-margins)

;;; ---------------------------------------------------------------------------
;;; Personal Information (BBDB)
;;; ---------------------------------------------------------------------------

(require 'bbdb)
(setq bbdb-default-country "Canada")

;;; ---------------------------------------------------------------------------
;;; History and discoverability helpers
;;; ---------------------------------------------------------------------------

(require 'savehist)
(add-hook 'after-init-hook #'savehist-mode)

(require 'which-key)
(add-hook 'after-init-hook #'which-key-mode)

;;; ---------------------------------------------------------------------------
;;; Completion and minibuffer setup
;;; ---------------------------------------------------------------------------

(require 'vertico)
(add-hook 'after-init-hook #'vertico-mode)

(require 'marginalia)
(add-hook 'after-init-hook #'marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides nil)

(require 'consult)
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap count-lines-page] 'consult-line)

;;; ---------------------------------------------------------------------------
;;; Dired configuration
;;; ---------------------------------------------------------------------------

(setq dired-listing-switches "-ahgo --group-directories-first --time-style=long-iso"
      dired-free-space nil
      dired-kill-when-opening-new-dired-buffer t)

;; Hide details in dired
(defun my-dired-init ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook 'my-dired-init)

;;; ---------------------------------------------------------------------------
;;; Org Mode configuration
;;; ---------------------------------------------------------------------------

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Basic Org settings
(setq org-startup-folded t
      org-startup-indented t
      org-insert-heading-respect-content t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-log-done t
      org-agenda-files '("~/media/doc/notes/20250707T180240--agenda.org"))

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Spell Checking
(add-hook 'text-mode-hook 'flyspell-mode)
(require 'company)
(add-hook 'text-mode-hook 'company-mode)

;; Capture templates
(setq org-capture-templates
      '(("f" "Fleeting thoughts and ideas" entry
         (file+headline
          "~/media/doc/notes/20250909T140227--inbox__important.org"
          "Fleeting")
         "* %^{Title}\n:PROPERTIES:\n:CAPTURED: %U\n:CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n:END:\n\n%?"
         :empty-lines-after 1)))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("j" "Journal" entry
                 (file denote-journal-path-to-new-or-existing-entry)
                 "* %U %?\n%i\n%a"
                 :kill-buffer t
                 :empty-lines 1)))

;;; ---------------------------------------------------------------------------
;;; Denote (note-taking and journaling)
;;; ---------------------------------------------------------------------------

(require 'denote)
(add-hook 'dired-mode-hook 'denote-dired-mode)

;; Keybindings
(global-set-key (kbd "C-c n n") 'denote)
(global-set-key (kbd "C-c n r") 'denote-rename-file)
(global-set-key (kbd "C-c n l") 'denote-link)
(global-set-key (kbd "C-c n b") 'denote-backlinks)
(global-set-key (kbd "C-c n d") 'denote-dired)
(global-set-key (kbd "C-c n g") 'denote-grep)

;; Basic settings
(setq denote-directory (expand-file-name "~/media/doc/notes/")
      denote-rename-confirmations nil)
(denote-rename-buffer-mode 1)

(require 'denote-journal)
(add-hook 'calendar-mode-hook 'denote-journal-calendar-mode)

(global-set-key (kbd "C-c n j") 'denote-journal-new-or-existing-entry)

(setq denote-journal-directory (expand-file-name "journal" denote-directory)
      denote-journal-keyword "journal"
      denote-journal-title-format 'day-date-month-year)

;;; ---------------------------------------------------------------------------
;;; Git integration
;;; ---------------------------------------------------------------------------

(require 'magit)
;; Suppress startup message
(setq magit-no-message '("Turning on magit-auto-revert-mode"))

;;; ---------------------------------------------------------------------------
;;; EMMS (Emacs Multimedia System)
;;; ---------------------------------------------------------------------------

(require 'emms-setup)

;; Suppress startup cache message
(with-eval-after-load 'emms-cache
  (defun emms-cache-restore ()
    "Restore the track cache from a file, quietly."
    (interactive)
    (load emms-cache-file t t t) ;; set NOMESSAGE to t
    (setq emms-cache-dirty nil)))

(emms-all)

(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native))

;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(denote-rename-confirmations nil)
 '(dired-listing-switches "--all --color=auto --human-readable -l -og")
 '(org-fold-core-style 'overlays)
 '(package-selected-packages '(ultra-scroll))
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url
		   "https://github.com/jdtsmith/ultra-scroll")))
 '(pixel-scroll-precision-mode t)
 '(pixel-scroll-precision-use-momentum t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(denote-faces-date ((t (:inherit font-lock-variable-name-face :foreground "white" :weight thin :height 0.1 :width condensed))))
 '(denote-faces-delimiter ((t (:foreground "white"))))
 '(denote-faces-extension ((t (:inherit shadow :foreground "white" :underline nil :height 0.5))))
 '(denote-faces-keywords ((t (:inherit font-lock-builtin-face :height 0.75))))
 '(denote-faces-title ((t (:weight bold :height 1.25))) t)
 '(fixed-pitch-serif ((t (:family "Serif")))))

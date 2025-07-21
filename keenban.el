(set-face-attribute 'line-number nil :foreground "grey75")

;; Sometimes, I prefer emacs to start in fullscreen mode.
;; This helps me stay focused and productive.

  (toggle-frame-fullscreen)

;; I then want to enable this in all text buffers,
;; as it should not be default in every buffer.

  ;; Create hook to enable these upon entering a text buffer.
    (add-hook 'text-mode #'my/focus)

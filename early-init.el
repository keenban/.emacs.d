
(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))


(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      menu-bar-mode nil
      tool-bar-mode nil
      tooltip-mode nil)
(set-scroll-bar-mode nil)
(set-face-attribute 'default nil :height 160 :background "#0d0e1c")

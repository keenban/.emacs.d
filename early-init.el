;; When I start Emacs, I want it to be as blank as possible.
;; This is because I find additional noise to distract from
;; what I am immediately trying to do.
  
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      menu-bar-mode nil
      tool-bar-mode nil
      tooltip-mode nil
      initial-scratch-message "")
(set-scroll-bar-mode nil)
(defun display-startup-echo-area-message () )

(defun open-init-file () (interactive) (find-file user-init-file))
(defun open-xinitrc () (interactive) (find-file "~/.xinitrc"))
(defun open-bashrc () (interactive) (find-file "~/.bashrc"))

(global-set-key (kbd "C-c e i") 'open-init-file)
(global-set-key (kbd "C-c e x") 'open-xinitrc)
(global-set-key (kbd "C-c e b") 'open-bashrc)

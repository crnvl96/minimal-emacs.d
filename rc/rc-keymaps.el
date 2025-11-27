;;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))

(provide 'rc-keymaps)

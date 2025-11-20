;;; -*- lexical-binding: t; -*-

(delight 'visual-line-mode)

(setq completion-ignore-case t)

(set-face-attribute 'default nil :height 180 :weight 'normal :family "Iosevka")
(set-face-attribute 'variable-pitch nil :height 180 :weight 'normal :family "Iosevka Aile")

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(add-hook 'compilation-filter-hook #'colorize-compilation-buffer)
(add-hook 'after-init-hook #'global-visual-line-mode)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x 2") (lambda ()
                                (interactive)
                                (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda ()
                                (interactive)
                                (split-window-horizontally) (other-window 1)))

(provide 'rc-builtin)

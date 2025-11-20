;;; -*- lexical-binding: t; -*-

(delight 'visual-line-mode)

(setq completion-ignore-case t)
(setq package-install-upgrade-built-in t)
(setq compile-command nil)

(set-face-attribute 'default nil :height 180 :weight 'normal :family "Iosevka")
(set-face-attribute 'variable-pitch nil :height 180 :weight 'normal :family "Iosevka Aile")

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(add-hook 'compilation-filter-hook #'colorize-compilation-buffer)
(add-hook 'after-init-hook #'global-visual-line-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'global-hl-line-mode)
(add-hook 'after-init-hook #'delete-selection-mode)

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

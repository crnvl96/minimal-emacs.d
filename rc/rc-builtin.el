;;; -*- lexical-binding: t; -*-

(setq completion-ignore-case t)
(setq package-install-upgrade-built-in t)
(setq compile-command nil)
(setq whitespace-style '(face trailing empty))
(setq whitespace-highlight-on-current-line t)
(setq auto-revert-interval 2)
(setq save-place-limit 150)

(set-face-attribute 'default nil :height 140 :weight 'normal :family "Iosevka")
(set-face-attribute 'variable-pitch nil :height 140 :weight 'normal :family "Iosevka Aile")

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))

(which-key-add-key-based-replacements
  "C-x p" "Project"
  "C-c c" "Crux"
  "C-c f" "Find")

(add-hook 'compilation-filter-hook (lambda () (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'after-init-hook #'global-visual-line-mode)
(add-hook 'after-init-hook #'which-key-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'global-hl-line-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'global-whitespace-mode)

(provide 'rc-builtin)

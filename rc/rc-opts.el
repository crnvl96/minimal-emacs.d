;;; -*- lexical-binding: t; -*-

(setq package-install-upgrade-built-in t)
(setq compile-command nil)
(setq whitespace-style '(face trailing empty))
(setq whitespace-highlight-on-current-line t)
(setq auto-revert-interval 2)
(setq save-place-limit 150)

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(add-hook 'compilation-filter-hook (lambda () (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'after-init-hook #'global-visual-line-mode)
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

(provide 'rc-opts)

;;; -*- lexical-binding: t; -*-

(setq-default display-line-numbers-type 'relative)
(setq-default word-wrap t)

(setq split-width-threshold 170
      split-height-threshold nil)

(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))

(setq-default truncate-lines t)
(setq native-comp-async-query-on-exit t)
(setq package-install-upgrade-built-in t)
(setq compile-command nil)
(setq whitespace-style '(face trailing empty))
(setq whitespace-highlight-on-current-line t)
(setq auto-revert-interval 2)
(setq save-place-limit 150)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(add-hook 'compilation-filter-hook (lambda () (ansi-color-apply-on-region compilation-filter-start (point-max))))
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

;; Prefer UTF-8 encoding
(prefer-coding-system 'utf-8)

(provide 'rc-opts)

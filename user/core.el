;;; core.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package delight
  :ensure t)

(use-package emacs
  :ensure nil
  :config
  (setq completion-ignore-case t)

  (set-face-attribute 'default nil :height 200 :weight 'normal :family "Iosevka")
  (set-face-attribute 'variable-pitch nil :height 200 :weight 'normal :family "Iosevka Aile")

  :bind (("M-n" . forward-paragraph)
         ("M-p" . backward-paragraph)
         ("C-x ;" . comment-or-uncomment-region)
         ("C-x 2" . (lambda ()
                      (interactive)
                      (split-window-vertically) (other-window 1)))
         ("C-x 3" . (lambda ()
                      (interactive) (split-window-horizontally) (other-window 1)))))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . colorize-compilation-buffer)
  :init (defun colorize-compilation-buffer ()
          (ansi-color-apply-on-region compilation-filter-start (point-max))))

(use-package package
  :ensure nil
  :config (setq package-install-upgrade-built-in t))

(use-package simple
  :ensure nil
  :hook (after-init . global-visual-line-mode))

(use-package compile
  :ensure nil
  :config (setq compile-command nil))

(use-package pixel-scroll
  :ensure nil
  :config
  (unless (and (eq window-system 'mac)
               (bound-and-true-p mac-carbon-version-string))
    (setq pixel-scroll-precision-use-momentum nil)
    (pixel-scroll-precision-mode 1)))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package display-line-numbers
  :ensure nil
  :hook (after-init . global-display-line-numbers-mode))

(use-package time
  :ensure nil
  :hook (after-init . display-time-mode))

(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

(use-package eldoc
  :ensure nil
  :delight)

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config (setq auto-revert-interval 3
                auto-revert-remote-files nil
                auto-revert-use-notify t
                auto-revert-avoid-polling nil
                auto-revert-verbose t))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package whitespace
  :ensure nil
  :delight
  (whitespace-mode)
  (global-whitespace-mode)
  :hook (after-init . global-whitespace-mode)
  :config (setq whitespace-style '(face trailing empty)
                whitespace-highlight-on-current-line t))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :config (setq save-place-limit 150))

(use-package flymake
  :ensure nil
  :bind (("C-}" . flymake-goto-next-error)
         ("C-{" . flymake-goto-prev-error)))

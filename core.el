;;; core.el --- Core options -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package emacs
  :init
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  :hook
  (after-init . display-time-mode)
  (after-init . show-paren-mode)
  (after-init . global-auto-revert-mode)
  (after-init . recentf-mode)
  (after-init . save-place-mode)
  (after-init . savehist-mode)
  :custom
  (text-mode-ispell-word-completion nil)
  :config
  (setq scroll-margin 8)
  (setq hscroll-margin 16)
  (setq-default display-line-numbers-type 'relative)
  (setq package-install-upgrade-built-in t)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'display-line-numbers-mode))
  (set-face-attribute 'default nil
                      :height 200 :weight 'normal :family "Berkeley Mono"))

;;; core.el ends here

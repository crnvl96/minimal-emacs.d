;;; treesit.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package treesit-auto
  :ensure t
  :hook (after-init . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; user/treesit.el --- Treesitter files -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Set the maximum level of syntax highlighting for Tree-sitter modes
(setq treesit-font-lock-level 4)

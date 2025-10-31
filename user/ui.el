;;; user/ui.el --- UI files -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

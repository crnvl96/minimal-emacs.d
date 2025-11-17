;;; code.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :bind ("M-i" . avy-goto-char-2))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package smartparens
  :delight
  :ensure t
  :hook (prog-mode text-mode markdown-mode)
  :config (require 'smartparens-config))

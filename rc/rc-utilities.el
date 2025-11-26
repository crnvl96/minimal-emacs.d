;;; -*- lexical-binding: t; -*-

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :bind ("M-i" . avy-goto-char-2))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'rc-utilities)

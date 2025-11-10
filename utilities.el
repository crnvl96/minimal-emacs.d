;;; utilities.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package magit
  :ensure t)

(use-package editorconfig
  :ensure t
  :hook
  (after-init . editorconfig-mode))

(use-package undo-fu
  :ensure t
  :demand t
  :config
  (global-unset-key (kbd "C-z"))
  :bind (("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :hook (after-init . undo-fu-session-global-mode))

(use-package eat
  :ensure t
  :commands eat)

(use-package wgrep
  :ensure t)

(use-package buffer-terminator
  :ensure t
  :delight
  :hook
  (after-init . buffer-terminator-mode)
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 20 60))
  (buffer-terminator-interval (* 10 60)))

(use-package helpful
  :ensure t
  :commands (helpful-callable
			 helpful-variable
			 helpful-key
			 helpful-command
			 helpful-at-point
			 helpful-function)
  :custom
  (helpful-max-buffers 3)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable))

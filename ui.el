;;; ui.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package ef-themes
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-dark t))

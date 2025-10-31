;;; user/themes.el --- Themes files -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :demand t)

(use-package ef-themes
  :ensure t
  :demand t)

(use-package doric-themes
  :ensure t
  :demand t)

(use-package standard-themes
  :ensure t
  :demand t)

(load-theme 'ef-melissa-light t)

;;; user/magit.el --- Magit files -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package magit
  :ensure t
  :bind (:map magit-mode-map
              ("<tab>" . nil)
              ("=" . magit-section-toggle)))

;;; -*- lexical-binding: t; -*-

(use-package rg
  :ensure t
  :config
  (rg-enable-menu)
  (require 'rg-isearch)
  (define-key isearch-mode-map "\M-s" 'rg-isearch-menu))

(provide 'rc-rg)

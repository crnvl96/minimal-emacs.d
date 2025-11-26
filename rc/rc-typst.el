;;; -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure t
  :mode (("\\.typ\\'" . typst-ts-mode))
  :config (setq typst-ts-watch-options "--open"
                typst-ts-mode-enable-raw-blocks-highlight t)
  :bind (:map typst-ts-mode-map
              ("C-c C-c" . typst-ts-menu)))

(provide 'rc-typst)

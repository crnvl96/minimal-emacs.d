;;; lang.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package apheleia
  :ensure t
  :delight
  :hook ((prog-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
		'(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
		'(gofmt)))

(defun crnvl96/corfu-setup-lsp ()
  "Use orderless and corfu completion style with lsp-capf instead of the
  default lsp-passthrough."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	    '(orderless)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((go-ts-mode . lsp-deferred)
		 (lsp-mode . lsp-ui-mode)
		 (lsp-completion-mode . crnvl96/corfu-setup-lsp))
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c .")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-enable-symbol-highlighting nil))

(use-package lsp-ui
  :ensure t
  :defer t
  :custom
  (lsp-ui-doc-show-with-mouse nil))

(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred)))
  :custom (lsp-pyright-langserver-command "pyright"))

(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

(use-package go-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
			 gfm-view-mode
			 markdown-mode
			 markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("README\\.md\\'" . gfm-mode)))

(use-package typst-ts-mode
  :ensure t
  :mode (("\\.typ\\'" . typst-ts-mode))
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :bind (:map typst-ts-mode-map
			  ("C-c C-c" . typst-ts-menu)))

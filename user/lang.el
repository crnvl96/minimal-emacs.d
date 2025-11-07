;;; lang.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; Treesitter
;; `crnvl96/treesit-install-all-languages' to install all languages
;; `treesit-install-language-grammar' to install a specific language
;; use `sort-lines' to sort
(setq treesit-language-source-alist
      '(
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.0"))
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
        (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/ikatyang/tree-sitter-toml")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (typst "https://github.com/uben0/tree-sitter-typst")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        ))

(defun crnvl96/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	  (treesit-install-language-grammar lang)
	  (message "`%s' parser was installed." lang)
	  (sit-for 0.75))))

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c .")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  :hook ((python-ts-mode . lsp)
         (go-ts-mode . lsp)
         (lsp-mode . lsp-ui-mode)
         (lsp-mode . lsp-lens-mode)
         (lsp-completion-mode . crnvl96/corfu-setup-lsp)) ; Use corfu instead the default for lsp completions
  :commands lsp
  :custom
  (lsp-completion-provider :none) ; Use corfu instead the default for lsp completions
  :config
  (defun crnvl96/corfu-setup-lsp ()
    "Use orderless and corfu completion style with lsp-capf instead of the
               default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-mouse nil))

(use-package lsp-pyright
  :ensure t)

;; Formatter
(use-package apheleia
  :ensure t
  :commands (apheleia-mode apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(gofumpt)))

;; Python
(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

(use-package pyvenv
  :ensure t)

;; Go
(use-package go-mode
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;; Typst
(use-package typst-ts-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t))

;;Org
(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-startup-truncated t))

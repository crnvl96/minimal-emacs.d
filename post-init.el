;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(load-file "~/.emacs.d/core.el")
(load-file "~/.emacs.d/editor.el")

(use-package editorconfig
  :ensure t
  :hook
  (after-init . editorconfig-mode))

(use-package ef-themes
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-dark t))

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

(use-package magit
  :ensure t
  :bind (("C-c g s" . magit-status)
         ("C-c g i" . magit-init)
         ("C-c g S" . magit-stage-file)
         ("C-c g U" . magit-unstage-file)
         ("C-c g b" . magit-blame-addition))
  :custom
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-diff-refine-hunk t)
  (magit-stage-all-confirm nil)
  (magit-unstage-all-confirm nil))

(use-package eat
  :ensure t
  :commands eat)

(use-package wgrep
  :ensure t)

(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function
			 elisp-refs-macro
			 elisp-refs-variable
			 elisp-refs-special
			 elisp-refs-symbol))

(use-package buffer-terminator
  :ensure t
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

(setq treesit-language-source-alist
      ;; - Use `crnvl96/treesit-install-all-languages' to install all languages
      ;; - Use `treesit-install-language-grammar' to install a specific language
      ;; - Use `sort-lines' to sort
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
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
	    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun crnvl96/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75))))

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

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

(use-package apheleia
  :ensure t
  :hook ((prog-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
		'(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
		'(gofmt)))

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

(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode (("\\.org\\'" . org-mode))
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

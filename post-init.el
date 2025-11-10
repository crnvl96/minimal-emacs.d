;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package delight
  :ensure t)

(use-package ef-themes
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-elea-dark t))

(use-package emacs
  :ensure nil
  :delight
  (eldoc-mode)
  (emacs-lisp-mode "Elisp" :major)
  :hook
  ;; (after-init . global-display-line-numbers-mode)
  (after-init . display-time-mode)
  (after-init . global-auto-revert-mode)
  (after-init . global-hl-line-mode)
  (after-init . global-whitespace-mode)
  (after-init . recentf-mode)
  (after-init . save-place-mode)
  (after-init . savehist-mode)
  (after-init . show-paren-mode)
  (after-init . delete-selection-mode)
  ;; ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :custom
  ;; This setting can be used together with `use-package-report' to profile emacs startup time
  ;; (use-package-compute-statistics t)
  (scroll-margin 8)
  (hscroll-margin 16)
  (whitespace-style '(face trailing empty))
  (whitespace-highlight-on-current-line t)
  (package-install-upgrade-built-in t)
  (save-place-limit 150)
  :config
  ;; (setq-default display-line-numbers-type 'relative)
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90)
  (set-face-attribute 'default nil :height 220 :weight 'normal :family "Iosevka")
  (set-face-attribute 'variable-pitch nil :height 220 :weight 'normal :family "Iosevka Aile"))

(use-package eww
  :ensure nil
  :commands eww)

(use-package which-key
  :ensure nil
  :delight
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-x p" "Project"
    "C-c ." "LSP"
    "C-c g" "Magit"
    "C-c f" "Find")
  :bind
  ("M-n" . forward-paragraph)
  ("M-p" . backward-paragraph))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        ;; - Use `crnvl96/treesit-install-all-languages' to install all languages
        ;; - Use `treesit-install-language-grammar' to install a specific language
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
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

;; (use-package ace-window
;;   :ensure t
;;   :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :bind ("M-i" . avy-goto-char-2))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit] . crux-keyboard-quit-dwin)
         ([remap upcase-region] . crux-upcase-region)
         ([remap downcase-region] . crux-downcase-region)
         ("C-^" . crux-top-join-line)
         ("C-6" . crux-other-window-or-switch-buffer)
         ("C-k" . crux-smart-kill-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c x" . crux-transpose-windows)
         ("C-c o" . crux-open-with)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c r" . crux-recentf-find-file)
         ("C-c R" . crux-recentf-find-directory)
         ("C-c k" . crux-kill-other-buffers))
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

(use-package marginalia
  :ensure t
  :hook
  (after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil))

(defun crnvl96/corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
	          (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)
    (corfu-mode 1)))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  (after-init . corfu-popupinfo-mode)
  (minibuffer-setup . crnvl96/corfu-enable-always-in-minibuffer)
  :custom
  (corfu-cycle t)
  (corfu-preselect nil)
  (corfu-auto nil)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preview-current t)
  (corfu-on-exact-match nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :bind (:map corfu-map
			  ("C-e" . corfu-quit)
			  ("C-i" . corfu-complete)
			  ("C-y" . corfu-insert)))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
			     nil
			     (window-parameters (mode-line-format . none))))
  :bind (("C-." . embark-act)
		 ("C-," . embark-dwin)
		 ("C-/" . embark-bindingsk)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (("C-c f f" . consult-fd)
         ("C-c f l" . consult-line)
         ("C-c f g" . consult-ripgrep)
         ("C-c f b" . consult-project-buffer)
         ("C-c f L" . consult-goto-line)))

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
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-agenda-tags-column 0)
  (org-ellipsis "…")
  (org-startup-truncated t))

(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (set-face-attribute 'org-modern-symbol nil :height 220 :weight 'normal :family "Iosevka"))


;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :hook
  (after-init . global-hl-line-mode)
  (after-init . display-time-mode)
  (after-init . show-paren-mode)
  (after-init . winner-mode)
  (after-init . minibuffer-depth-indicate-mode)
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  (dired-mode . dired-hide-details-mode)
  :custom
  ;; This setting can be used together with `use-package-report' to profile emacs startup time
  ;; (use-package-compute-statistics t)
  (scroll-margin 8)
  (hscroll-margin 16)
  (dired-movement-style 'bounded-files)
  (package-install-upgrade-built-in t)
  :config
  (setq-default display-line-numbers-type 'relative)
  (set-face-attribute 'default nil :height 200 :weight 'normal :family "Berkeley Mono"))

(use-package delete-selection
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package auto-save-visited
  :ensure nil
  :hook (after-init . auto-save-visited-mode)
  :custom (auto-save-visited-interval 5))

(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :custom
  (tooltip-hide-delay 20)
  (tooltip-delay 0.4)
  (tooltip-short-delay 0.08))

(use-package eww
  :ensure nil
  :commands eww)

(use-package whitespace
  :ensure nil
  :hook
  (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face trailing empty))
  (whitespace-highlight-on-current-line t))

(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring
	 register-alist
	 mark-ring global-mark-ring
	 search-ring regexp-search-ring)))

(use-package autorevert
  :ensure nil
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

(use-package recentf
  :ensure nil
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
		 "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
		 "\\.7z$" "\\.rar$"
		 "COMMIT_EDITMSG\\'"
		 "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
		 "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package saveplace
  :ensure nil
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(use-package server
  :ensure nil
  :hook
  (after-init . server-start))

(use-package ef-themes
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-dark t))

(use-package undo-fu
  :ensure t
  :demand t)

(use-package undo-fu-session
  :ensure t
  :hook (after-init . undo-fu-session-global-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :hook (after-init . evil-mode)
  :custom
  (evil-ex-visual-char-range t)
  (evil-ex-search-vim-style-regexp t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-echo-state nil)
  (evil-move-cursor-back nil)
  (evil-v$-excludes-newline t)
  (evil-want-C-h-delete t)
  (evil-want-C-u-delete t)
  (evil-want-fine-undo t)
  (evil-move-beyond-eol t)
  (evil-search-wrap nil)
  (evil-want-Y-yank-to-eol t))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(with-eval-after-load "evil"
  (evil-define-operator crnvl96/evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'crnvl96/evil-comment-or-uncomment))

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

(use-package which-key
  :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
	"C-x p" "Project"
	"C-c ." "LSP"
	"C-c g" "Magit"
	"C-c f" "Find"))

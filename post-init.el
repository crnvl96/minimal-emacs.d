;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(load-file "~/.emacs.d/user/options.el")

(use-package modus-themes
  :ensure t)

(use-package ef-themes
  :ensure t)

(use-package standard-themes
  :ensure t)

(use-package doric-themes
  :ensure t)

(use-package doom-themes
  :ensure t
  :custom
  ;; if nil, bold is universally disabled
  (doom-themes-enable-bold t)
  ;; if nil, italics is universally disabled
  (doom-themes-enable-italic t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'ef-duo-dark t)

(use-package outline-indent
  :ensure t
  :commands outline-indent-minor-mode
  :custom
  (outline-indent-ellipsis " ▼ ")
  :init
  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode))

(use-package golden-ratio-scroll-screen
  :ensure t
  :bind (("C-v" . golden-ratio-scroll-screen-up)
         ("M-v" . golden-ratio-scroll-screen-down)))

(use-package stripspace
  :ensure t
  :commands stripspace-local-mode
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t)
  :init
  (stripspace-global-mode 1))

(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package paredit
  :ensure t
  :commands paredit-mode
  :hook
  (emacs-lisp-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package page-break-lines
  :ensure t
  :commands (page-break-lines-mode
             global-page-break-lines-mode)
  :hook
  (emacs-lisp-mode . page-break-lines-mode))

(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

(use-package persist-text-scale
  :commands (persist-text-scale-mode
             persist-text-scale-restore)
  :hook (after-init . persist-text-scale-mode)
  :custom
  (text-scale-mode-step 1.07))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Version control
(use-package magit
  :ensure t)

;; Better Undo-Redo
(use-package undo-fu
  :ensure t
  :demand t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :config
  (global-unset-key (kbd "C-z"))
  :bind (("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

;; Completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                                                          ; Enable cycling the selector
  (corfu-preselect nil)                                                    ; Don't preselect any item
  (corfu-auto nil)                                                         ; Show completion only when requested
  (corfu-quit-at-boundary nil)                                             ; Never quit at completion boundary
  (corfu-quit-no-match nil)                                                ; Never quit, even if there is no match
  (corfu-preview-current t)                                                ; Enable current candidate preview
  (corfu-on-exact-match nil)                                               ; Configure handling of exact matches
  (read-extended-command-predicate #'command-completion-default-include-p) ; Hide commands in M-x which do not apply to the current mode.
  (text-mode-ispell-word-completion nil)                                   ; Disable Ispell completion function. As an alternative try `cape-dict'.
  (tab-always-indent 'complete)                                            ; Controls the operation of the TAB key. Hitting TAB always just indents the current line.
  :bind (:map corfu-map                                                    ; Keymaps uses when completion popup is shown
              ("C-e" . corfu-quit)                                         ; Exit completion
              ("C-i" . corfu-complete)                                     ; Trigger completion
              ("C-y" . corfu-insert))                                      ; Insert the currently selected item
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

;; Completion at point package
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;; Search and navigation
(use-package wgrep
  :ensure t)

;; Context-aware actions
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-," . embark-dwim)
   ("C-/" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Integration between Embark and consult
;; This package is auto-loaded when Consult is detected, so there's no need
;; for awaiting it to be loaded
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Better interface for Search (files, grep, buffers, etc..)
(use-package consult
  :ensure t
  :bind (("C-c f l" . consult-line)
         ("C-c f f" . consult-fd)
         ("C-c f g" . consult-ripgrep)
         ("C-c f L" . consult-goto-line)
         ("C-c f b" . consult-project-buffer)))

;; Language support
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

(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

(use-package apheleia
  :ensure t
  :commands (apheleia-mode apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(gofumpt)))

(use-package pyvenv
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package typst-ts-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

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

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package ace-window
  :ensure t
  :bind (("M-i" . avy-goto-char-2)))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package zoom
  :ensure t
  :custom
  (zoom-size '(0.618 . 0.618))
  :config
  (zoom-mode t))

;; Utilities
(use-package which-key
  :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40)
  :config
  (which-key-add-key-based-replacements
    "C-x p" "Project"
    "C-c ." "LSP"))

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 20 60)) ;20 minutes
  (buffer-terminator-interval (* 10 60)) ; 10 minutes
  :config
  (buffer-terminator-mode 1))

(use-package eat
  :ensure t)

(use-package agent-shell
  :ensure t)

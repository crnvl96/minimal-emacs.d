;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Compilation
(use-package compile-angel
  :demand t
  :custom
  (compile-angel-verbose t)
  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1))

(use-package emacs
  :init
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  :hook
  (after-init . display-time-mode)
  (after-init . show-paren-mode)
  (after-init . global-auto-revert-mode)
  (after-init . recentf-mode)
  (after-init . save-place-mode)
  (after-init . savehist-mode)
  :custom
  (text-mode-ispell-word-completion nil)
  :config
  (setq scroll-margin 8)
  (setq hscroll-margin 16)
  ;; (setq-default display-line-numbers-type 'relative)
  (setq package-install-upgrade-built-in t)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'display-line-numbers-mode))
  (set-face-attribute 'default nil
                      :height 200 :weight 'normal :family "Berkeley Mono")
  :bind(("M-n" . forward-paragraph)
        ("M-p". backward-paragraph)))

(use-package modus-themes
  :ensure t)

(use-package ef-themes
  :ensure t)

(use-package standard-themes
  :ensure t)

(use-package doric-themes
  :ensure t)

(load-theme 'standard-light-tinted t)

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

(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Version control
(use-package magit
  :ensure t)

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

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect nil)
  (corfu-auto nil)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<escape>" . corfu-quit)
              ("C-i" . corfu-complete)
              ("C-g" . corfu-quit)
              ("C-e" . corfu-quit)
              ("RET" . corfu-insert)
              ("C-y" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;; Search and navigation
(use-package wgrep
  :ensure t)

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

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (("C-c f l" . consult-line)
         ("C-c f f" . consult-fd)
         ("C-c f g" . consult-ripgrep)
         ("C-c f L" . consult-goto-line)
         ("C-c f b" . consult-project-buffer)))

;; Language support
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq typst-tsauto-config
      (make-treesit-auto-recipe
       :lang 'typst
       :ts-mode 'typ-ts-mode
       :remap '(typst-mode)
       :url "https://github.com/uben0/tree-sitter-typst"
       :revision "master"
       :source-dir "src"
       :ext "\\.typ\\'"))

(add-to-list 'treesit-auto-recipe-list typst-tsauto-config)

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

(use-package typst-ts-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

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

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; post-init.el ends here

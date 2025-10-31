;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;;; Custom packages and configurations that align with my personal taste.

;;; Code:

(use-package modus-themes
  :ensure t
  :demand t)

(use-package ef-themes
  :ensure t
  :demand t)

(use-package doric-themes
  :ensure t
  :demand t)

(use-package standard-themes
  :ensure t
  :demand t)

(load-theme 'ef-melissa-light t)

(use-package emacs
  :custom
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  (text-mode-ispell-word-completion nil)
  :hook
  (after-init-hook . display-time-mode)
  (after-init-hook . show-paren-mode)
  (after-init-hook . save-place-mode)
  (after-init-hook . savehist-mode)
  (after-init-hook . recentf-mode)
  (after-init-hook . global-auto-revert-mode)
  :config
  (setq display-line-numbers-type 'relative)
  (setq package-install-upgrade-built-in t)
  (set-face-attribute 'default nil
                      :height 160 :weight 'normal :family "Berkeley Mono")
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'display-line-numbers-mode))
  :bind (("C-6" . (lambda () (interactive) (switch-to-buffer (other-buffer))))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-w" . vertico-directory-delete-word)
              ("RET" . vertico-directory-enter)))

  (use-package orderless
    :ensure t
    :custom
    (orderless-style-dispatchers '(orderless-affix-dispatch))
    (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles partial-completion))))
    (completion-category-defaults nil))

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
                ("C-g" . corfu-quit)
                ("C-e" . corfu-quit)
                ("RET" . corfu-insert)
                ("C-y" . corfu-insert)
                ("M-d" . corfu-show-documentation)
                ("M-l" . corfu-show-location))
    :init
    (global-corfu-mode)
    (corfu-popupinfo-mode)
    :hook
    (minibuffer-setup-hook . corfu-enable-always-in-minibufer)
    :config
    (defun corfu-enable-always-in-minibuffer ()
      "Enable Corfu in the minibuffer if Vertico/Mct are not active."
      (unless (or (bound-and-true-p mct--active)
                  (bound-and-true-p vertico--input))
        (setq-local corfu-auto nil)
        (corfu-mode 1))))

  (use-package cape
    :ensure t
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

  (use-package magit
    :ensure t
    :bind (:map magit-mode-map
                ("<tab>" . nil)
                ("=" . magit-section-toggle)))

  (use-package consult
    :ensure t
    :bind (("C-c f l" . consult-line)
           ("C-c f f" . consult-fd)
           ("C-c f g" . consult-ripgrep)))

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
      "Use orderless + corfu completion style with lsp-capf instead of the
    default lsp-passthrough."
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))))

  (use-package pyvenv
    :ensure t)

  (use-package go-mode
    :ensure t)

  (use-package apheleia
    :ensure t
    :commands (apheleia-mode apheleia-global-mode)
    :hook ((prog-mode . apheleia-mode))
    :config
    (setf (alist-get 'python-ts-mode apheleia-mode-alist)
          '(ruff-isort ruff))
    (setf (alist-get 'go-ts-mode apheleia-mode-alist)
          '(gofumpt)))

  (use-package flymake-ruff
    :ensure t
    :hook (python-ts-mode . flymake-ruff-load))

  (use-package lsp-pyright
    :ensure t)

  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :custom
    (lsp-ui-doc-show-with-mouse nil))

  (use-package markdown-mode
    :commands (gfm-mode
               gfm-view-mode
               markdown-mode
               markdown-view-mode)
    :mode (("\\.markdown\\'" . markdown-mode)
           ("\\.md\\'" . markdown-mode)
           ("README\\.md\\'" . gfm-mode))
    :bind
    (:map markdown-mode-map
          ("C-c C-e" . markdown-do)))

  (use-package avy
    :ensure t
    :bind (("M-i" . avy-goto-char-2)))

  (use-package ace-window
    :ensure t
    :init
    (defvar aw-dispatch-alist
      '((?c aw-delete-window "Delete Window")
  	    (?x aw-swap-window "Swap Windows")
  	    (?m aw-move-window "Move Window")
  	    (?n aw-flip-window "Flip Window")
  	    (?s aw-split-window-vert "Split Vert Window")
  	    (?v aw-split-window-horz "Split Horz Window")
  	    (?o delete-other-windows "Delete Other Windows")
  	    (?? aw-show-dispatch-help))
      "List of actions for `aw-dispatch-default'.")
    :custom
    (aw-dispatch-always t)
    :config
    (setq aw-keys '(?h ?j ?k ?k ?l ?g ?f ?d ?s ?a))
    :bind (("M-o" . ace-window)))

  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))

  (use-package undo-fu
    :ensure t
    :demand t
    :commands (undo-fu-only-undo
               undo-fu-only-redo
               undo-fu-only-redo-all
               undo-fu-disable-checkpoint)
    :config
    (global-unset-key (kbd "C-z"))
    (global-set-key (kbd "C-z") 'undo-fu-only-undo)
    (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

  (use-package undo-fu-session
    :ensure t
    :commands undo-fu-session-global-mode
    :hook (after-init . undo-fu-session-global-mode))

  (use-package eat
    :ensure t)

  (use-package buffer-terminator
    :ensure t
    :custom
    (buffer-terminator-verbose nil)
    (buffer-terminator-inactivity-timeout (* 10 60)) ;10 minutes
    (buffer-terminator-interval (* 5 60)) ; 5 minutes
    :config
    (buffer-terminator-mode 1))

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
      "C-c f" "Find"
      "C-c ." "LSP"))

;;; post-init.el ends her

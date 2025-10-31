;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(minimal-emacs-load-user-init "user/compile.el")
(minimal-emacs-load-user-init "user/emacs.el")
(minimal-emacs-load-user-init "user/themes.el")
(minimal-emacs-load-user-init "user/treesit.el")
(minimal-emacs-load-user-init "user/completion.el")
(minimal-emacs-load-user-init "user/magit.el")
(minimal-emacs-load-user-init "user/ui.el")
(minimal-emacs-load-user-init "user/nav.el")

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
  ;; Setup lsp to use corfu for lsp completion
  (defun crnvl96/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
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

;; The markdown-mode package provides a major mode for Emacs for syntax
;; highlighting, editing commands, and preview support for Markdown documents.
;; It supports core Markdown syntax as well as extensions like GitHub Flavored
;; Markdown (GFM).
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
  (setq aw-keys '(?f ?j ?d ?k ?l ?h ?g ?h ?a))
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
  (buffer-terminator-interval (* 10 60)) ; 10 minutes
  :config
  (buffer-terminator-mode 1))

;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
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

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; post-init.el ends her

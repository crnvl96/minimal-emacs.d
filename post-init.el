;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(minimal-emacs-load-user-init "user/compile.el")
(minimal-emacs-load-user-init "user/emacs.el")
(minimal-emacs-load-user-init "user/themes.el")
(minimal-emacs-load-user-init "user/treesit.el")

(use-package magit
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-c f l" . 'consult-line)
         ("C-c f f" . 'consult-fd)
         ("C-c f g" . 'consult-ripgrep)))

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
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
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

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle nil)
  (corfu-auto nil)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  :bind (:map corfu-map ("M-n" . corfu-complete))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

(use-package eglot
  :ensure nil
  :custom
  (eglot-completion-at-point-function nil)
  :hook ((python-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)))

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

(use-package which-key
  :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(use-package avy
  :ensure t
  :bind (("M-i" . avy-goto-char-2)))

(use-package ace-window
  :ensure t
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

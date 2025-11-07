;;; utilities.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package avy
  :ensure t)

(use-package golden-ratio-scroll-screen
  :ensure t)

(use-package magit
  :ensure t)

(use-package eat
  :ensure t)

(use-package ace-window
  :ensure t
  :config)

(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package undo-fu
  :ensure t
  :demand t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :config
  (global-unset-key (kbd "C-z")))

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

(use-package persist-text-scale
  :ensure t
  :commands (persist-text-scale-mode
             persist-text-scale-restore)
  :hook (after-init . persist-text-scale-mode)
  :custom
  (text-scale-mode-step 1.07))

(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 20 60)) ;20 minutes
  (buffer-terminator-interval (* 10 60)) ; 10 minutes
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
  (helpful-max-buffers 3))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil))

(use-package corfu
  :ensure t
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

;; Completion at point
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;; Context-aware actions
(use-package embark
  :ensure t
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

(use-package consult
  :ensure t)

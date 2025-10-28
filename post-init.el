;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package compile-angel
  :demand t
  :custom
  (compile-angel-verbose t)
  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1))

(set-face-attribute 'default nil
                    :height 200 :weight 'normal :family "Berkeley Mono")

(set-fringe-mode 10)                    ; frame edges set to 10px
(recentf-mode 1)                        ; remember recent files
(save-place-mode 1)                     ; remember cursor position
(savehist-mode 1)                       ; enable history saving
(global-hl-line-mode 1)                 ; enable current line highlight
(global-visual-line-mode t)             ; visual line breaking
(global-auto-revert-mode 1)             ; update externaly edited files

(setq package-install-upgrade-built-in t)
(delete-selection-mode 1)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

(setq-default display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(use-package server
  :ensure nil
  :commands server-start
  :hook
  (after-init . server-start))

(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-dark t))

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
  (corfu-auto t)
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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c L")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-completion-mode))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

(use-package ruff-format
  :ensure t
  :hook (python-mode . ruff-format-on-save-mode))

(use-package eglot
  :ensure nil
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-workspace-configuration '((:gopls . ((gofumpt . t)))))
  :hook ((go-mode . eglot-ensure)))

(use-package go-mode
  :ensure t)

(use-package which-key
  :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;; Window management keybindings
(global-set-key (kbd "C-c C-h") 'windmove-left)
(global-set-key (kbd "C-c C-j") 'windmove-down)
(global-set-key (kbd "C-c C-k") 'windmove-up)
(global-set-key (kbd "C-c C-l") 'windmove-right)
(global-set-key (kbd "C-c C-_") 'split-window-below)
(global-set-key (kbd "C-c C-|") 'split-window-right)
(global-set-key (kbd "C-c C-c") 'delete-window)
(global-set-key (kbd "C-c C-o") 'delete-other-windows)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; post-init.el ends here

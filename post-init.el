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
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

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

(use-package which-key
  :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; post-init.el ends here

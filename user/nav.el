;;; user/nav.el --- Nav files -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package consult
  :ensure t
  :bind (("C-c l" . consult-line)
         ("C-c f" . consult-fd)
         ("C-c g" . consult-ripgrep)))

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

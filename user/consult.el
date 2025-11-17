;;; consult.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package embark
  :ensure t
  :config (setq prefix-help-command #'embark-prefix-help-command
                eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind (("C-." . embark-act)
         ("C-," . embark-dwim)
         ("C-'" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :init
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons (mapcar (lambda (r) (consult--convert-regexp r type)) input)
          (lambda (str) (orderless--highlight input t str))))
  :config (setq consult-async-min-input 2
                consult-narrow-key "<"
                xref-show-xrefs-function #'consult-xref
                xref-show-definitions-function #'consult-xref
                consult--regexp-compiler #'consult--orderless-regexp-compiler)
  (consult-customize
   consult-buffer  consult-yank-pop consult-fd consult-outline
   consult-imenu consult-info consult-flymake consult-history
   consult-focus-lines consult-line consult-ripgrep consult-goto-line
   :preview-key nil)
  :bind (("C-c f b" . consult-buffer)
         ("C-c f y" . consult-yank-pop)
         ("C-c f f" . consult-fd)
         ("C-c f o" . consult-outline)
         ("C-c f i" . consult-imenu)
         ("C-c f I" . consult-info)
         ("C-c f k" . consult-flymake)
         ("C-c f h" . consult-history)
         ("C-c f u" . consult-focus-lines)
         ("C-c f l" . consult-line)
         ("C-c f g" . consult-ripgrep)
         ("C-c f L" . consult-goto-line)))

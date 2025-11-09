;;; editor.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :bind ("M-i" . avy-goto-char))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit] . crux-keyboard-quit-dwin)
         ("C-^" . crux-top-join-lines)
         ("C-k" . crux-smart-kill-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c o" . crux-open-with)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c r" . crux-recentf-find-file)
         ("C-c R" . crux-recentf-find-directory)
         ("C-c k" . crux-kill-other-buffers))
  :config
  (crux-with-region-or-buffer crux-cleanup-buffer-or-region)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

(use-package marginalia
  :ensure t
  :hook
  (after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil))

(defun crnvl96/corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
	          (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)
    (corfu-mode 1)))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  (after-init . corfu-popupinfo-mode)
  (minibuffer-setup . crnvl96/corfu-enable-always-in-minibuffer)
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
  :bind (:map corfu-map
			  ("C-e" . corfu-quit)
			  ("C-i" . corfu-complete)
			  ("C-y" . corfu-insert)))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
			     nil
			     (window-parameters (mode-line-format . none))))
  :bind (("C-." . embark-act)
		 ("C-," . embark-dwin)
		 ("C-/" . embark-bindingsk)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (("C-c f f" . consult-fd)
         ("C-c f l" . consult-line)
         ("C-c f g" . consult-ripgrep)
         ("C-c f b" . consult-project-buffer)
         ("C-c f L" . consult-goto-line)))


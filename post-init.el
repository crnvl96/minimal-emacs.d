;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d/rc/"))

(require 'rc-delight)
(require 'rc-builtin)
(require 'rc-spacious-padding)
(require 'rc-themes)
(require 'rc-icons)
(require 'rc-dired)

(use-package which-key
  :ensure nil
  :delight
  :hook (after-init . which-key-mode)
  :config (which-key-add-key-based-replacements
            "C-x p" "Project"
            "C-c c" "Crux"
            "C-c f" "Find"))

(use-package treesit-auto
  :ensure t
  :hook (after-init . global-treesit-auto-mode)
  :config
  (setq my-json-tsauto-config
        (make-treesit-auto-recipe
         :lang 'json
         :ts-mode 'json-ts-mode
         :remap '(json-mode)
         :url "https://github.com/tree-sitter/tree-sitter-json"
         :revision "master"
         :source-dir "src"
         :ext "\\.json\\'"))
  (add-to-list 'treesit-auto-recipe-list my-json-tsauto-config)
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :bind ("M-i" . avy-goto-char-2))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package smartparens
  :delight
  :ensure t
  :hook (prog-mode text-mode markdown-mode)
  :config (require 'smartparens-config))

;;; Utilities

(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package wgrep
  :ensure t)

(use-package buffer-terminator
  :ensure t
  :delight
  :hook (after-init . buffer-terminator-mode)
  :config (setq buffer-terminator-verbose nil
                buffer-terminator-inactivity-timeout (* 20 60)
                buffer-terminator-interval (* 10 60)))

(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :config
  (setq helpful-max-buffers 3)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable))

(use-package super-save
  :ensure t
  :delight
  :hook (after-init . super-save-mode)
  :init (setq auto-save-default nil)
  :config (setq super-save-auto-save-when-idle t
                super-save-delete-trailing-whitespace t
                super-save-all-buffers t)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package undo-fu
  :demand t
  :ensure t
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
  :hook (after-init . undo-fu-session-global-mode)
  :commands (undo-fu-session-global-mode))

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

(use-package crux
  :ensure t
  :demand t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit]  . crux-keyboard-quit-dwin)
         ([remap upcase-region] . crux-upcase-region)
         ([remap downcase-region] . crux-downcase-region)
         ("C-S-j" . crux-top-join-line)
         ("C-S-n" . crux-cleanup-buffer-or-region)
         ("C-S-t" . crux-visit-term-buffer)
         ("C-S-d" . crux-duplicate-current-line-or-region)
         ("C-k" . crux-smart-kill-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c c o" . crux-open-with)
         ("C-c c u" . crux-view-url)
         ("C-c c k" . crux-kill-other-buffers))
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

;;; Completion

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("<backspace>" . vertico-directory-delete-char)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-w" . vertico-directory-delete-word )
              ("RET" . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :config (setq completion-styles '(orderless partial-completion basic)
                completion-category-defaults nil
                completion-category-overrides '((eglot (styles orderless))
                                                (eglot-capf (styles orderless)))))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  (after-init . corfu-popupinfo-mode)
  (minibuffer-setup . my/corfu-enable-always-in-minibuffer)
  :init
  (defun my/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (setq read-extended-command-predicate #'command-completion-default-include-p
        text-mode-ispell-word-completion nil
        tab-always-indent 'complete)
  :config
  (setq corfu-cycle t)
  (setq corfu-preselect 'directory)
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode))))))

(use-package corfu-terminal
  :ensure t
  :vc ( :url "https://codeberg.org/akib/emacs-corfu-terminal.git"
        :rev
        :newest)
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; Consult

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

;;; Magit

(use-package magit
  :ensure t)

;;; Lang

(use-package apheleia
  :ensure t
  :delight
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(gofumpt)))

(use-package eglot
  :ensure nil
  :hook
  (python-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  :config
  (setq eglot-server-programs
        '( (python-ts-mode . ("pyright-langserver" "--stdio"))
           (go-ts-mode . ("gopls"))
           (typescript-ts-mode . ("typescript-language-server" "--stdio"))
           (tsx-ts-mode . ("typescript-language-server" "--stdio"))))
  (setq-default eglot-workspace-configuration
                '( :pyright ( :disableOrganizeImports t)
                   :python.analysis ( :autoSearchPaths t
                                      :useLibraryCodeForTypes t
                                      :diagnosticMode "openFilesOnly")
                   :gopls ( :gofumpt t
                            :staticcheck t
                            :completeUnimported t)))
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package pyvenv
  :ensure t)

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

(use-package go-mode
  :ensure t)

(use-package elisp-mode
  :ensure nil
  :delight (emacs-lisp-mode "Elisp" :major))

(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package typst-ts-mode
  :ensure t
  :mode (("\\.typ\\'" . typst-ts-mode))
  :config (setq typst-ts-watch-options "--open"
                typst-ts-mode-enable-raw-blocks-highlight t)
  :bind (:map typst-ts-mode-map
              ("C-c C-c" . typst-ts-menu)))

;;; Org

(use-package org
  :ensure nil
  :delight (org-indent-mode "" "org-indent")
  :commands (org-mode org-version)
  :mode (("\\.org\\'" . org-mode))
  :hook (org-mode . org-indent-mode)
  :config (setq org-M-RET-may-split-line '((default . nil))
                org-insert-heading-respect-content t
                org-log-done 'time
                org-log-into-drawer t
                org-directory "~/Developer/personal/notes/agenda/"
                org-agenda-files (list org-directory)
                org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))
  :bind
  ("C-c a" . org-agenda))

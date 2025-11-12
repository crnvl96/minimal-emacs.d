;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; Delight is a package that modify the lighter value displayed in the mode line for
;; the given mode spec.
;; It is used alongside the configuration file inside use-package declarations, to declutter the information
;; showed on mode-line
(use-package delight
  :ensure t)

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-completions '((t . (bold))))
  (modus-themes-prompts '(bold))
  (modus-themes-headings)
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-operandi-tinted t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package emacs
  :ensure nil
  :custom
  (package-install-upgrade-built-in t)
  (completion-ignore-case t)
  (compile-command nil)
  :config
  ;; We're currently using `'super-save-mode'
  (setq auto-save-default nil)

  ;; Fonts definition ---
  (set-face-attribute 'default nil :height 220 :weight 'normal :family "Iosevka")
  (set-face-attribute 'variable-pitch nil :height 220 :weight 'normal :family "Iosevka Aile")

  ;; Add a more "Fluid" mouse scroll animation ---
  (unless (and (eq window-system 'mac)
               (bound-and-true-p mac-carbon-version-string))
    (setq pixel-scroll-precision-use-momentum nil)
    (pixel-scroll-precision-mode 1))
  :bind
  ("M-n" . forward-paragraph)
  ("M-p" . backward-paragraph)
  ("C-x ;" . comment-or-uncomment-region))

(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode))

(use-package display-line-numbers
  :ensure nil
  :hook (after-init . global-display-line-numbers-mode))

(use-package time
  :ensure nil
  :hook (after-init . display-time-mode))

(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package winner
  :ensure nil
  :hook
  (after-init . winner-mode))

(use-package eldoc
  :ensure nil
  :delight)

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package whitespace
  :ensure nil
  :delight
  (whitespace-mode)
  (global-whitespace-mode)
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face trailing empty))
  (whitespace-highlight-on-current-line t))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :custom
  (save-place-limit 150))

(use-package flymake
  :ensure nil
  :bind (("C-}" . flymake-goto-next-error)
         ("C-{" . flymake-goto-prev-error)))

(use-package which-key
  :ensure nil
  :delight
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-x p" "Project"
    "C-c c" "Crux"
    "C-c f" "Find"))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        ;; - Use `my/treesit-install-all-languages' to install all languages
        ;; - Use `treesit-install-language-grammar' to install a specific language
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
          (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/ikatyang/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
          (typst "https://github.com/uben0/tree-sitter-typst")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (defun my/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75))))

  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :bind ("M-i" . avy-goto-char-2))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

(use-package super-save
  :ensure t
  :hook
  (after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace t)
  (super-save-all-buffers t)
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
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
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

(use-package crux
  :demand t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit] . crux-keyboard-quit-dwin)
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

(use-package marginalia
  :ensure t
  :hook
  (after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode)
  :bind (:map vertico-map
              ("<backspace>" . vertico-directory-delete-char)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-w" . vertico-directory-delete-word )
              ("RET" . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  ;; (after-init . corfu-popupinfo-mode)
  (minibuffer-setup . my/corfu-enable-always-in-minibuffer)
  :custom
  (corfu-cycle t)
  ;; (corfu-auto nil)
  ;; (corfu-auto-prefix 2)
  ;; (corfu-auto-delay 0.1)
  ;; (corfu-popupinfo-delay '(0.5 . 0.5))
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :bind (:map corfu-map
              ("C-e" . corfu-quit)
              ("C-i" . corfu-complete)
              ("C-j" . corfu-next)
              ("M-j" . corfu-info-documentation)
              ("C-k" . corfu-previous)
              ("M-k" . corfu-info-location)
              ("C-y" . corfu-insert))
  ;; ("C-n" . corfu-quit)
  ;; ("C-p" . corfu-quit)
  ;; ("RET" . corfu-quit))
  :config
  ;; (keymap-unset corfu-map "RET")
  (defun my/corfu-enable-always-in-minibuffer ()
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
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package embark
  :ensure t
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :custom
  (consult-async-min-input 2)
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
         ("C-c f L" . consult-goto-line))
  :config
  (setq consult-narrow-key "<")

  (consult-customize
   consult-buffer consult-yank-pop consult-fd consult-outline
   consult-imenu consult-info consult-flymake consult-history
   consult-focus-lines consult-line consult-ripgrep consult-goto-line
   :preview-key nil)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler))

(use-package magit
  :ensure t)

(use-package editorconfig
  :ensure t
  :hook
  (after-init . editorconfig-mode))

(use-package eat
  :ensure t
  :commands eat)

(use-package wgrep
  :ensure t)

(use-package buffer-terminator
  :ensure t
  :delight
  :hook
  (after-init . buffer-terminator-mode)
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 20 60))
  (buffer-terminator-interval (* 10 60)))

(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :custom
  (helpful-max-buffers 3)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable))

(use-package apheleia
  :ensure t
  :delight
  :hook ((prog-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(gofumpt)))

(use-package eglot
  :ensure nil
  :hook
  ((python-ts-mode go-ts-mode) . eglot-ensure)
  :config
  (setq eglot-server-programs
        '((python-ts-mode . ("pyright-langserver" "--stdio"))
          (go-ts-mode . ("gopls"))))
  (setq-default eglot-workspace-configuration
                '(
                  :pyright (:disableOrganizeImports t)
                  :python.analysis (:autoSearchPaths t :useLibraryCodeForTypes t :diagnosticMode "openFilesOnly")
                  :gopls (:gofumpt t)))
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package pyvenv
  :ensure t)

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
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :bind (:map typst-ts-mode-map
              ("C-c C-c" . typst-ts-menu)))

(use-package org
  :ensure nil
  :delight
  (org-indent-mode "" "org-indent")
  :commands (org-mode org-version)
  :mode (("\\.org\\'" . org-mode))
  :hook (org-mode . org-indent-mode)
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-directory "~/Developer/personal/notes/agenda/")
  (org-agenda-files (list org-directory))
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))
  :bind
  ("C-c a" . org-agenda))

;; Bench
;; Packages that are not being used at the moment (but who knows the future?)

;; (use-package paren
;;   :ensure nil
;;   :hook
;;   (after-init . show-paren-mode))

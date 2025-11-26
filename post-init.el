;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d/rc/"))

(require 'rc-delight)
(require 'rc-builtin)
(require 'rc-spacious-padding)
(require 'rc-themes)
(require 'rc-icons)
(require 'rc-dired)
(require 'rc-treesit)
(require 'rc-utilities)
(require 'rc-crux)
(require 'rc-completion)
(require 'rc-consult)

(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))

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

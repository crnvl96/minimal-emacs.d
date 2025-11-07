;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;
;; Builtin packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :custom
  (auto-save-visited-interval 5)
  (scroll-margin 8)
  (hscroll-margin 16)
  (dired-movement-style 'bounded-files)
  (tooltip-hide-delay 20)
  (tooltip-delay 0.4)
  (tooltip-short-delay 0.08)
  (package-install-upgrade-built-in t)
  :init
  (auto-save-visited-mode 1)
  (tooltip-mode 1)
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  :hook
  (after-init . display-time-mode)
  (after-init . show-paren-mode)
  (after-init . winner-mode)
  (dired-mode . dired-hide-details-mode)
  (after-init . minibuffer-depth-indicate-mode)
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (set-face-attribute 'default nil :height 120 :weight 'normal :family "Berkeley Mono"))

(use-package whitespace
  :custom
  (whitespace-style '(face trailing empty))
  (whitespace-highlight-on-current-line t)
  :init
  (global-whitespace-mode t))

;; Savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  ;; Interval between saves
  (savehist-autosave-interval 600)
  ;; List of additional variables to save
  (savehist-additional-variables
   '(kill-ring                         ; clipboard
     register-alist                    ; macros
     mark-ring global-mark-ring        ; marks
     search-ring regexp-search-ring))) ; List of regular expression search string sequences.

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  ;; Time in seconds between Auto-Revert mode checks files
  (auto-revert-interval 3)
  ;; Don't revert remote files
  (auto-revert-remote-files nil)
  ;; Use file notification functions
  (auto-revert-use-notify t)
  ;; Avoid polling when possible
  (auto-revert-avoid-polling nil)
  ;; Generate a message whenever a buffer is reverted
  (auto-revert-verbose t))

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)
  :custom
  ;; Define when to automatically clean the recentf list
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  ;; List to exclude from recentf
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  ;; Max number of entries to retain in the list (nil means no limit)
  (save-place-limit 400))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(use-package server
  :ensure nil
  :commands server-start
  :hook
  (after-init . server-start))

;;;;;;;;;;;;
;; Themes ;;
;;;;;;;;;;;;

(use-package modus-themes
  :ensure t
  :demand t
  :init
  (modus-themes-include-derivatives-mode 1)
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))
  (setq modus-themes-common-palette-overrides nil))

(use-package ef-themes
  :ensure t)

(use-package standard-themes
  :ensure t)

(use-package doric-themes
  :ensure t)

(use-package doom-themes
  :ensure t
  :custom
  ;; if nil, bold is universally disabled
  (doom-themes-enable-bold t)
  ;; if nil, italics is universally disabled
  (doom-themes-enable-italic t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'doom-city-lights t)

;;;;;;;;;;;;;;;
;; Evil Mode ;;
;;;;;;;;;;;;;;;

(setq evil-undo-system 'undo-fu)

;; Vim emulation
(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :hook (after-init . evil-mode)
  :init
  ;; It has to be defined before evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :custom
  ;; Make :s in visual mode operate only on the actual visual selection
  ;; (character or block), instead of the full lines covered by the selection
  (evil-ex-visual-char-range t)
  ;; Use Vim-style regular expressions in search and substitute commands,
  ;; allowing features like \v (very magic), \zs, and \ze for precise matches
  (evil-ex-search-vim-style-regexp t)
  ;; Enable automatic horizontal split below
  (evil-split-window-below t)
  ;; Enable automatic vertical split to the right
  (evil-vsplit-window-right t)
  ;; Disable echoing Evil state to avoid replacing eldoc
  (evil-echo-state nil)
  ;; Do not move cursor back when exiting insert state
  (evil-move-cursor-back nil)
  ;; Make `v$` exclude the final newline
  (evil-v$-excludes-newline t)
  ;; Allow C-h to delete in insert state
  (evil-want-C-h-delete t)
  ;; Enable C-u to delete back to indentation in insert state
  (evil-want-C-u-delete t)
  ;; Enable fine-grained undo behavior
  (evil-want-fine-undo t)
  ;; Allow moving cursor beyond end-of-line in visual block mode
  (evil-move-beyond-eol t)
  ;; Disable wrapping of search around buffer
  (evil-search-wrap nil)
  ;; Whether Y yanks to the end of the line
  (evil-want-Y-yank-to-eol t))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  ;; It has to be defined before evil-colllection
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;;; Which-key
(use-package which-key
  :ensure nil
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-x p" "Project"
    "C-c ." "LSP"
    "C-c f" "Find")

  (which-key-add-keymap-based-replacements global-map
    ;; General
    "M-n" '("Go to next paragraph" . forward-paragraph)
    "M-p" '("Go to previous paragraph" . backward-paragraph)

    ;; Consult
    "C-c f f" '("Find files" . consult-fd)
    "C-c f l" '("Find lines" . consult-line)
    "C-c f g" '("Grep files" . consult-ripgrep)
    "C-c f b" '("List buffers" . consult-project-buffer)
    "C-c f L" '("Go to line" . consult-goto-line)

    ;; Golden Ratio Scroll Screen
    "C-v" '("Scroll screen up" . golden-ratio-scroll-screen-up)
    "M-v" '("Scroll screen down" . golden-ratio-scroll-screen-down)

    ;; Expand Region
    "C-=" '("Expand region" . er/expand-region)

    ;; Undo-Fu
    "C-z" '("Undo" . undo-fu-only-undo)
    "C-S-z" '("Redo" . undo-fu-only-redo)

    ;; Multiple Cursors
    "C->" '("Put new cursor below" . mc/mark-next-like-this)
    "C-<" '("Put new cursor above" . mc/mark-previous-like-this)

    ;; Embark
    "C-." '("Embark act" . embark-act)
    "C-," '("Embark dwin" . embark-dwim)
    "C-/" '("Embark bindings" . embark-bindings)

    ;; Avy
    "M-i" '("Avy goto char" . avy-goto-char-2)

    ;; Ace-Window
    "M-o" '("Ace window" . ace-window)
    )

  (which-key-add-keymap-based-replacements isearch-mode-map
    "C-j" '("Exit isearch" . isearch-exit))

  (which-key-add-keymap-based-replacements corfu-map
    ;; Corfu
    "C-e" '("Abort completion menu" . corfu-quit)
    "C-i" '("Trigger completion" . corfu-complete)
    "C-y" '("Insert completion item" . corfu-insert)
    )

  (which-key-add-keymap-based-replacements typst-ts-mode-map
    ;; Typst-ts-mode
    "C-c C-c" '("Typst menu" . typst-ts-tmenu)
    ))

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(use-package magit
  :ensure t)

(use-package eat
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

;;;;;;;;;;;;;;;;;;;;;;
;; Language Support ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Treesitter
;; `crnvl96/treesit-install-all-languages' to install all languages
;; `treesit-install-language-grammar' to install a specific language
;; use `sort-lines' to sort
(setq treesit-language-source-alist
      '(
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.0"))
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
        (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/ikatyang/tree-sitter-toml")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (typst "https://github.com/uben0/tree-sitter-typst")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        ))

(defun crnvl96/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	  (treesit-install-language-grammar lang)
	  (message "`%s' parser was installed." lang)
	  (sit-for 0.75))))

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c .")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  :hook ((python-ts-mode . lsp)
         (go-ts-mode . lsp)
         (lsp-mode . lsp-ui-mode)
         (lsp-mode . lsp-lens-mode)
         (lsp-completion-mode . crnvl96/corfu-setup-lsp)) ; Use corfu instead the default for lsp completions
  :commands lsp
  :custom
  (lsp-completion-provider :none) ; Use corfu instead the default for lsp completions
  :config
  (defun crnvl96/corfu-setup-lsp ()
    "Use orderless and corfu completion style with lsp-capf instead of the
               default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-mouse nil))

(use-package lsp-pyright
  :ensure t)

;; Formatter
(use-package apheleia
  :ensure t
  :commands (apheleia-mode apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(gofumpt)))

;; Python
(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

(use-package pyvenv
  :ensure t)

;; Go
(use-package go-mode
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;; Typst
(use-package typst-ts-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t))

;;Org
(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-startup-truncated t))

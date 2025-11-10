;;; core.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :delight
  (eldoc-mode)
  (emacs-lisp-mode "Elisp" :major)
  :hook
  (after-init . display-line-numbers-mode)
  (after-init . display-time-mode)
  (after-init . global-auto-revert-mode)
  (after-init . global-hl-line-mode)
  (after-init . global-whitespace-mode)
  (after-init . recentf-mode)
  (after-init . save-place-mode)
  (after-init . savehist-mode)
  (after-init . show-paren-mode)
  (after-init . delete-selection-mode)
  :custom
  ;; This setting can be used together with `use-package-report' to profile emacs startup time
  ;; (use-package-compute-statistics t)
  (scroll-margin 8)
  (hscroll-margin 16)
  (whitespace-style '(face trailing empty))
  (whitespace-highlight-on-current-line t)
  (package-install-upgrade-built-in t)
  (save-place-limit 150)
  :config
  (setq-default display-line-numbers-type 'relative)
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90)
  (set-face-attribute 'default nil :height 200 :weight 'normal :family "Berkeley Mono"))

(use-package eww
  :ensure nil
  :commands eww)

(use-package which-key
  :ensure nil
  :delight
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-x p" "Project"
    "C-c ." "LSP"
    "C-c g" "Magit"
    "C-c f" "Find")
  :bind
  ("M-n" . forward-paragraph)
  ("M-p" . backward-paragraph))

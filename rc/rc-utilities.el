;;; -*- lexical-binding: t; -*-

;; (use-package ace-window
;;   :ensure t
;;   :bind ("M-o" . ace-window))

;; (use-package avy
;;   :ensure t
;;   :config
;;   (setq avy-keys (number-sequence ?a ?y))
;;   :bind (("M-i" . avy-goto-char-2)
;;          ("C-d" . avy-goto-word-0)))

;; (use-package expand-region
;;   :ensure t
;;   :bind (("C-=" . er/expand-region)))

(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode))

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; (use-package buffer-terminator
;;   :ensure t
;;   :delight
;;   :hook (after-init . buffer-terminator-mode)
;;   :config (setq buffer-terminator-verbose nil
;;                 buffer-terminator-inactivity-timeout (* 20 60)
;;                 buffer-terminator-interval (* 20 60)))

;; (use-package super-save
;;   :ensure t
;;   :delight
;;   :hook (after-init . super-save-mode)
;;   :init (setq auto-save-default nil)
;;   :config (setq super-save-auto-save-when-idle t
;;                 super-save-delete-trailing-whitespace t
;;                 super-save-all-buffers t)
;;   (add-to-list 'super-save-triggers 'ace-window)
;;   (add-to-list 'super-save-hook-triggers 'find-file-hook))

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

(provide 'rc-utilities)

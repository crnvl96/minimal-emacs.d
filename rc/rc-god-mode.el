;;; -*- lexical-binding: t; -*-

(use-package god-mode
  :ensure t
  :hook (after-init . god-mode-all)
  :init
  (setq god-mode-enable-function-key-translation nil)
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)

  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)

  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (global-set-key (kbd "<escape>") #'(lambda () (interactive) (god-local-mode 1)))
  (define-key god-local-mode-map (kbd ".") #'repeat)

  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-window-below)
  (global-set-key (kbd "C-x C-3") #'split-window-right)
  (global-set-key (kbd "C-x C-0") #'delete-window)

  (custom-set-faces
   '(god-mode-lighter ((t (:inherit error)))))

  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))

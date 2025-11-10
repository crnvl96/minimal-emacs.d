;;; editor.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; (use-package ace-window
;;   :ensure t
;;   :bind ("M-o" . ace-window))

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

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit] . crux-keyboard-quit-dwin)
         ([remap upcase-region] . crux-upcase-region)
         ([remap downcase-region] . crux-downcase-region)
         ("C-^" . crux-top-join-line)
         ("C-6" . crux-other-window-or-switch-buffer)
         ("C-k" . crux-smart-kill-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c x" . crux-transpose-windows)
         ("C-c o" . crux-open-with)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c r" . crux-recentf-find-file)
         ("C-c R" . crux-recentf-find-directory)
         ("C-c k" . crux-kill-other-buffers))
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

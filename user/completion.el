;;; user/completion.el --- Completion files -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle nil)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Correct file path when changed
         (minibuffer-setup . vertico-repeat-save))
  :bind (:map vertico-map
              ("<escape>" . minibuffer-keyboard-quit)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-q" . vertico-exit)
              ("M-." . vertico-repeat)
              ("<backspace>" . vertico-directory-delete-char)
              ("C-w" . vertico-directory-delete-word)
              ("C-<backspace>" . vertico-directory-delete-word)
              ("RET" . vertico-directory-enter)
              ("C-i" . vertico-quick-insert)
              ("C-o" . vertico-quick-exit)
              ("C-l" . vertico-insert)))

(use-package orderless
  :ensure t
  :custom
  (orderless-style-dispatchers '(orderless-affix-dispatch))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)) ;; Disable defaults

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect nil)
  (corfu-auto t)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<escape>" . corfu-quit)
              ("C-g" . corfu-quit)
              ("C-e" . corfu-quit)
              ("RET" . corfu-insert)
              ("C-y" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location)
              ; ("SPC" . corfu-insert-separator)
              ("C-i" . corfu-quick-complete))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

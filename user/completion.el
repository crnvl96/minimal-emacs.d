;;; user/completion.el --- Completion files -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  ;; Substitute TAB with M-n for accepting suggestions in minibuffer
  (define-key vertico-map (kbd "<tab>") nil)
  (define-key vertico-map (kbd "C-,") #'vertico-insert))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (orderless-style-dispatchers '(orderless-affix-dispatch))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)) ;; Disable defaults, use our settings

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect nil)
  :bind ((:map corfu-map ("SPC" . corfu-insert-separator))
         (:map corfu-map ("M-n" . corfu-quick-complete))
         (:map corfu-map ("C-n" . corfu-quick-insert)))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;;; user/emacs.el --- Emacs files -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1)
  (setq-default scroll-margin 0)

  (define-minor-mode crnvl96/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if crnvl96/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  :bind ("C-c L" . crnvl96/scroll-centre-cursor-mode))

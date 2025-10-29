;;; user/opts.el --- Opts files -*- no-byte-compile: t; lexical-binding: t; -*-

(set-face-attribute 'default nil
                    :height 200 :weight 'normal :family "Berkeley Mono")

(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(global-hl-line-mode 1)
(global-visual-line-mode t)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

(setq package-install-upgrade-built-in t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

(setq-default display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

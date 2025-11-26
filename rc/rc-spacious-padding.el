;;; -*- lexical-binding: t; -*-

(use-package spacious-padding
  :ensure t
  :demand t
  :hook
  (after-init . spacious-padding-mode)
  :config (setq spacious-padding-widths
                '( :internal-border-width 15
                   :header-line-width 4
                   :mode-line-width 6
                   :tab-width 4
                   :right-divider-width 30
                   :scroll-bar-width 8
                   :fringe-width 8)
                spacious-padding-subtle-frame-lines
                `( :mode-line-active 'default
                   :mode-line-inactive vertical-border)))

(provide 'rc-spacious-padding)

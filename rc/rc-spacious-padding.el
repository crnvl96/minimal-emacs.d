;;; -*- lexical-binding: t; -*-

(require 'spacious-padding)

(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

(setq spacious-padding-subtle-frame-lines
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border))

(add-hook 'after-init-hook #'spacious-padding-mode)

(provide 'rc-spacious-padding)

;;; -*- lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :demand t
  :init (modus-themes-include-derivatives-mode 1)
  :config (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
                modus-themes-to-rotate modus-themes-items
                modus-themes-mixed-fonts t
                modus-themes-variable-pitch-ui t
                modus-themes-italic-constructs t
                modus-themes-bold-constructs t
                modus-themes-completions '((t . (bold)))
                modus-themes-prompts '(bold)
                modus-themes-common-palette-overrides nil
                modus-themes-headings '((agenda-structure . (variable-pitch light 2.2))
                                        (agenda-date . (variable-pitch regular 1.3))
                                        (t . (regular 1.15)))))

;; (use-package ef-themes
;;   :ensure t
;;   :demand t)

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi t)

(provide 'rc-themes)

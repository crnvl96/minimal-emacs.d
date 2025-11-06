;;; themes.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :demand t
  :init
  (modus-themes-include-derivatives-mode 1)
  :config
  (;; enable inheritance from `fixed-pitch' in some faces
   setq modus-themes-mixed-fonts t
   ;; Use proportional fonts (variable-pitch) in UI elements
   modus-themes-variable-pitch-ui t
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t
   modus-themes-completions '((t . (bold)))
   modus-themes-prompts '(bold)
   modus-themes-headings
   '((agenda-structure . (variable-pitch light 2.2))
     (agenda-date . (variable-pitch regular 1.3))
     (t . (regular 1.15))))
  (setq modus-themes-common-palette-overrides nil))

(use-package ef-themes
  :ensure t)

(use-package standard-themes
  :ensure t)

(use-package doric-themes
  :ensure t)

(use-package doom-themes
  :ensure t
  :custom
  ;; if nil, bold is universally disabled
  (doom-themes-enable-bold t)
  ;; if nil, italics is universally disabled
  (doom-themes-enable-italic t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'doric-pine t)

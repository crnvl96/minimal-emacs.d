;;; which-key.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package which-key
  :ensure nil
  :delight
  :hook (after-init . which-key-mode)
  :config (which-key-add-key-based-replacements
            "C-x p" "Project"
            "C-c c" "Crux"
            "C-c f" "Find"))

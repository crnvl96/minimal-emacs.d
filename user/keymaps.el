;;; keymaps.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package which-key
  :ensure nil
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-x p" "Project"
    "C-c ." "LSP"
    "C-c f" "Find")

  (which-key-add-keymap-based-replacements global-map
    ;; General
    "M-n" '("Go to next paragraph" . forward-paragraph)
    "M-p" '("Go to previous paragraph" . backward-paragraph)

    ;; Consult
    "C-c f f" '("Find files" . consult-fd)
    "C-c f l" '("Find lines" . consult-line)
    "C-c f g" '("Grep files" . consult-ripgrep)
    "C-c f b" '("List buffers" . consult-project-buffer)
    "C-c f L" '("Go to line" . consult-goto-line)

    ;; Golden Ratio Scroll Screen
    "C-v" '("Scroll screen up" . golden-ratio-scroll-screen-up)
    "M-v" '("Scroll screen down" . golden-ratio-scroll-screen-down)

    ;; Expand Region
    "C-=" '("Expand region" . er/expand-region)

    ;; Undo-Fu
    "C-z" '("Undo" . undo-fu-only-undo)
    "C-S-z" '("Redo" . undo-fu-only-redo)

    ;; Multiple Cursors
    "C->" '("Put new cursor below" . mc/mark-next-like-this)
    "C-<" '("Put new cursor above" . mc/mark-previous-like-this)

    ;; Embark
    "C-." '("Embark act" . embark-act)
    "C-," '("Embark dwin" . embark-dwim)
    "C-/" '("Embark bindings" . embark-bindings)

    ;; Avy
    "M-i" '("Avy goto char" . avy-goto-char-2)

    ;; Ace-Window
    "M-o" '("Ace window" . ace-window)
    )

  (which-key-add-keymap-based-replacements isearch-mode-map
    "C-j" '("Exit isearch" . isearch-exit))

  (which-key-add-keymap-based-replacements corfu-map
    ;; Corfu
    "C-e" '("Abort completion menu" . corfu-quit)
    "C-i" '("Trigger completion" . corfu-complete)
    "C-y" '("Insert completion item" . corfu-insert)
    )

  (which-key-add-keymap-based-replacements typst-ts-mode-map
    ;; Typst-ts-mode
    "C-c C-c" '("Typst menu" . typst-ts-tmenu)
    ))

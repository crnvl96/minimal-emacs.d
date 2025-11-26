;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d/rc/"))

(require 'rc-delight)
(require 'rc-builtin)
(require 'rc-spacious-padding)
(require 'rc-themes)
(require 'rc-icons)
(require 'rc-dired)
(require 'rc-treesit)
(require 'rc-utilities)
(require 'rc-crux)
(require 'rc-nov)
(require 'rc-completion)
(require 'rc-consult)
(require 'rc-rg)
(require 'rc-magit)
(require 'rc-apheleia)
(require 'rc-eglot)
(require 'rc-python)
(require 'rc-json)
(require 'rc-go)
(require 'rc-elisp)
(require 'rc-markdown)
(require 'rc-typst)
(require 'rc-org)

(use-package devil
  :ensure t
  :hook (after-init . global-devil-mode)
  :config
  (assoc-delete-all "%k SPC" devil-special-keys)
  (devil-set-key (kbd "j")))

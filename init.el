;;; init.el --- Init -*- lexical-binding: t; -*-

;; Initialize and refresh package contents again if needed
(package-initialize)
;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (unless (seq-empty-p package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

;; Ensure use-package is available
(require 'use-package)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/rc/"))

;; Core settings
(require 'rc-delight)
(require 'rc-opts)
(require 'rc-fonts)
(require 'rc-keymaps)
(require 'rc-which-key)
(require 'rc-theme)

;; File management
(require 'rc-dired)
(require 'rc-trashed)
(require 'rc-treesit)

;; Navigation
(require 'rc-ace-window)
(require 'rc-avy)
(require 'rc-expand-region)

;; Editing
(require 'rc-editorconfig)
(require 'rc-mise)
(require 'rc-exec-path-from-shell)
(require 'rc-buffer-terminator)
(require 'rc-super-save)
(require 'rc-undo-fu)
(require 'rc-helpful)
(require 'rc-crux)

;; Reading
(require 'rc-nov)

;; Completion
(require 'rc-completion)
(require 'rc-consult)
(require 'rc-rg)

;; Version control
(require 'rc-magit)

;; Code formatting
(require 'rc-apheleia)

;; LSP
(require 'rc-eglot)

;; Languages
(require 'rc-python)
(require 'rc-json)
(require 'rc-go)
(require 'rc-elisp)
(require 'rc-markdown)
(require 'rc-typst)
(require 'rc-org)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((typst-ts-mode :url
                    "https://codeberg.org/meow_king/typst-ts-mode.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

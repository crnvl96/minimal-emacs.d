;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(minimal-emacs-load-user-init "user/core.el")
(minimal-emacs-load-user-init "user/ui.el")
(minimal-emacs-load-user-init "user/icons.el")
(minimal-emacs-load-user-init "user/dired.el")
(minimal-emacs-load-user-init "user/which-key.el")
(minimal-emacs-load-user-init "user/treesit.el")
(minimal-emacs-load-user-init "user/code.el")
(minimal-emacs-load-user-init "user/utils.el")
(minimal-emacs-load-user-init "user/completion.el")
(minimal-emacs-load-user-init "user/consult.el")
(minimal-emacs-load-user-init "user/magit.el")
(minimal-emacs-load-user-init "user/lang.el")
(minimal-emacs-load-user-init "user/org.el")

;; Bench
;; Packages that are not being used at the moment (but who knows?)

;; (use-package paren
;;   :ensure nil
;;   :hook
;;   (after-init . show-paren-mode))

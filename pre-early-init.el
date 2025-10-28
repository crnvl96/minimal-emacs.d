;;; pre-early-init.el --- Pre Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Reducing clutter in ~/.emacs.d by redirecting files to ~/.emacs.d/var/
(setq user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))


;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; pre-early-init.el ends here

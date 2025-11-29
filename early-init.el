;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

(defvar user-data-directory user-emacs-directory
  "Directory beneath which Emacs data files are placed.")

;;; Reducing clutter in ~/.emacs.d by redirecting files to ~/.emacs.d/var/
(setq user-emacs-directory (expand-file-name "var/" user-data-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Performance optimizations
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB
(setq file-name-handler-alist nil)

(setq redisplay-dont-pause t)
(setq inhibit-compacting-font-caches t)
(setq comp-eln-load-path (list (expand-file-name (format "eln-cache/%s/" emacs-version) user-emacs-directory)))

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (let ((msg (format "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     (time-to-seconds (time-since before-init-time))
                     gcs-done)))
    (message msg)))

(add-hook 'emacs-startup-hook #'display-startup-time 100)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))  ; 20MB
            (setq file-name-handler-alist file-name-handler-alist-original))
          101)

(setq inhibit-splash-screen t)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq tooltip-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq use-package-compute-statistics nil)

(setq use-package-expand-minimally t)
(setq package-quickstart-file
      (expand-file-name "package-quickstart.el" user-emacs-directory))
(setq package-enable-at-startup nil)  ; Let the init.el file handle this
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(setq package-archive-priorities '(("melpa"        . 90)
                                   ("gnu"          . 70)
                                   ("nongnu"       . 60)
                                   ("melpa-stable" . 50)))

;; Security: enforce TLS verification
(setq gnutls-verify-error t)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here

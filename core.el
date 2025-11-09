;;; core.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :hook
  (after-init . minibuffer-depth-indicate-mode)
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :custom
  ;; This setting can be used together with `use-package-report' to profile emacs startup time
  (use-package-compute-statistics nil)
  (scroll-margin 8)
  (hscroll-margin 16)
  (package-install-upgrade-built-in t)
  :config
  (setq-default display-line-numbers-type 'relative)
  (set-face-attribute 'default nil :height 160 :weight 'normal :family "Berkeley Mono"))

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-movement-style bounded-files)
  (dired-movement-style 'bounded-files)
  (dired-omit-files (concat "\\`[.]\\'"
                            "\\|\\(?:\\.js\\)?\\.meta\\'"
                            "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                            "\\|^\\.DS_Store\\'"
                            "\\|^\\.\\(?:svn\\|git\\)\\'"
                            "\\|^\\.ccls-cache\\'"
                            "\\|^__pycache__\\'"
                            "\\|^\\.project\\(?:ile\\)?\\'"
                            "\\|^flycheck_.*"
                            "\\|^flymake_.*"))
  (add-hook 'dired-mode-hook #'dired-omit-mode))

(with-eval-after-load 'dired
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args))))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

(use-package show-paren
  :ensure nil
  :hook (after-init . show-paren-mode))

(use-package display-time
  :ensure nil
  :hook (after-init . display-time-mode))

(use-package global-hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package delete-selection
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package auto-save-visited
  :ensure nil
  :hook (after-init . auto-save-visited-mode)
  :custom (auto-save-visited-interval 5))

(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :custom
  (tooltip-hide-delay 20)
  (tooltip-delay 0.4)
  (tooltip-short-delay 0.08))

(use-package eww
  :ensure nil
  :commands eww)

(use-package whitespace
  :ensure nil
  :hook
  (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face trailing empty))
  (whitespace-highlight-on-current-line t))

(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring
	 register-alist
	 mark-ring global-mark-ring
	 search-ring regexp-search-ring)))

(use-package autorevert
  :ensure nil
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

(use-package recentf
  :ensure nil
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
		 "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
		 "\\.7z$" "\\.rar$"
		 "COMMIT_EDITMSG\\'"
		 "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
		 "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package saveplace
  :ensure nil
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(use-package server
  :ensure nil
  :hook
  (after-init . server-start))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-x p" "Project"
    "C-c ." "LSP"
    "C-c g" "Magit"
    "C-c f" "Find")
  :bind
  ("M-n" . forward-paragraph)
  ("M-p" . backward-paragraph))

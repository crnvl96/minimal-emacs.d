;;; user/emacs.el --- Emacs files -*- no-byte-compile: t; lexical-binding: t; -*-

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
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
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

;; Configure the built-in Emacs server to start after initialization,
;; allowing the use of the emacsclient command to open files in the
;; current session.
(use-package server
  :ensure nil
  :commands server-start
  :hook
  (after-init . server-start))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package emacs
  :config
  (set-face-attribute 'default nil
                      :height 200 :weight 'normal :family "Berkeley Mono")
  
  ;; When Delete Selection mode is enabled, typed text replaces the selection
  ;; if the selection is active.
  (delete-selection-mode 1)
  
  ;; Allow Emacs to upgrade built-in packages, such as Org mode
  (setq package-install-upgrade-built-in t)

  ;; Display of line numbers in the buffer:
  ;; (setq-default display-line-numbers-type 'relative)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'display-line-numbers-mode))

  ;; Display the time in the modeline
  (add-hook 'after-init-hook #'display-time-mode)

  ;; Paren match highlighting
  (add-hook 'after-init-hook #'show-paren-mode)

  ;; Track changes in the window configuration, allowing undoing actions such as
  ;; closing windows.
  (add-hook 'after-init-hook #'winner-mode)

  (use-package uniquify
    :ensure nil
    :custom
    (uniquify-buffer-name-style 'reverse)
    (uniquify-separator "•")
    (uniquify-after-kill-buffer-p t))

  ;; Window dividers separate windows visually. Window dividers are bars that can
  ;; be dragged with the mouse, thus allowing you to easily resize adjacent
  ;; windows.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
  (add-hook 'after-init-hook #'window-divider-mode)

  ;; Constrain vertical cursor movement to lines within the buffer
  (setq dired-movement-style 'bounded-files)

  ;; Dired buffers: Automatically hide file details (permissions, size,
  ;; modification date, etc.) and all the files in the `dired-omit-files' regular
  ;; expression for a cleaner display.
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  ;; Hide files from dired
  (setq dired-omit-files (concat "\\`[.]\\'"
                                 "\\|\\(?:\\.js\\)?\\.meta\\'"
                                 "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                                 "\\|^\\.DS_Store\\'"
                                 "\\|^\\.\\(?:svn\\|git\\)\\'"
                                 "\\|^\\.ccls-cache\\'"
                                 "\\|^__pycache__\\'"
                                 "\\|^\\.project\\(?:ile\\)?\\'"
                                 "\\|^flycheck_.*"
                                 "\\|^flymake_.*"))
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  ;; dired: Group directories first
  (with-eval-after-load 'dired
    (let ((args "--group-directories-first -ahlv"))
      (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
        (if-let* ((gls (executable-find "gls")))
            (setq insert-directory-program gls)
          (setq args nil)))
      (when args
        (setq dired-listing-switches args))))

  ;; Enables visual indication of minibuffer recursion depth after initialization.
  (add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)

  ;; When tooltip-mode is enabled, certain UI elements (e.g., help text,
  ;; mouse-hover hints) will appear as native system tooltips (pop-up windows),
  ;; rather than as echo area messages. This is useful in graphical Emacs sessions
  ;; where tooltips can appear near the cursor.
  (setq tooltip-hide-delay 20)    ; Time in seconds before a tooltip disappears (default: 10)
  (setq tooltip-delay 0.4)        ; Delay before showing a tooltip after mouse hover (default: 0.7)
  (setq tooltip-short-delay 0.08) ; Delay before showing a short tooltip (Default: 0.1)
  (tooltip-mode 1)

  (define-minor-mode crnvl96/scroll-center-cursor-mode
    "Toggle centerd cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if crnvl96/scroll-center-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  :bind ("C-c L" . crnvl96/scroll-center-cursor-mode))

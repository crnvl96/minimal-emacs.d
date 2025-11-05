;;; options.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; Options and variables

;; Display current time on modeline
(add-hook 'after-init-hook #'display-time-mode)

;; Highlight matching paren
(add-hook 'after-init-hook #'show-paren-mode)

;; Track changes on windows configuration
(add-hook 'after-init-hook #'winner-mode)

;; auto-save-visited-mode only saves file-visiting buffers
;; after a period of idle time, directly saving to the file itself without
;; creating backup files.
(setq auto-save-visited-interval 5) ; Save after 5 seconds of inactivity
(auto-save-visited-mode 1)

;; Scrolloff
(setq scroll-margin 8)   ; Vertical
(setq hscroll-margin 16) ; Horizontal

;; Automatically hide file details (permissions, size, modification date, etc.)
;; and all the files in the `dired-omit-files' regular expression for a cleaner display.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
;; Constrain vertical cursor movement
(setq dired-movement-style 'bounded-files)

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

;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.
(delete-selection-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

;; Always display line numbers
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(set-face-attribute 'default nil
                    :height 120 :weight 'normal :family "Berkeley Mono")

;; Builtin Packages
;;
;; Configure some packages that are already builtin into Emacs, such
;; as savehist, autorevert, etc.

;; Savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  ;; Interval between saves
  (savehist-autosave-interval 600)
  ;; List of additional variables to save
  (savehist-additional-variables
   '(kill-ring                         ; clipboard
     register-alist                    ; macros
     mark-ring global-mark-ring        ; marks
     search-ring regexp-search-ring))) ; List of regular expression search string sequences.

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  ;; Time in seconds between Auto-Revert mode checks files
  (auto-revert-interval 3)
  ;; Don't revert remote files
  (auto-revert-remote-files nil)
  ;; Use file notification functions
  (auto-revert-use-notify t)
  ;; Avoid polling when possible
  (auto-revert-avoid-polling nil)
  ;; Generate a message whenever a buffer is reverted
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
  ;; Define when to automatically clean the recentf list
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  ;; List to exclude from recentf
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

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  ;; Max number of entries to retain in the list (nil means no limit)
  (save-place-limit 400))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(define-key isearch-mode-map (kbd "C-j") 'isearch-exit)

(use-package server
  :ensure nil
  :commands server-start
  :hook
  (after-init . server-start))

;;; -*- lexical-binding: t; -*-

(use-package treesit-auto
  :ensure t
  :hook (after-init . global-treesit-auto-mode)
  :config
  (setq my-json-tsauto-config
        (make-treesit-auto-recipe
         :lang 'json
         :ts-mode 'json-ts-mode
         :remap '(json-mode)
         :url "https://github.com/tree-sitter/tree-sitter-json"
         :revision "master"
         :source-dir "src"
         :ext "\\.json\\'"))
  (add-to-list 'treesit-auto-recipe-list my-json-tsauto-config)
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all))

(provide 'rc-treesit)

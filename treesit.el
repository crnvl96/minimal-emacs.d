;;; treesit.el -*- no-byte-compile: t; lexical-binding: t; -*-

(setq treesit-language-source-alist
      ;; - Use `crnvl96/treesit-install-all-languages' to install all languages
      ;; - Use `treesit-install-language-grammar' to install a specific language
      ;; - Use `sort-lines' to sort
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	    (go "https://github.com/tree-sitter/tree-sitter-go")
	    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	    (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.0"))
	    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
	    (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
	    (json "https://github.com/tree-sitter/tree-sitter-json")
	    (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
	    (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
	    (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
	    (python . ("https://github.com/tree-sitter/tree-sitter-python"))
	    (rust "https://github.com/tree-sitter/tree-sitter-rust")
	    (toml "https://github.com/ikatyang/tree-sitter-toml")
	    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
	    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
	    (typst "https://github.com/uben0/tree-sitter-typst")
	    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun crnvl96/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75))))

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

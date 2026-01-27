;; -*- lexical-binding: t; -*-

(use-package eglot
  :ensure
  :hook
  (c-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (haskell-mode . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

  (defun kjs-eglot-format-dwim ()
    "Format region if active, otherwise buffer"
    (interactive)
    (if (use-region-p)
        (eglot-format (region-beginning) (region-end))
      (eglot-format-buffer)))

    :bind (("C-c C-a" . eglot-code-actions)))


(use-package consult-eglot
  :ensure
  :bind (("C-c C-s" . consult-eglot-symbols)))


(use-package treesit
  :preface
  (defun kjs-ts-url (proj)
    (concat
     "https://github.com/tree-sitter/tree-sitter-"
     proj))
  (defun kjs-setup-install-grammars ()
    "From https://github.com/mickeynp/combobulate"
    (interactive)
    (dolist (grammar
             `((c . (,(kjs-ts-url "c") "v0.23.5"))
               (cmake . ("https://github.com/uyha/tree-sitter-cmake" "v0.7.2"))
               (cpp . (,(kjs-ts-url "cpp") "v0.23.4"))
               (css . (,(kjs-ts-url "css") "v0.20.0"))
               (go . (,(kjs-ts-url "go") "v0.20.0"))
               (html . (,(kjs-ts-url "html") "v0.20.1"))
               (javascript . (,(kjs-ts-url "javascript") "v0.20.1" "src"))
               (json . (,(kjs-ts-url "json") "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . (,(kjs-ts-url "python") "v0.20.4"))
               (rust . (,(kjs-ts-url "rust") "v0.21.2"))
               (toml . (,(kjs-ts-url "toml") "v0.5.1"))
               (tsx . (,(kjs-ts-url "typescript") "v0.20.3" "tsx/src"))
               (typescript . (,(kjs-ts-url "typescript") "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             ))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (kjs-setup-install-grammars))


(provide 'kjs-lsp)

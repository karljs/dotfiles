;;  -*- lexical-binding: t; -*-

(use-package magit
  :ensure
  :bind (
	 ("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c M-g" . magit-file-dispatch)))


(use-package compiler-explorer
  :ensure
  :custom
  (compiler-explorer-output-filters
   '(:binary nil :binaryObject nil :commentOnly t :demangle t :directives
             t :intel nil :labels t :libraryCode t :trim nil
             :debugCalls nil)))


(use-package paredit
  :ensure
  :diminish paredit-mode
  :config
  ;; this interferes with my preferred consult keys
  (eval-after-load "paredit"
    '(progn
       (define-key paredit-mode-map (kbd "M-s") nil)))
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (racket-mode . enable-paredit-mode)
  (racket-hash-lang-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (minibuffer-setup . disable-paredit-mode))


(use-package paredit-everywhere
  :ensure t
  :after paredit
  :diminish paredit-everywhere-mode
  :config
  (eval-after-load "paredit-everywhere"
    '(progn
       (define-key paredit-everywhere-mode-map (kbd "M-s") nil)))
  :hook
  (prog-mode . paredit-everywhere-mode))


(use-package dpkg-dev-el
  :ensure t)


(use-package haskell-mode
  :ensure t)


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))


(use-package rust-mode
  :after eglot
  :preface
  :ensure
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save nil)
  ;; This tells rust-analyzer to use clippy, and to search all kinds
  ;; of symbols since for some reason the default is only types.
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer"
                  :initializationOptions
                  (:check
                   (:command "clippy")
                   :workspace
                   (:symbol (:search (:kind "all_symbols"))))))))


(use-package racket-mode
  :ensure t)


(use-package geiser
  :ensure)


(use-package geiser-mit
  :ensure
  :after geiser)


(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t
        org-latex-compiler 'lualatex)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))


(use-package yaml-mode
  :ensure
  :mode "\\.yml\\'")


(use-package python-mode
  :ensure)


(use-package cc-mode
  :preface
  (defun kjs--insert-ifdef-guards ()
    "Insert #ifdef guards for C/C++ header files based on the buffer name"
    (interactive)
    (let* ((project (projectile-project-name))
           (name (file-name-base (buffer-file-name)))
           (symbol (concat project "_" name "_h")))
      (progn
        (insert (concat "#ifndef " symbol))
        (end-of-line)
        (newline)
        (insert (concat  "#define " symbol))
        (end-of-line)
        (newline)
        (newline)
        (insert "#endif")
        (previous-line))))

  :config
  (setq c-ts-mode-indent-offset 4)
  (setq c-ts-mode-indent-style 'linux)
  )


(provide 'kjs-prog)

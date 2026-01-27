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

(use-package c-ts-mode
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c++-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hxx\\'" . c++-ts-mode))
  :hook ((c-ts-mode . kjs-c-ts-commont-setup)
         (c++-ts-mode . kjs-c-ts-common-setup))
  :init
  (defun kjs-c-ts-common-setup ()
    "Shared setup for C and C++"
    (electric-indent-local-mode -1)
    (setq-local indent-line-function #'kjs-indent-to-next-multiple)
    (local-set-key (kbd "<backtab>") #'kjs-dedent-to-prev-multiple)
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (setq-local post-self-insert-hook (list t)))
              nil t)
    (eglot-ensure)
    (add-hook 'before-save-hook #'kjs-eglot-format-on-save))

  (defun kjs-eglot-format-on-save ()
    (when (bound-and-true-p eglot--managed-mode)
      (eglot-format-buffer)))

  (defun kjs-c-ts-bind-keys (map)
    (define-key map (kbd "C-c f") #'kjs-eglot-format-dwim)
    (define-key map (kbd "C-c F") #'eglot-format-buffer))

  (defun kjs-indent-to-next-multiple ()
    (interactive)
    (let ((col (save-excursion (back-to-indentation) (current-column))))
      (if (<= (current-column) col)
          ;; Point is in leading whitespace, indent the line
          (let ((cur (current-indentation)))
            (indent-line-to (+ cur (- 4 (mod cur 4)))))
        ;; Point is after text, signal no indent happened
        nil)))


  (defun kjs-dedent-to-prev-multiple ()
    (interactive)
    (let ((cur (current-indentation)))
      (indent-line-to (max 0 (- cur (if (zerop (mod cur 4)) 4 (mod cur 4)))))))

  :config
  (kjs-c-ts-bind-keys c-ts-mode-map)
  (kjs-c-ts-bind-keys c++-ts-mode-map))


(use-package cmake-ts-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-ts-mode . kjs-cmake-maybe-eglot)
  :init
  (defun kjs-cmake-maybe-eglot ()
    (require 'eglot)
    (add-to-list 'eglot-server-programs `((cmake-ts-mode) . ("neocmakelsp" "stdio")))
    (eglot-ensure)))


(provide 'kjs-prog)

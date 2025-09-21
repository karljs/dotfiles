;;
;; _____       __________      ______
;; ___(_)_________(_)_  /_________  /
;; __  /__  __ \_  /_  __/_  _ \_  /
;; _  / _  / / /  / / /___/  __/  /
;; /_/  /_/ /_//_/  \__/(_)___//_/
;;



;;------------------------------------------------------------------------------
;; Setup package manager and bootstrap use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;;------------------------------------------------------------------------------
;; Global settings, environment, cleanup
(use-package emacs
  :demand t
  :init
  (setq gc-cons-percentage 0.5
        gc-cons-threshold (* 128 1024 1024))

  ;; Customize is frustrating
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Aesthetics
  (let ((font-size
         (if (eq system-type 'darwin)
             170
           140)))
    (set-face-attribute 'default nil
		        :font "PragmataPro"
		        :height font-size))
  (tool-bar-mode -1)
  (set-scroll-bar-mode nil)
  (when (not (eq system-type 'darwin))
    (menu-bar-mode -1))
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode)

  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'none
          dired-use-ls-dired nil))

  ;; Basic behavior changes
  (setq save-interprogram-paste-before-kill t)
  (setq yank-pop-change-selection t)
  (setq help-window-select t)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (setq-default indent-tabs-mode nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

  ;; Global keybinds
  :bind (([remap list-buffers] . ibuffer)
         ("C-M-y" . duplicate-dwim)
         ([remap zap-to-char] . zap-up-to-char)))


(use-package no-littering
  :ensure
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
	`(("." . ,(no-littering-expand-var-file-name "backups/")))))


(use-package exec-path-from-shell
  :ensure
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))




;;------------------------------------------------------------------------------
;; LSP, treesitter

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
  (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
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
             (c++-mode . c++-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (kjs-setup-install-grammars))


;;------------------------------------------------------------------------------
;; Vertico, Marginalia, Ordlerless, Consult, and Embark (all inter-related)
(use-package vertico
  :ensure
  :init
  (vertico-mode))

(use-package marginalia
  :ensure
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure
  :config
  (setq orderless-matching-styles '(orderless-literal orderless-initialism orderless-flex)
	completion-styles '(orderless basic)
        ;; This is a workaround for TRAMP hostname completion
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure
  :after perspective
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)

         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop

         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)

         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-narrow-key "<")

  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C->" . embark-export)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure)

;;------------------------------------------------------------------------------
;; Completions

(use-package corfu
  :ensure
  :init
  (global-corfu-mode)
  :config
  (setq tab-always-indent 'complete))

(use-package cape
  :ensure
  :bind ("C-<tab>" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package bash-completion
  :ensure
  :config
  (bash-completion-setup))


;;------------------------------------------------------------------------------
;; General minor improvements, navigation

(use-package perspective
  :ensure
  ;; :bind
  ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))


(use-package eldoc-box
  :ensure)

(use-package helpful
  :ensure
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package ace-window
  :ensure
  :bind ("M-o". ace-window))

(use-package avy
  :ensure
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char))

(use-package change-inner
  :ensure
  :bind (("M-i" . change-inner)))

(use-package jinx
  :ensure
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-\"" . jinx-correct)))

(use-package vterm
  :ensure
  :bind (("C-c t" . vterm)))

(use-package vundo
  :ensure)

(use-package whole-line-or-region
  :ensure
  :config
  (whole-line-or-region-global-mode nil))


;;------------------------------------------------------------------------------
;; Other file types

(use-package pdf-tools
  :ensure
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))



;;------------------------------------------------------------------------------
;; More aesthetics, mostly borrowing from Doom

(use-package doom-modeline
  :ensure
  :hook (after-init . doom-modeline-mode)
  :config
  (setq nerd-icons-font-family "PragmataPro"))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :ensure
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;------------------------------------------------------------------------------
;; General programming

(use-package magit
  :ensure
  :bind (
	 ("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c M-g" . magit-file-dispatch)))

(use-package projectile
  :ensure
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; (use-package realgud
;;   :ensure
;;   :defer)

;; (use-package realgud-lldb
;;   :ensure
;;   :defer
;;   :after realgud)


(use-package paredit
  :ensure
  :config
  ;; this interferes with my preferred consult keys
  (eval-after-load "paredit"
    '(progn
       (define-key paredit-mode-map (kbd "M-s") nil)))
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (minibuffer-setup . disable-paredit-mode))

(use-package paredit-everywhere
  :ensure
  :after paredit
  :config
  (eval-after-load "paredit-everywhere"
    '(progn
       (define-key paredit-everywhere-mode-map (kbd "M-s") nil)))
  :hook
  (prog-mode . paredit-everywhere-mode))


;;------------------------------------------------------------------------------
;; Language-specific

(use-package haskell-mode
  :ensure
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package rust-mode
  :after eglot
  :preface
  :ensure
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
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

;; (use-package geiser
;;   :ensure)

;; (use-package geiser-racket
;;   :ensure
;;   :after geiser
;;   :config
;;   (setq geiser-racket-binary "/usr/local/bin/racket"))

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t
        org-latex-compiler 'lualatex)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; (use-package slime
;;   :ensure
;;   :config
;;   (setq inferior-lisp-program "sbcl"))

(use-package yaml-mode
  :ensure
  :mode "\\.yml\\'")

(use-package python-mode
  :ensure)

(use-package cc-mode
  :preface
  (defun kjs/insert-ifdef-guards ()
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

(use-package poke-mode
  :ensure)

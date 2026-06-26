;;; init.el --- Karl's Emacs Configuration -*- lexical-binding: t; -*-

;; _____       __________      ______
;; ___(_)_________(_)_  /_________  /
;; __  /__  __ \_  /_  __/_  _ \_  /
;; _  / _  / / /  / / /___/  __/  /
;; /_/  /_/ /_//_/  \__/(_)___//_/

;; Use `consult-imenu' (M-g i) to jump to any section.

;;; Package Bootstrap

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; Core Settings

;; Keep .emacs.d clean - do this first
(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
	`(("." . ,(no-littering-expand-var-file-name "backups/")))))


(use-package exec-path-from-shell
  :ensure
  :demand t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (dolist (var '("JIRA_API_KEY"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))


(use-package emacs
  :ensure nil
  :demand t
  :init
  ;; Performance (GC settings are in early-init.el)
  (setq load-prefer-newer t)
  (setq read-process-output-max (* 4 1024 1024))
  (setq process-adaptive-read-buffering nil)
  (setq max-lisp-eval-depth 10000)
  (setq auto-mode-case-fold nil)

  ;; Fonts
  (let ((font-size (if (eq system-type 'darwin) 170 150)))
    (set-face-attribute 'default nil
		        :font "PragmataPro"
                        :height font-size)
    (set-face-attribute 'variable-pitch nil
                        :font "Equity OT"
                        :height font-size)
    (set-face-attribute 'fixed-pitch nil
		        :font "PragmataPro"
                        :height font-size))

  ;; UI (tool-bar, scroll-bar, menu-bar disabled in early-init.el)
  (setq ring-bell-function nil)
  (setq visible-bell t)
  (display-battery-mode)

  ;; macOS keys
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'none
          dired-use-ls-dired nil))

  ;; Basic behavior
  (recentf-mode)
  (setq save-interprogram-paste-before-kill t)
  (setq yank-pop-change-selection t)
  (setq help-window-select t)
  (setq enable-recursive-minibuffers t)
  (setq compilation-scroll-output t)
  (minibuffer-depth-indicate-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq browse-url-browser-function 'browse-url-firefox)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; Enable some disabled functions
  (mapc (lambda (fn) (put fn 'disabled nil))
        '(narrow-to-region narrow-to-page upcase-region downcase-region))

  :bind (([remap list-buffers] . ibuffer)
         ("C-M-y" . duplicate-dwim)
         ([remap zap-to-char] . zap-up-to-char)))


(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)


(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer t))


(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("TAB" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB" . dired-subtree-remove)))


(use-package wdired
  :ensure t
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))


(use-package eshell
  :ensure nil)


(use-package isearch
  :ensure nil
  :config
  (setq search-whitespace-regexp ".*")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil))


;; Custom mark functions
(defun kjs--push-mark-no-activate ()
  (interactive)
  (push-mark (point) t nil))

(defun kjs--jump-to-mark ()
  (interactive)
  (set-mark-command 1))

(defun kjs--exchange-point-and-mark-no-activate ()
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "C-M-`") 'jump-to-mark)
(global-set-key [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;;; Completion Framework

(use-package vertico
  :ensure t
  :init
  (vertico-mode))


(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))


(use-package orderless
  :ensure t
  :config
  (setq orderless-matching-styles '(orderless-literal orderless-initialism orderless-flex)
	completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))


(use-package consult
  :ensure t
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"))


(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C->" . embark-export)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
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

;;; Appearance

(use-package doom-modeline
  :ensure t
  :init
  (setq nerd-icons-font-family "PragmataPro")
  :hook (after-init . doom-modeline-mode))


(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t))


(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-cyprus t))


(use-package standard-themes
  :ensure t)


(use-package monokai-pro-theme
  :ensure t)


(use-package lin
  :ensure t
  :config
  (setq lin-mode-hooks
        '(dired-mode-hook
          git-rebase-mode-hook
          grep-mode-hook
          ibuffer-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          package-menu-mode-hook
          pdf-outline-buffer-mode-hook
          proced-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1))


(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-subtle-frame-lines t)
  (spacious-padding-mode 1))


(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package ansi-color
  :ensure t
  :config
  (defun kjs-filter-osc-sequences ()
    "Remove OSC and terminal query sequences from compilation output."
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char compilation-filter-start)
        ;; Remove OSC sequences: ESC ] ... (BEL | ESC \)
        (while (re-search-forward "\e\\][^\a\e]*\\(\a\\|\e\\\\\\)" nil t)
          (replace-match ""))
        (goto-char compilation-filter-start)
        ;; Remove CSI ? sequences (cursor show/hide, etc)
        (while (re-search-forward "\e\\[\\?[0-9;]*[a-zA-Z]" nil t)
          (replace-match "")))))
  :hook ((compilation-filter . ansi-color-compilation-filter)
         (compilation-filter . kjs-filter-osc-sequences)))

;;; Navigation & Projects

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


(use-package projectile-ripgrep
  :ensure t)


(use-package ace-window
  :ensure
  :bind ("M-o". ace-window))


(use-package avy
  :ensure
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char))

;;; Editing Helpers

(use-package diminish
  :ensure t)


(use-package substitute
  :ensure t
  :demand t
  :bind (("C-c s" . substitute-prefix-map)))


(use-package helpful
  :ensure
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))


(use-package change-inner
  :ensure
  :bind (("M-i" . change-inner)))


(use-package jinx
  :ensure
  :diminish jinx-mode
  :preface
  (setenv "DICPATH" (expand-file-name "~/.local/share/hunspell"))
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-\"" . jinx-correct)))


(use-package vundo
  :ensure)


(use-package whole-line-or-region
  :ensure t
  :diminish (whole-line-or-region-mode whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode nil))


(use-package pdf-tools
  :ensure
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

;;; LSP & Tree-sitter

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
    (concat "https://github.com/tree-sitter/tree-sitter-" proj))

  (defun kjs-setup-install-grammars ()
    "Install tree-sitter grammars if missing."
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
             (c++-mode . c++-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (kjs-setup-install-grammars))

;;; Programming Languages

(use-package magit
  :ensure
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c M-g" . magit-file-dispatch)))


(use-package compiler-explorer
  :ensure
  :custom
  (compiler-explorer-output-filters
   '(:binary nil :binaryObject nil :commentOnly t :demangle t :directives
             t :intel nil :labels t :libraryCode t :trim nil :debugCalls nil)))


(use-package paredit
  :ensure
  :diminish paredit-mode
  :config
  (eval-after-load "paredit"
    '(progn (define-key paredit-mode-map (kbd "M-s") nil)))
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
    '(progn (define-key paredit-everywhere-mode-map (kbd "M-s") nil)))
  :hook (prog-mode . paredit-everywhere-mode))


(use-package dpkg-dev-el
  :ensure t)


(use-package haskell-mode
  :ensure t)


(use-package rust-mode
  :after eglot
  :ensure
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save nil)
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer"
                  :initializationOptions
                  (:check (:command "clippy")
                   :workspace (:symbol (:search (:kind "all_symbols"))))))))


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
  :hook ((c-ts-mode . kjs-c-ts-common-setup)
         (c++-ts-mode . kjs-c-ts-common-setup))
  :init
  (defun kjs-c-ts-common-setup ()
    "Shared setup for C and C++.
Uses tree-sitter indentation with format-on-save via eglot/clangd
as the source of truth."
    (eglot-ensure)
    (add-hook 'before-save-hook #'kjs-eglot-format-on-save nil t))

  (defun kjs-eglot-format-on-save ()
    (when (bound-and-true-p eglot--managed-mode)
      (eglot-format-buffer)))

  (defun kjs-c-ts-bind-keys (map)
    (define-key map (kbd "C-c f") #'kjs-eglot-format-dwim)
    (define-key map (kbd "C-c F") #'eglot-format-buffer))

  ;; --- Old manual indentation functions (kept for reference) ---
  ;; To restore: uncomment these and the old kjs-c-ts-common-setup body above.
  ;;
  ;; (defun kjs-indent-to-next-multiple ()
  ;;   (interactive)
  ;;   (let ((col (save-excursion (back-to-indentation) (current-column))))
  ;;     (if (<= (current-column) col)
  ;;         (let ((cur (current-indentation)))
  ;;           (indent-line-to (+ cur (- 4 (mod cur 4)))))
  ;;       nil)))
  ;;
  ;; (defun kjs-dedent-to-prev-multiple ()
  ;;   (interactive)
  ;;   (let ((cur (current-indentation)))
  ;;     (indent-line-to (max 0 (- cur (if (zerop (mod cur 4)) 4 (mod cur 4)))))))
  ;;
  ;; Old kjs-c-ts-common-setup body:
  ;;   (electric-indent-local-mode -1)
  ;;   (setq-local indent-line-function #'kjs-indent-to-next-multiple)
  ;;   (local-set-key (kbd "<backtab>") #'kjs-dedent-to-prev-multiple)
  ;;   (add-hook 'eglot-managed-mode-hook
  ;;             (lambda () (setq-local post-self-insert-hook (list t)))
  ;;             nil t)
  ;; ---

  :config
  (setq c-ts-mode-indent-offset 4)
  (setq c-ts-mode-indent-style 'linux)
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

;;; Org & Notes

(use-package org
  :ensure
  :config
  (setq org-pretty-entities t)
  (setq org-directory (expand-file-name "~/Documents/notes"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (setf (alist-get "\\.x?html?\\'" org-file-apps nil nil #'string=)
        'browse-url-browser-function)
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))


(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:map global-map
        ("C-c n n" . denote)
        ("C-c n r" . denote-rename-file)
        ("C-c n l" . denote-link)
        ("C-c n b" . denote-backlinks)
        ("C-c n d" . denote-dired)
        :map dired-mode-map
        ("C-c C-d C-i" . denote-dired-link-marked-notes)
        ("C-c C-d C-r" . denote-dired-rename-files)
        ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
        ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("emacs" "ubuntu" "personal"))
  (setq denote-sort-keywords t)
  (denote-rename-buffer-mode 1))


(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1)
  (setq consult-denote-grep-command 'consult-ripgrep))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do))
  :hook (markdown-mode . visual-line-mode))

;;; AI Tools

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'claude-4.6-sonnet
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  :hook (gptel-post-stream-hook . gptel-auto-scroll))


(use-package gptel-agent
  :ensure t
  :after gptel
  :config
  (gptel-agent-update))

(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest))

;;; External Tools

(use-package transient
  :ensure t)


;;; Terminals

(use-package eat
  :ensure t)


(use-package ghostel
  :ensure t
  :bind (("C-c t" . ghostel)))

;;; Debian Packaging

;; New deb-packaging package (separate repo)
(use-package deb-packaging
  :load-path "~/deb-packaging-el"
  :bind ("C-c d" . deb-packaging-status))

;; Legacy kjs-deb (available via C-c ` for advanced flag control)
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(use-package kjs-transient
  :ensure nil)

(use-package kjs-deb
  :ensure nil)

;;; init.el ends here

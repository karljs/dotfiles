;;  -*- lexical-binding: t; -*-

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


(use-package substitute
  :ensure t
  :demand t
  :bind (("C-c s" . substitute-prefix-map)))


(use-package diminish
  :ensure t)


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
  :diminish jinx-mode
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-\"" . jinx-correct)))


(use-package vundo
  :ensure)


(use-package whole-line-or-region
  :ensure t
  :diminish
  (whole-line-or-region-mode whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode nil))


(use-package pdf-tools
  :ensure
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))


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


(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))


(use-package ef-themes
  :ensure t)


(use-package standard-themes
  :ensure t)


(use-package rainbow-delimiters
  :ensure
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package ansi-color
  :ensure t
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))


(use-package transient
  :ensure t)


(provide 'kjs-misc)

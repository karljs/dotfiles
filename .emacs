;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/


;; Load libraries
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; GUI Cleanup
;(menu-bar-mode nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (global-linum-mode)
(set-fringe-mode 0)

;; Font & Colors
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-initialize)
(color-theme-solarized-light)
(set-default-font "-*-inconsolata-*-*-*-*-16-*-*-*-*-*-*-*")

;; Haskell
(load "/Users/karl/.emacs.d/site-lisp/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Lisp, Slime, Paredit
(setq inferior-lisp-program "/opt/local/bin/sbcl")
(require 'slime-autoloads)
(slime-setup '(slime-repl))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;; Good behavior
(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


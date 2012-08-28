;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/


;;------------------------------------------------------------------------------
;; Load things
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;------------------------------------------------------------------------------
;; GUI Cleanup
;; (menu-bar-mode nil)  ; not necessary on osx
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

;;------------------------------------------------------------------------------
;; Font & Colors
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
(load-theme 'solarized-light t)
;(require 'color-theme)
;(require 'color-theme-solarized)
;(color-theme-initialize)
;(color-theme-solarized-light)
(set-default-font "-*-ubuntu mono-*-*-*-*-16-*-*-*-*-*-*-*")

;;------------------------------------------------------------------------------
;; Good behavior
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
;(setq osx-key-mode 0)
;(setq mac-option-key-is-meta t)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ns-pop-up-frames nil)
(global-auto-revert-mode 1)
(setq-default fill-column 80)


;;------------------------------------------------------------------------------
;; General & Keybindings
(server-start)
(global-set-key (kbd "C-x a r") 'align-regexp)


;;------------------------------------------------------------------------------
;; Spelling
(setq-default ispell-program-name "aspell")


;;------------------------------------------------------------------------------
;; LaTeX
;(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)


;;------------------------------------------------------------------------------
;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;;--------------------------------------------------------------------------------
;; Multi Term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(defun add-mode-line-dirtrack ()
  (add-to-list 'mode-line-buffer-identification
               '(:propertize (" " default-directory " ") face dired-directory)))
(add-hook 'term-mode-hook 'add-mode-line-dirtrack)



;;------------------------------------------------------------------------------
;; Comments
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region-or-line)


;;------------------------------------------------------------------------------
;; Magit
(require 'magit)


;;------------------------------------------------------------------------------
;; Haskell
(load "/Users/karl/.emacs.d/site-lisp/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


;;------------------------------------------------------------------------------
;; Lisp, Slime, Paredit
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime-autoloads)
(slime-setup '(slime-repl))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))


;;------------------------------------------------------------------------------
;; ML
; (load "/Users/karl/.emacs.d/site-lisp/ocaml-mode/ocaml.emacs")

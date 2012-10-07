;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/


;;------------------------------------------------------------------------------
;; Load things

(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;------------------------------------------------------------------------------
;; Install packages as necessary. Thanks to the Prelude project for inspiration
(require 'cl)

(defvar karl-packages
  '(auctex clojure-mode color-theme-solarized haskell-mode magit multi-term nrepl paredit)
  "Packages to install at launch, when necessary.")

(defun karl-packages-installed-p ()
  (loop for p in karl-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun karl-install-packages ()
  (unless (karl-packages-installed-p)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (dolist (p karl-packages)
      (unless (package-installed-p p)
	(package-install p)))))

(karl-install-packages)


;;------------------------------------------------------------------------------
;; Path stuff
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/opt/local/bin")))

;;------------------------------------------------------------------------------
;; GUI Cleanup
;; (menu-bar-mode nil)  ; not necessary on osx
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;;------------------------------------------------------------------------------
;; Font & Colors
(load-theme 'solarized-light t)
;(load-theme 'tomorrow-night t)
(set-default-font "-*-inconsolata-*-*-*-*-16-*-*-*-*-*-*-*")

;;------------------------------------------------------------------------------
;; Good behavior
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ns-pop-up-frames nil)
(global-auto-revert-mode 1)
(setq-default fill-column 80)
(setq vc-follow-symlinks t)

;;------------------------------------------------------------------------------
;; General & Keybindings
(global-set-key (kbd "C-x a r") 'align-regexp)

;;______________________________________________________________________________
;; LaTeX
(setq latex-run-command "/usr/texbin/pdflatex")
;(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

;;------------------------------------------------------------------------------
;; Spelling
(setq-default ispell-program-name "aspell")

;;------------------------------------------------------------------------------
;; Org
(define-key global-map "\C-ca" 'org-agenda)
;; (setq org-agenda-files (list "~/notes/dsvl/dsvl.org"
                             ;;"~/notes/awareness/awareness.org"))

;;------------------------------------------------------------------------------
;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)


;;--------------------------------------------------------------------------------
;; Multi Term
(setq multi-term-program "/bin/bash")
(global-set-key (kbd "C-x t") 'multi-term)

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
;; C/C++
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;------------------------------------------------------------------------------
;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;------------------------------------------------------------------------------
;; Lisp, Slime, Paredit
;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;(require 'slime-autoloads)
;(slime-setup '(slime-repl))
;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;(autoload 'paredit-mode "paredit"
  ;"Minor mode for pseudo-structurally editing Lisp code." t)
;(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
;(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
;(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;;------------------------------------------------------------------------------
;; AceJump
;(autoload
  ;'ace-jump-mode
  ;"ace-jump-mode"
  ;"Emacs quick move minor mode"
  ;t)

;;------------------------------------------------------------------------------
;; Clojure
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(setq nrepl-popup-stacktraces nil)

;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/

;;------------------------------------------------------------------------------
;; Load and setup
(package-initialize)

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;------------------------------------------------------------------------------
;; Install packages as necessary. Credit to the Prelude project
(require 'cl)

(defvar karl-packages '(ace-jump-mode auctex birds-of-paradise-plus-theme
                                      clojure-mode clojurescript-mode
                                      color-theme-sanityinc-solarized
                                      color-theme-sanityinc-tomorrow
                                      cyberpunk-theme evil google-c-style
                                      goto-chg haskell-mode inkpot-theme
                                      ir-black-theme magit markdown-mode
                                      multi-term nrepl paredit twilight-theme
                                      ucs-utils unicode-fonts zenburn-theme)
  "Packages to install at launch, when necessary.")

;; removed: paredit, slime

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
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/opt/local/bin")))
(setq exec-path (append exec-path '("/Users/karl/Library/Haskell/bin/")))
(setenv "PATH" (concat (getenv "PATH") ":/Users/karl/Library/Haskell/bin"))

;;------------------------------------------------------------------------------
;; GUI Cleanup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "karl")
(transient-mark-mode 1)

;;------------------------------------------------------------------------------
;; Font & Colors
(setq solarized-broken-srgb t)
(setq solarized-italic nil)
(setq solarized-bold nil)
(load-theme 'solarized-light t)
(set-face-attribute 'default nil :font "Consolas-14")

;; Thanks to the Emacs Starter Kit for the following bit
;; Make the font larger on my external monitor
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
            (set-frame-parameter frame 'font "Consolas-17")
          (set-frame-parameter frame 'font "Consolas-14")))))
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)


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
; (setq visible-bell t)
(setq ring-bell-function (lambda () (message "*beep*")))
(setq-default truncate-lines t)
(setq dired-use-ls-dired nil)

;;------------------------------------------------------------------------------
;; General & Keybindings
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-h C-f") 'find-tag)

;;------------------------------------------------------------------------------
;; Ace jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;------------------------------------------------------------------------------
;; LaTeX
(setq TeX-PDF-mode t)
;(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

;;------------------------------------------------------------------------------
;; Spelling
(setq-default ispell-program-name "aspell")

;;------------------------------------------------------------------------------
;; Org
(define-key global-map "\C-ca" 'org-agenda)

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
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-l") 'compile)))

;;------------------------------------------------------------------------------
;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;------------------------------------------------------------------------------
;; Agda
; (require 'ucs-utils)
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(add-hook 'agda2-mode-hook
          '(lambda ()
             (require 'unicode-fonts)
             (unicode-fonts-setup)))

;;------------------------------------------------------------------------------
;; Lisp, Slime, Paredit
;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;(require 'slime-autoloads)
;(slime-setup '(slime-repl))
;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;(autoload 'paredit-mode "paredit"
;  "Minor mode for pseudo-structurally editing Lisp code." t)
; (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
; (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;;------------------------------------------------------------------------------
;; Clojure
; (add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(setq nrepl-popup-stacktraces nil)

;;------------------------------------------------------------------------------
;; Evil
; (require 'evil)
; (evil-mode 1)
; (define-key evil-ex-map "e " 'ido-find-file)
;; (define-key evil-ex-map "w " 'ido-write-file)
; (define-key evil-ex-map "b " 'ido-switch-buffer)

;;------------------------------------------------------------------------------
;; Speedbar
(require 'sr-speedbar)
(global-set-key (kbd "M-s M-s") 'sr-speedbar-toggle)

;;------------------------------------------------------------------------------
(message "%s" "You shouldn't have come back, Karl")

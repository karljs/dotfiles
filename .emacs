;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/


;;------------------------------------------------------------------------------
;; Package manager load and setup
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;------------------------------------------------------------------------------
;; Install packages as necessary on startup. Credit to the Prelude project.
(defvar my-packages '(ace-jump-mode
                      auctex
                      buffer-move
                      clojure-mode
                      clojurescript-mode
                      color-theme-solarized
                      evil
                      fastnav
                      google-c-style
                      goto-chg
                      haskell-mode
                      inkpot-theme
                      jabber
                      magit
                      markdown-mode
                      multi-term
                      nrepl
                      paredit
                      rainbow-delimiters
                      sr-speedbar
                      ucs-utils
                      unicode-fonts
                      zenburn-theme)
  "Packages to install at launch, when necessary.")

(require 'cl)

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun my-install-packages ()
  (unless (my-packages-installed-p)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (dolist (p my-packages)
      (unless (package-installed-p p)
	(package-install p)))))

(my-install-packages)

;;------------------------------------------------------------------------------
;; Path stuff.  Machine specific.
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/Users/karl/.cabal/bin:"
                         "/Users/karl/bin:"
                         (getenv "PATH")))
  (push "/Users/karl/.cabal/bin" exec-path)
  (push "/Users/karl/bin" exec-path))

;;------------------------------------------------------------------------------
;; GUI Settings
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "karl")
(transient-mark-mode 1)

;;------------------------------------------------------------------------------
;; Font & Colors
(setq solarized-italic nil)
(setq solarized-broken-srgb t)
(load-theme 'solarized-light t)

;; Set the font depending on OS and pixel density
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
            (set-frame-parameter frame 'font "Source Code Pro-15")
          (set-frame-parameter frame 'font "Source Code Pro-14")))))
(if (eq system-type 'darwin)
    (fontify-frame nil)
  (set-face-attribute 'default nil :font "Inconsolata-13"))
(push 'fontify-frame after-make-frame-functions)


;;------------------------------------------------------------------------------
;; Good behavior
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq-default indent-tabs-mode nil)  ; never use tabs anywhere
(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ns-pop-up-frames nil)
(global-auto-revert-mode 1)
(setq-default fill-column 80)
(setq vc-follow-symlinks t)
(setq ring-bell-function (lambda () (message "*beep*")))  ; stop beeping
;(setq-default truncate-lines t)
(setq dired-use-ls-dired nil)

;;------------------------------------------------------------------------------
;; General keybindings
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;;------------------------------------------------------------------------------
;; buffer-move
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

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

;;------------------------------------------------------------------------------
;; Commenting
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-end-position)))
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
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq haskell-program-name "ghci")

;; The following needs to be done without the hardcoded path.

;; (defun civil-tags ()
;;   "Generate TAGS file specifically for CIViL."
;;   (interactive)
;;   (shell-command "cd ~/workspace/civil/src && echo \":etags\" | ghci -v0 Graphics/Civil.hs"))

;; (add-hook 'haskell-mode-hook
;;           '(lambda ()
;;              (add-hook 'before-save-hook
;;                        (lambda ()
;;                          (civil-tags)))))

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
;; Clojure/nREPL
(setq nrepl-popup-stacktraces nil)

;;------------------------------------------------------------------------------
;; Paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)
(add-hook 'nrepl-interaction-mode 'paredit-mode)
(add-hook 'nrepl-connected-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)

;;------------------------------------------------------------------------------
;; Rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;------------------------------------------------------------------------------
;; Evil
;; (evil-mode 1)
;; (define-key evil-ex-map "e " 'ido-find-file)
;; (define-key evil-ex-map "w " 'ido-write-file)
;; (define-key evil-ex-map "b " 'ido-switch-buffer)

;;------------------------------------------------------------------------------
;; Speedbar
(require 'sr-speedbar)
(global-set-key (kbd "M-s M-s") 'sr-speedbar-toggle)
(speedbar-add-supported-extension ".hs")

;;------------------------------------------------------------------------------
;; GDB
(setq gdb-many-windows t)

;;------------------------------------------------------------------------------
;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;------------------------------------------------------------------------------
;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;------------------------------------------------------------------------------
;; Jabber
(setq jabber-alert-presence-hooks nil)
(setq jabber-account-list
      '(("karl.smeltzer@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))

;;------------------------------------------------------------------------------
;; FastNav
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)


;;------------------------------------------------------------------------------
(message "%s" "You shouldn't have come back, Karl")

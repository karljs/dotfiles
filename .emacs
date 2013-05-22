;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/


;;------------------------------------------------------------------------------
;; Import things
(require 'cl)

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
                      evil
                      exec-path-from-shell
                      fastnav
                      geiser
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
                      solarized-theme
                      sr-speedbar
                      ucs-utils
                      unicode-fonts
                      yasnippet
                      zenburn-theme)
  "Packages to install at launch, when necessary.")

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
  (exec-path-from-shell-initialize))

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
;(global-linum-mode t)
(column-marker-1 80)

;;------------------------------------------------------------------------------
;; Font & Colors
; (setq color-theme-sanityinc-solarized-rgb-is-srgb t)
(load-theme 'sanityinc-solarized-light t)

;; Set the font depending on OS and pixel density
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
            (set-frame-parameter frame 'font "Source Code Pro-16")
          (set-frame-parameter frame 'font "Source Code Pro-14")))))
(if (eq system-type 'darwin)
    (fontify-frame nil)
  (set-face-attribute 'default nil :font "Anonymous Pro-13"))
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
(setq compilation-scroll-output 1)
(setq compilation-window-height 10)

;;------------------------------------------------------------------------------
;; Global keybindings
;;(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; <3 Unix
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Programming specific
(global-set-key (kbd "M-g n") 'next-error)
(global-set-key (kbd "M-g p") 'previous-error)



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
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)

(setq TeX-view-program-selection
      '((output-pdf "PDF Viewer")
        (output-dvi "DVI Viewer")
        (output-html "HTML Viewer")))
(when (eq system-type 'darwin)
  (setq TeX-view-program-list
        '(("PDF Viewer" "open %o")
          ("DVI Viewer" "open %o")
          ("HTML Viewer" "open %o"))))

;;------------------------------------------------------------------------------
;; Spelling
(setq-default ispell-program-name "aspell")

;;------------------------------------------------------------------------------
;; Org
(define-key global-map "\C-ca" 'org-agenda)

;;------------------------------------------------------------------------------
;; Ido
(setq ido-auto-merge-work-directories-length -1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)
(setq ido-default-buffer-method 'selected-window)

;;------------------------------------------------------------------------------
;; Commenting
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
   there's no active region."
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
(defun my-c-mode-hook ()
  (setq c-basic-offset 4
        c-indent-level 4
        c-default-style "BSD")
  (local-set-key (kbd "C-c C-c") 'my-compile-func)
  (local-set-key (kbd "C-c C-k") 'my-compile-clean-func))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Walks up from current directory until it finds a makefile."
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

(defun my-compile-func ()
  (interactive)
  (compile (format "make -C %s" (file-name-directory (get-closest-pathname)))))

(defun my-compile-clean-func ()
  (interactive)
  (compile (format "make -C %s clean"
                   (file-name-directory (get-closest-pathname)))))


;;------------------------------------------------------------------------------
;; Haskell
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))
(setq haskell-program-name "ghci -fno-ghci-sandbox")

;;------------------------------------------------------------------------------
;; Agda
;; (require 'ucs-utils)
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))
(add-hook 'agda2-mode-hook
          '(lambda ()
             (require 'unicode-fonts)
             (unicode-fonts-setup)))

;;------------------------------------------------------------------------------
;; Clojure/nREPL
(setq nrepl-popup-stacktraces nil)

(defun nrepl-refresh ()
  (interactive)
  (set-buffer "*nrepl*")
  (goto-char (point-max))
  (insert "(clojure.tools.namespace.repl/refresh)")
  (nrepl-return))

(add-hook 'nrepl-mode-hook
          (lambda () (local-set-key (kbd "C-c r") 'nrepl-refresh)))

;;------------------------------------------------------------------------------
;; Paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)

;;------------------------------------------------------------------------------
;; Rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;------------------------------------------------------------------------------
;; Evil
;; (setq evil-want-C-u-scroll t)
;; (setq evil-shift-width 4)
;; (setq-default evil-auto-indent nil)

;; (evil-mode 1)
;; (define-key evil-ex-map "e " 'ido-find-file)
;; (define-key evil-ex-map "w " 'ido-write-file)
;; (define-key evil-ex-map "b " 'ido-switch-buffer)

;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
;; (define-key evil-visual-state-map [escape] 'keyboard-quit)
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (require 'evil-paredit)
;; (add-hook 'clojure-mode-hook 'evil-paredit-mode)
;; (add-hook 'clojurescript-mode-hook 'evil-paredit-mode)
;; (add-hook 'nrepl-interaction-mode 'evil-paredit-mode)
;; (add-hook 'nrepl-connected-hook 'evil-paredit-mode)
;; (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
;; (add-hook 'lisp-mode-hook 'evil-paredit-mode)


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
;; Change Inner
(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;;------------------------------------------------------------------------------
;; Yasnippet
(setq yas-verbosity 0)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

;;------------------------------------------------------------------------------
;; Misc things that should probably be in a different file
(defun smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)

(defun reload-dot-emacs ()
  (interactive)
  (load-file "~/.emacs")
  (message "Reloaded .emacs file..."))

(global-set-key (kbd "C-x C-r") 'reload-dot-emacs)

(defun open-with ()
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(global-set-key (kbd "C-c o") 'open-with)


;;------------------------------------------------------------------------------
;; Creating various tags
(defun htags (dir)
  "Create Haskell tags"
  (interactive)
  (shell-command (concat "cd " dir "&& hasktags -e -o ./TAGS ."))
  (message "Created Haskell tags."))

(defun ctags (dir)
  "Create ctags"
  (interactive)
  (shell-command (concat "cd " dir "&& ctags -e -R ."))
  (message "Created ctags."))

(defun tags (dir)
  "Create tags"
  (interactive (list (read-directory-name "Directory: ")))
  (if (equal major-mode 'haskell-mode)
      (htags dir)
    (ctags dir)))

(global-set-key (kbd "C-x t") 'tags)


;;------------------------------------------------------------------------------
(message "%s" "You shouldn't have come back, Karl")

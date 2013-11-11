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
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;------------------------------------------------------------------------------
;; Install packages as necessary on startup. Credit to the Prelude project.
(defvar my-packages '(ace-jump-mode
                      ag
                      auctex
                      buffer-move
                      change-inner
                      clojure-mode
                      clojurescript-mode
                      exec-path-from-shell
                      fastnav
                      glsl-mode
                      haskell-mode
                      ido-ubiquitous
                      inkpot-theme
                      lua-mode
                      magit
                      markdown-mode
                      monokai-theme
                      ;; multi-web-mode
                      nrepl
                      paredit
                      projectile
                      rainbow-delimiters
                      scala-mode2
                      smartparens
                      smex
                      solarized-theme
                      ucs-utils
                      unicode-fonts
                      web-mode
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
(if (or (not (display-graphic-p)) (not (eq system-type 'darwin)))
    (menu-bar-mode -1))
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "karl"
      suggest-key-bindings nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(transient-mark-mode -1)


;;------------------------------------------------------------------------------
;; Font & Colors
;; (setq font-lock-maximum-decoration nil)
;; (setq solarized-use-variable-pitch nil
;;       solarized-height-minus-1 1
;;       solarized-height-plus-1 1
;;       solarized-height-plus-2 1
;;       solarized-height-plus-3 1
;;       solarized-height-plus-4 1)
(load-theme 'solarized-light t)

;; Set the font depending on OS and pixel density
(defun fontify-frame (frame)
  (interactive)
  (when window-system
    (if (> (x-display-pixel-width) 2000)
        (set-frame-parameter frame 'font "Source Code Pro-15")
      (set-frame-parameter frame 'font "Source Code Pro-14"))))
(if (eq system-type 'darwin)
    (fontify-frame nil)
  (set-face-attribute 'default nil :font "Inconsolata-13"))
;; (push 'fontify-frame after-make-frame-functions)

;;------------------------------------------------------------------------------
;; Good behavior
(setq confirm-nonexistent-file-or-buffer nil
      dired-use-ls-dired nil
      mac-command-modifier 'meta
      mac-option-modifier 'none
      make-backup-files nil
      ns-pop-up-frames nil
      ring-bell-function (lambda () (message "*beep*"))
      vc-follow-symlinks t)

(when (eq system-type 'darwin)
  (setq default-directory "/Users/karl"))

(setq-default fill-column 80
              indent-tabs-mode nil)

(global-auto-revert-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;------------------------------------------------------------------------------
;; Global keybindings
;; (evil-mode 1)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-c %") 'replace-regexp)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; <3 Unix
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-h") 'backward-kill-word)
;; (global-set-key (kbd "C-?") 'help-command)
;; (global-set-key (kbd "M-?") 'mark-paragraph)

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
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)

;;------------------------------------------------------------------------------
;; LaTeX
(require 'tex-site)

(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; (setq font-latex-fontify-script nil)
;; (setq font-latex-fontify-sectioning 'color)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; (add-hook 'LaTeX-mode-hook (lambda ()
;;   (push
;;    '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;;      :help "Run Latexmk on file")
;;    TeX-command-list)))

(when (eq system-type 'darwin)
  (setq TeX-view-program-list
        '(("PDF Viewer" "open %o")
          ("DVI Viewer" "open %o")
          ("HTML Viewer" "open %o"))))

(setq TeX-view-program-selection
      '((output-pdf "PDF Viewer")
        (output-dvi "DVI Viewer")
        (output-html "HTML Viewer")))

;;------------------------------------------------------------------------------
;; Spelling
;; (setq speck-engine (quote Hunspell))
;; (setq speck-hunspell-default-dictionary-name "en_US")
(setq-default ispell-program-name "aspell")


;;------------------------------------------------------------------------------
;; Org
;; (setq org-fontify-emphasized-text nil)
;; (define-key global-map "\C-ca" 'org-agenda)

;;------------------------------------------------------------------------------
;; Ido
;; (setq ido-auto-merge-work-directories-length -1)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
;; (setq ido-default-buffer-method 'selected-window)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(setq smex-key-advice-ignore-menu-bar t)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(ido-vertical-mode 1)

;;------------------------------------------------------------------------------
;; Commenting
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there
is no active region."
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
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (local-set-key (kbd "C-c C-c") 'my-compile-func)
  (local-set-key (kbd "C-c C-k") 'my-compile-clean-func)
  (local-set-key (kbd "C-c C-r") 'execute-premake-executable))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

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
 '(haskell-process-type 'ghci)
 '(haskell-process-args-ghci '("-fno-ghci-sandbox"))
 ;; '(haskell-tags-on-save t)
)

(setq haskell-program-name "ghci -fno-ghci-sandbox")
(add-hook 'haskell-mode-hook 'haskell-hook)

(defun haskell-hook ()
  (turn-on-haskell-indentation)
  (setq projectile-tags-command "hasktags -e")
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload-file)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  (define-key haskell-mode-map (kbd "C-c C-z")
    (lambda ()
      (interactive)
      (switch-to-buffer-other-window
       (haskell-session-interactive-buffer (haskell-session))))))


;;------------------------------------------------------------------------------
;; Agda
;; (require 'ucs-utils)
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))
;; (add-hook 'agda2-mode-hook
;;           '(lambda ()
;;              (require 'unicode-fonts)
;;              (unicode-fonts-setup)))

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
;; Paredit / Smartparens
(setq smartparens-strict-mode t)
(smartparens-global-mode)

;; (add-hook 'clojure-mode-hook 'paredit-mode)
;; (add-hook 'clojurescript-mode-hook 'paredit-mode)
;; (add-hook 'nrepl-mode-hook 'paredit-mode)
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;; (add-hook 'lisp-mode-hook 'paredit-mode)
;; (add-hook 'scheme-mode-hook 'paredit-mode)
;; (add-hook 'geiser-repl-mode-hook 'paredit-mode)

;;------------------------------------------------------------------------------
;; Rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;------------------------------------------------------------------------------
;; GDB
(setq gdb-many-windows t)

;;------------------------------------------------------------------------------
;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(when (eq system-type 'darwin)
  (setq magit-emacsclient-executable
        "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient"))

;;------------------------------------------------------------------------------
;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

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
;; (setq yas-verbosity 0)
;; (yas-global-mode 1)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))

;;------------------------------------------------------------------------------
;; Whitespace and long lines
(setq whitespace-style '(face lines))
(setq whitespace-line-column 80)
(add-hook 'prog-mode-hook 'whitespace-mode)

;;------------------------------------------------------------------------------
;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;------------------------------------------------------------------------------
;; Scala stuff
;; (add-to-list 'load-path "/Users/karl/.emacs.d/ensime_2.10.0-0.9.8.9/elisp/")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;------------------------------------------------------------------------------
;; MultiTerm + AnsiTerm
;; (setq term-default-fg-color (face-foreground 'default))
;; (setq term-default-bg-color (face-background 'default))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term "/opt/local/bin/bash"))
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-c t") 'visit-term-buffer)


;;------------------------------------------------------------------------------
;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glvs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glfs\\'" . glsl-mode))


;;------------------------------------------------------------------------------
;; Tramp
(setq tramp-shell-prompt-pattern
      "^[^]#$%>\n]*[]#$%>]$? *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
(setq ido-enable-tramp-completion t)
(setq tramp-default-method "ssh")


;;------------------------------------------------------------------------------
;; Rebox
(setq rebox-style-loop '(13))
(global-set-key (kbd "C-c b") 'rebox-dwim)


;;------------------------------------------------------------------------------
;; Projectile
(projectile-global-mode)
(setq projectile-indexing-method 'native)

;;------------------------------------------------------------------------------
;; Misc things that should probably be in a different file

(defun smart-open-line ()
  "Insert a line below regardless of point position.  Like Vim's 'o' command."
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


(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))
(global-set-key (kbd "C-O") 'open-line-above)


;;------------------------------------------------------------------------------
;; Creating various tags.
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
;; (global-set-key (kbd "C-c t") 'tags)


;;------------------------------------------------------------------------------
;; Premake specific stuff for finding a project's executable and running it
(defun find-premake-executable ()
  (let ((mf (get-closest-pathname "Makefile")))
    (with-temp-buffer
      (insert-file-contents mf)
      (split-string (buffer-string) "\n" t)
      (search-forward "PROJECTS := ")
      (buffer-substring (point) (line-end-position)))))

(defun execute-premake-executable ()
  (interactive)
  (let* ((exe-name (find-premake-executable))
         (exe (get-closest-pathname exe-name))
         (path (file-name-directory exe)))
    (cd path)
    (shell-command exe)))


;;------------------------------------------------------------------------------
;; General utilities
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Walks up from current directory until it finds a particular file."
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

;;------------------------------------------------------------------------------
(server-start)
(message "%s" "You shouldn't have come back, Karl")

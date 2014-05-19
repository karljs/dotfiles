;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/

;;------------------------------------------------------------------------------
;; Package manager load and setup
(require 'cl)
(package-initialize)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;------------------------------------------------------------------------------
;; Install packages as necessary on startup. Credit to the Prelude project.
(defvar my-packages '(ace-jump-mode
                      ag
                      auctex
                      auctex-latexmk
                      buffer-move
                      change-inner
                      elm-mode
                      exec-path-from-shell
                      evil
                      fastnav
                      flx
                      flx-ido
                      glsl-mode
                      haskell-mode
                      ido-ubiquitous
                      ido-vertical-mode
                      idris-mode
                      lua-mode
                      magit
                      markdown-mode
                      monokai-theme
                      paredit
                      projectile
                      rainbow-delimiters
                      s
                      scala-mode2
                      smex
                      solarized-theme
                      ucs-utils
                      unicode-fonts
                      web-mode
                      zenburn-theme)
  "Packages to install at launch, when necessary.")

(defun my-packages-installed-p ()
  "Loop through my preferred packages and determine which are installed."
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun my-install-packages ()
  "Install any missing packages."
  (unless (my-packages-installed-p)
    (package-refresh-contents)
    (dolist (p my-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(my-install-packages)

;;------------------------------------------------------------------------------
;; Fix broken GUI path on OSX
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (setq default-directory "/Users/karl"))

;;------------------------------------------------------------------------------
;; GUI Settings
(if (or (not (display-graphic-p)) (not (eq system-type 'darwin)))
    (menu-bar-mode -1))
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "")
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (transient-mark-mode -1)
(column-number-mode 1)


;;------------------------------------------------------------------------------
;; Font & Colors
;; (setq solarized-broken-srgb 'nil)
;; (setq ns-use-srgb-colorspace t)
(load-theme 'solarized-light t)

;; Set the font depending on OS and pixel density
(defun fontify-frame (frame)
  (interactive)
  (when window-system
    (if (> (x-display-pixel-width) 2000)
        (set-frame-parameter frame 'font "Source Code Pro-14")
      (set-frame-parameter frame 'font "Source Code Pro-14"))))
(if (eq system-type 'darwin)
    (fontify-frame nil)
  (set-face-attribute 'default nil :font "Inconsolata-13"))

;;------------------------------------------------------------------------------
;; Good behavior
(setq confirm-nonexistent-file-or-buffer nil
      mac-command-modifier 'meta
      mac-option-modifier 'none
      make-backup-files nil
      ns-pop-up-frames nil
      ring-bell-function (lambda () (message "*beep*"))
      vc-follow-symlinks t
      scroll-conservatively 1
      require-final-newline t)

(setq-default fill-column 78
              indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)


;;------------------------------------------------------------------------------
;; Default registers
(set-register ?e '(file . "~/.emacs"))


;;------------------------------------------------------------------------------
;; Global keybindings
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-c %") 'replace-regexp)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key (kbd "C-'") 'imenu)

(global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-?") 'help-command)
;; (global-set-key (kbd "M-?") 'mark-paragraph)

;;------------------------------------------------------------------------------
;; Evil
;; (evil-mode)
;; (define-key evil-ex-map "e " 'ido-find-file)
;; (define-key evil-ex-map "w " 'ido-write-file)
;; (define-key evil-ex-map "b " 'ido-switch-buffer)


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
;; Auctex / LaTeX
(require 'tex-site)
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
;; (auctex-latexmk-setup)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(when (eq system-type 'darwin)
  (setq TeX-view-program-list
        '(("PDF Viewer" "open %o")
          ("DVI Viewer" "open %o")
          ("HTML Viewer" "open %o"))
        TeX-view-program-selection
        '((output-pdf "PDF Viewer")
          (output-dvi "DVI Viewer")
          (output-html "HTML Viewer"))))

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
                             ;; (setq LaTeX-indent-level 0
                             ;;       LaTeX-item-indent 0)
                             (turn-on-auto-fill)
                             (LaTeX-math-mode)
                             (turn-on-reftex)
                             ;; (outline-minor-mode)
                             ))

(defun kjs-bibtex-next-entry ()
  (interactive)
  (bibtex-end-of-entry)
  (search-forward-regexp "^@.*")
  (beginning-of-line))

(defun kjs-bibtex-prev-entry ()
  (interactive)
  (bibtex-beginning-of-entry)
  (search-backward-regexp "^@.*")
  (beginning-of-line))

(defun kjs-bibtex-bind-forward-back-keys ()
  "Change the keys for moving by paragraph to do something
sensible in bibtex files."
  (define-key bibtex-mode-map (kbd "M-}") 'kjs-bibtex-next-entry)
  (define-key bibtex-mode-map (kbd "M-{") 'kjs-bibtex-prev-entry))

(add-hook 'bibtex-mode-hook 'kjs-bibtex-bind-forward-back-keys)


;;------------------------------------------------------------------------------
;; Spelling
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq-default ispell-program-name "aspell")
;; (setq ispell-extra-args '("--sug-mode=fast"))


;;------------------------------------------------------------------------------
;; Org
;; (setq org-fontify-emphasized-text nil)
(setq org-pretty-entities 1)


;;------------------------------------------------------------------------------
;; Ido / smex / vertical
(setq ido-create-new-buffer 'always)
(require 'flx-ido)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
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

(defun comment-header-line ()
  "Insert a long comment line with hyphens to denote sections in code."
  (interactive)
  (if (and comment-start comment-end)
      (progn (end-of-line)
             (unless (eq (current-column) 0)
               (open-line-above))
             (let* ((cs-trim (s-trim comment-start))
                    (ce-trim (s-trim comment-end))
                    (nbeg (string-width cs-trim))
                    (nend (string-width ce-trim))
                    (numchars (- fill-column (+ nbeg nend))))
               (insert cs-trim)
               (insert-char ?- numchars)
               (insert ce-trim)))
    (message "Comment characters are not set")))  ; looking at you, web-mode
(global-set-key (kbd "C-c h") 'comment-header-line)


;;------------------------------------------------------------------------------
;; C/C++

;; TODO -- write some kind of magic function to automatically determine which
;; build system to use.  In the mean time, default to scons.
(defun my-c-mode-hook ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  ;; (local-set-key (kbd "C-c C-c") 'my-compile-func)
  ;; (local-set-key (kbd "C-c C-k") 'my-compile-clean-func)
  (local-set-key (kbd "C-c C-l") 'scons-build)
  (local-set-key (kbd "C-c C-r") 'scons-run-exec))
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
(add-hook 'haskell-mode-hook 'haskell-hook)
(defun haskell-hook ()
  (setq haskell-interactive-mode-hide-multi-line-errors nil
        haskell-tags-on-save t)
  (turn-on-haskell-indentation)
  (turn-on-haskell-decl-scan)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload-file)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  (define-key haskell-mode-map (kbd "C-c C-h") 'haskell-check))


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
;; Idris
(add-hook 'idris-mode-hook
          '(lambda ()
             (set-face-attribute 'idris-semantic-data-face nil
                                 :foreground nil
                                 :inherit 'font-lock-string-face)
             (set-face-attribute 'idris-semantic-type-face nil
                                 :foreground nil
                                 :inherit 'font-lock-string-face)))


;;------------------------------------------------------------------------------
;; Clojure/nREPL

;; (setq nrepl-popup-stacktraces nil)
;; (defun nrepl-refresh ()
;;   (interactive)
;;   (set-buffer "*nrepl*")
;;   (goto-char (point-max))
;;   (insert "(clojure.tools.namespace.repl/refresh)")
;;   (nrepl-return))

;; (add-hook 'nrepl-mode-hook
;;           (lambda () (local-set-key (kbd "C-c r") 'nrepl-refresh)))


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
;; Eshell
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
     (split-window-sensibly (selected-window))
     (other-window 1)
     (ansi-term "/opt/local/bin/bash"))
    (switch-to-buffer-other-window "*ansi-term*")))

(defalias 'flip
  (lambda ()
    (cd "/ssh:smeltzek@flip.engr.oregonstate.edu:~/")))

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
;; Projectile
(projectile-global-mode)
(setq projectile-indexing-method 'native)
(global-set-key (kbd "M-p") 'projectile-find-file)


;;------------------------------------------------------------------------------
;; Racket
;; (require 'quack)


;;------------------------------------------------------------------------------
;; Misc things that should probably be in a different file
(defun reload-dot-emacs ()
  "Reload the default configuration file."
  (interactive)
  (load-file "~/.emacs")
  (message "Reloaded .emacs file..."))
;; (global-set-key (kbd "C-x C-r") 'reload-dot-emacs)

(defun open-with ()
  "Open current buffer with an external tool, such as a browser."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))
(global-set-key (kbd "C-c o") 'open-with)

;; TODO -- this currently only works for OSX
(defun open-file-externally (fname)
  "Interactively select a file to open with an external tool."
  (interactive (list (read-file-name "Open file: ")))
  (message "Path is %s" fname)
  (when fname
    (shell-command (concat
                    "open " fname))))

(defun open-line-below ()
  "Insert a line below regardless of point position.  Like Vim's 'o' command."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key [(shift return)] 'open-line-below)
(global-set-key (kbd "C-o") 'open-line-below)

(defun open-line-above ()
  "Open a new line above the current line, like Vim's 'O' command."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))
(global-set-key (kbd "C-S-o") 'open-line-above)


;;------------------------------------------------------------------------------
;; Poorly constructed tags stuff
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
;; Premake
(defun premake-find-exec ()
  "Find the Premake-generated makefile and grab the name of the executable."
  (interactive)
  (let ((ext (lambda ()
               (progn (search-forward "PROJECTS := ")
                      (buffer-substring (point) (line-end-position))))))
    (extract-exe-from-buildscript "Makefile" ext)))

(defun premake-run-exec ()
  "Find and run the executable built by Premake."
  (interactive)
  (run-buildscript-exe (lambda () (premake-find-exec))))


;;------------------------------------------------------------------------------
;; Scons stuff
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(defun scons-build ()
  "Find the 'SConstruct' file and run SCons on it."
  (interactive)
  (let ((sc-dir (find-file-upward-dir "SConstruct")))
    (cd sc-dir)
    (shell-command "scons")))

(defun scons-find-exec ()
  "Find the executable produced by SCons in a very failure-prone manner."
  (interactive)
  (let ((ext (lambda () (let ((strt (progn (search-forward "target = '")
                                           (point)))
                              (end (progn (search-forward "'")
                                          (backward-char)
                                          (point))))
                          (buffer-substring strt end)))))
    (message (extract-exe-from-buildscript "SConstruct" ext))))

(defun scons-run-exec ()
  "Find and run the executable built by SCons"
  (interactive)
  (run-buildscript-exe (lambda () (scons-find-exec))))


;;------------------------------------------------------------------------------
;; Configuration utilities
(defun find-file-upward-full (file)
  "Walks up from current directory until it finds a particular file."
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

(defun find-file-upward-dir (file)
  "Walks up from current directory until it finds a particular
file.  Return just the directory in which it's found."
  (file-name-directory (find-file-upward-full file)))

(defun extract-exe-from-buildscript (scriptname extractor)
  "Machinery to extract part of a build script."
  (interactive)
  (let ((buildscript (find-file-upward-full scriptname)))
    (with-temp-buffer
      (insert-file-contents buildscript)
      (split-string (buffer-string) "\n" t)
      (funcall extractor))))

(defun run-buildscript-exe (exe-getter)
  "Run the executable returned by EXE-GETTER"
  (let* ((exe-name (funcall exe-getter))
         (exe (find-file-upward-full exe-name)))
    (cd (file-name-directory exe))
    (shell-command exe)))


;;------------------------------------------------------------------------------
(server-start)
(message "%s" "You shouldn't have come back, Karl")

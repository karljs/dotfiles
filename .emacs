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
             '("melpa" . "https://melpa.org/packages/"))

;;------------------------------------------------------------------------------
;; Install packages as necessary on startup. Credit largely to Emacs Prelude.
(defvar my-packages '(ace-jump-mode
                      ag
                      auctex
                      auctex-latexmk
                      change-inner
                      elm-mode
                      exec-path-from-shell
                      expand-region
                      fastnav
                      flx-ido
                      gnugo
		      gruvbox-theme
                      haskell-mode
                      idris-mode
                      ido-vertical-mode
                      ;; latex-preview-pane
                      magit
                      markdown-mode
                      monokai-theme
                      paredit
                      projectile
                      rainbow-delimiters
                      smex
                      solarized-theme
                      spaceline
                      transpose-frame
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
;; Fix broken $PATH on OS X with GUI
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (setq default-directory "/Users/karl/"))

;;------------------------------------------------------------------------------
;; GUI Settings
(if (or (not (display-graphic-p)) (not (eq system-type 'darwin)))
    (menu-bar-mode -1))
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message ""
      frame-title-format "%b")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode -1)
(column-number-mode 1)
;; (set-fringe-mode 0)


;;------------------------------------------------------------------------------
;; Fonts, colors, aesthetics
(defun kjs-size-font ()
  (interactive)
  (concat "PragmataPro Mono" "-"
          (when window-system
            (if (> (x-display-pixel-width) 2000)
                "16"
              "14"))))

(let ((font-name (kjs-size-font)))
  ;; (add-to-list 'default-frame-alist (cons 'font font-name))
  (set-face-attribute 'default nil :font font-name))

;; (setq solarized-scale-org-headlines nil
;;       solarized-use-variable-pitch nil)
(setq-default line-spacing 0)
(load-theme 'monokai t)

;; Spaceline in particular
(require 'spaceline-config)
(setq powerline-default-separator 'nil
      ;; powerline-height 1.1
      )
(spaceline-emacs-theme)


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

(setq-default dired-use-ls-dired nil
              fill-column 78
              indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)


;;------------------------------------------------------------------------------
;; Default registers
(set-register ?e '(file . "~/.emacs"))

;;------------------------------------------------------------------------------
;; Global keybindings

;; (evil-mode 1)

(global-set-key (kbd "C-x a r") 'align-regexp)
;; (global-set-key (kbd "C-c %") 'replace-regexp)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

(global-set-key (kbd "C-c w") 'wrap-region)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-h") 'backward-kill-word)
;; (global-set-key (kbd "C-?") 'help-command)
;; (global-set-key (kbd "M-?") 'mark-paragraph)

;;------------------------------------------------------------------------------
;; Ace jump mode
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)


;;------------------------------------------------------------------------------
;; Spelling
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-reall-hunspell t))
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(setq-default ispell-program-name "aspell")


;;------------------------------------------------------------------------------
;; Auctex / LaTeX / latexmk
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(setq TeX-PDF-mode t
      TeX-auto-save t
      TeX-parse-self t
      reftex-plug-into-AUCTeX t
      LaTeX-indent-level 0
      LaTeX-item-indent 0
      TeX-brace-indent-level 0)
(customize-set-variable 'LaTeX-verbatim-environments
                        '("verbatim" "verbatim*" "program" "programc" "prog"
                          "BVerbatim"))

;; Use Skim.  Fix for other OS.
;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list
;;       '(("PDF Viewer"
;;          "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b"
;;          )))

;; Spell checking
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Synctex
;; (setq TeX-source-correlate-start-server nil)
;; (setq TeX-source-correlate-method 'synctex)
;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (turn-on-auto-fill)
                             (LaTeX-math-mode)
                             (turn-on-reftex)
                             (outline-minor-mode)))
; (auctex-latexmk-setup)

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
;; Org
(set-register ?o (cons 'file "~/todo.org"))
(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "HOLD" "REVIEW" "|" "DONE"))
      org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
      org-pretty-entities 1
      org-agenda-files (list "~/notes/todo.org"))
(define-key global-map "\C-ca" 'org-agenda)

;;------------------------------------------------------------------------------
;; Projectile
;; (projectile-global-mode)

;;------------------------------------------------------------------------------
;; Ido + Smex
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-use-faces nil)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;------------------------------------------------------------------------------
;; Commenting
(defun kjs-comment-or-uncomment-region-or-line ()
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

(defun kjs-test-web-mode ()
  "Handles web-modes stupid special commenting stuff"
  (interactive)
  (if (equal major-mode 'web-mode)
      (web-mode-comment-or-uncomment)
    (kjs-comment-or-uncomment-region-or-line)))

(global-set-key (kbd "C-x C-;") 'kjs-test-web-mode)

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
(global-set-key (kbd "C-c C-h") 'comment-header-line)

;;------------------------------------------------------------------------------
;; C/C++

;; TODO -- write some kind of magic function to automatically determine which
;; build system to use.  In the mean time, default to scons.
(defun kjs-c-mode-hook ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  ;; (local-set-key (kbd "C-c C-c") 'kjs-compile-func)
  ;; (local-set-key (kbd "C-c C-k") 'kjs-compile-clean-func)
  (local-set-key (kbd "C-c C-l") 'scons-build)
  (local-set-key (kbd "C-c C-r") 'scons-run-exec))
(add-hook 'c-mode-common-hook 'kjs-c-mode-hook)

(defun kjs-compile-func ()
  (interactive)
  (compile (format "make -C %s" (file-name-directory (get-closest-pathname)))))

(defun kjs-compile-clean-func ()
  (interactive)
  (compile (format "make -C %s clean"
                   (file-name-directory (get-closest-pathname)))))

;;------------------------------------------------------------------------------
;; Haskell

;; Haskell Mode dev
;; (add-to-list 'load-path "~/src/haskell-mode/")
;; (require 'haskell-mode-autoloads)

(add-hook 'haskell-mode-hook 'kjs-haskell-hook)
(defun kjs-haskell-hook ()
  (setq haskell-interactive-mode-hide-multi-line-errors nil
        haskell-tags-on-save t
        haskell-process-type 'auto)
  (haskell-decl-scan-mode)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  (define-key haskell-mode-map (kbd "C-c C-h") 'haskell-check))

;;------------------------------------------------------------------------------
;; Agda
(modify-syntax-entry ?⟪ "w")  ;; Emacs level up
(modify-syntax-entry ?⟫ "w")  ;; Customize the default syntax table

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
             ;; (shell-command-to-string "/Users/karl/bin/agda-mode locate")))
(add-hook 'agda2-mode-hook
          '(lambda ()
             (customize-set-variable
              'agda2-highlight-face-groups 'default-faces)
             ;; (setq agda2-program-args "-i" "/Users/karl/src/agda-stdlib/src")
             ))

;;------------------------------------------------------------------------------
;; Idris
(setq idris-interpreter-flags '("-p" "contrib"))
(add-hook 'idris-mode-hook
          '(lambda ()
             ;; (setq idris-semantic-source-highlighting t)
             (set-face-attribute 'idris-semantic-data-face nil
                                 :foreground nil
                                 :inherit 'font-lock-string-face)
             (set-face-attribute 'idris-semantic-type-face nil
                                 :foreground nil
                                 :inherit 'font-lock-string-face)
             (set-face-attribute 'idris-loaded-region-face nil
                                 :background nil)))

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

;;------------------------------------------------------------------------------
;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;------------------------------------------------------------------------------
;; FastNav
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)

;;------------------------------------------------------------------------------
;; Change Inner
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
;; (setq whitespace-style '(face lines))
;; (setq whitespace-line-column 80)
;; (add-hook 'prog-mode-hook 'whitespace-mode)

;;------------------------------------------------------------------------------
;; Web mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; This is a true hack, and should be fixed
(defun kjs-run-tidy ()
  (interactive)
  (shell-command "tidy-dir")
  (revert-buffer t t))

(defun kjs-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (define-key web-mode-map (kbd "C-c C-c t") 'kjs-run-tidy))
(add-hook 'web-mode-hook 'kjs-web-mode-hook)

;;------------------------------------------------------------------------------
;; Eshell and other terminals
(add-hook 'eshell-mode-hook
          (lambda () (setq pcomplete-cycle-completions nil)))

(add-hook 'term-mode-hook
          (lambda () (setq term-buffer-maximum-size 10000)))

;; (setq explicit-shell-file-name "/usr/local/bin/bash")

;; (defun visit-term-buffer ()
;;   "Create or visit a terminal buffer."
;;   (interactive)
;;   (if (not (get-buffer "*ansi-term*"))
;;       (progn
;;      (split-window-sensibly (selected-window))
;;      (other-window 1)
;;      (ansi-term))
;;     (switch-to-buffer-other-window "*ansi-term*")))

(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)

(defalias 'access
  (lambda ()
    (cd "/ssh:smeltzek@access.engr.oregonstate.edu:~/")))
(defalias 'nome
  (lambda ()
    (cd "/ssh:smeltzek@nome.eecs.oregonstate.edu:~/")))

(global-set-key (kbd "C-c t") 'eshell)

;;------------------------------------------------------------------------------
;; GLSL
;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.glvs\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.glfs\\'" . glsl-mode))

;;------------------------------------------------------------------------------
;; Tramp
(setq tramp-shell-prompt-pattern
      "^[^]#$%>\n]*[]#$%>]$? *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
(setq ido-enable-tramp-completion t)
(setq tramp-default-method "ssh")

;;------------------------------------------------------------------------------
;; Proof General & Coq
(load-file "/Users/karl/src/ProofGeneral/generic/proof-site.elc")
(add-to-list 'load-path "/usr/local/opt/coq/lib/emacs/site-lisp")
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
;; (add-hook 'coq-mode-hook #'company-coq-initialize)
(setq proof-splash-enable nil
      proof-electric-terminator-enable)

;;------------------------------------------------------------------------------
;; Eww
(global-set-key (kbd "C-c b") 'eww)

;;------------------------------------------------------------------------------
;; Gnu-Go
(setq gnugo-xpms 'gnugo-imgen-create-xpms)
(add-hook 'gnugo-start-game-hook 'gnugo-image-display-mode)

;;------------------------------------------------------------------------------
;; Prolog
;; TODO: Start caring about Perl
(add-hook 'prolog-mode-hook 'kjs-prolog-hook)
(setq prolog-system 'swi)
(defun kjs-prolog-hook ()
  (setq prolog-indent-width 4)
  (define-key prolog-mode-map (kbd "C-c C-l") 'prolog-compile-file))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;;------------------------------------------------------------------------------
;; Company
;; (add-hook 'after-init-hook 'global-company-mode)
;; (add-to-list 'company-backends 'company-ghc)

;;------------------------------------------------------------------------------
;; Dash
(global-set-key (kbd "C-c d") 'dash-at-point)

;;------------------------------------------------------------------------------
;; Expand Region
(global-set-key (kbd "C-=") 'er/expand-region)

;;------------------------------------------------------------------------------
;; Transpose Frame
;; (require 'transpose-frame)

;;------------------------------------------------------------------------------
;; Modified version of Brent's indent trickeration
;; (require 'vim-indent-mode)
;; (global-vim-indent-mode 1)

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
;; Load local files / packages
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "wrap")
(load-library "buildscript")
(load-library "tagutils")


;;------------------------------------------------------------------------------
(message "%s" "You shouldn't have come back, Karl")

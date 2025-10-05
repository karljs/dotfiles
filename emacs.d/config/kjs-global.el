;;; -*- lexical-binding: t; -*-

;;; kjs-global.el --- global, general configuration

;;; Commentary

;; This is configuration code that applies globally across Emacs and a
;; few of its built-in packages, like eshell and dired.  A few others,
;; like org and eglot are handled in more appropriate places.

;;; Code

;; Do this as early as possible, to avoid things being put in the
;; wrong place.
(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
	`(("." . ,(no-littering-expand-var-file-name "backups/")))))


(use-package exec-path-from-shell
  :ensure
  :demand t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))


(use-package emacs
  :ensure nil
  :demand t
  :init
  ;; performance and weird legacy stuff
  (setq gc-cons-percentage 0.5
        gc-cons-threshold (* 128 1024 1024))
  (setq load-prefer-newer t)
  (setq read-process-output-max (* 4 1024 1024))
  (setq process-adaptive-read-buffering nil)
  (setq max-lisp-eval-depth 10000)
  (setq auto-mode-case-fold nil)

  ;; aesthetics
  (let ((font-size
         (if (eq system-type 'darwin)
             170
           140)))
    (set-face-attribute 'default nil
		        :font "PragmataPro"
		        :height font-size)
    (set-face-attribute 'variable-pitch nil
                        :font "Inter"
                        :height font-size))
  (tool-bar-mode -1)
  (set-scroll-bar-mode nil)
  (when (not (eq system-type 'darwin))
    (menu-bar-mode -1))
  (setq ring-bell-function nil)
  (setq visible-bell t)
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors 'silent))
  (display-battery-mode)

  ;; macos keys
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'none
          dired-use-ls-dired nil))

  ;; basic behavior changes
  (recentf-mode)
  (setq save-interprogram-paste-before-kill t)
  (setq yank-pop-change-selection t)
  (setq help-window-select t)
  (setq enable-recursive-minibuffers t)
  (setq compilation-scroll-output t)
  (minibuffer-depth-indicate-mode 1)
  (setq-default indent-tabs-mode nil)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; turn on some disabled functions
  (mapc
   (lambda (fn)
     (put fn 'disabled nil))
   '(narrow-to-region narrow-to-page upcase-region downcase-region))

  ;; global keybinds
  :bind (([remap list-buffers] . ibuffer)
         ("C-M-y" . duplicate-dwim)
         ([remap zap-to-char] . zap-up-to-char)))


(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)


(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer t))


(use-package eshell
  :ensure
  :bind (("C-c t" . eshell)))



;;------------------------------------------------------------------------------
;; These utility functions are courtesy of
(defun kjs--push-mark-no-activate ()
  (interactive)
  (push-mark (point) t nil))

(defun kjs--jump-to-mark ()
  (interactive)
  (set-mark-command 1))

(defun kjs--exchange-point-and-mark-no-activate ()
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "C-M-`") 'jump-to-mark)
(global-set-key [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(provide 'kjs-global)

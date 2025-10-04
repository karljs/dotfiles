;; -*- lexical-binding: t; -*-

(use-package no-littering
  :ensure
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
	`(("." . ,(no-littering-expand-var-file-name "backups/")))))


(use-package emacs
  :demand t
  :init
  (setq gc-cons-percentage 0.5
        gc-cons-threshold (* 128 1024 1024))

  ;; aesthetics
  (let ((font-size
         (if (eq system-type 'darwin)
             170
           140)))
    (set-face-attribute 'default nil
		        :font "PragmataPro"
		        :height font-size))
  (tool-bar-mode -1)
  (set-scroll-bar-mode nil)
  (when (not (eq system-type 'darwin))
    (menu-bar-mode -1))
  (setq ring-bell-function nil)
  (setq visible-bell t)
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors 'silent))

  ;; macos
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

  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

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
  :hook (dired-mode . dired-hide-details-mode))


(use-package exec-path-from-shell
  :ensure
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))


(use-package eshell
  :ensure
  :bind (("C-c t" . eshell))
  :hook
  (eshell-hist-mode . (lambda ()
                        (define-key eshell-hist-mode-map
                                    (kbd "C-c M-p") nil))))



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

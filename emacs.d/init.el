;;  -*- lexical-binding: t; -*-

;; _____       __________      ______
;; ___(_)_________(_)_  /_________  /
;; __  /__  __ \_  /_  __/_  _ \_  /
;; _  / _  / / /  / / /___/  __/  /
;; /_/  /_/ /_//_/  \__/(_)___//_/
;;

(setq custom-file null-device)

;; package manager and bootstrap use-package
(setq package-enable-at-startup nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; load all my local config files
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(use-package kjs-global
  :ensure nil)
(use-package kjs-note
  :ensure nil)
(use-package kjs-lsp
  :ensure nil)
(use-package kjs-nav
  :ensure nil)
(use-package kjs-misc
  :ensure nil)
(use-package kjs-complete
  :ensure nil)
(use-package kjs-prog
  :ensure nil)
(use-package kjs-deb
  :ensure nil)

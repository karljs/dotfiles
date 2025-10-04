;; -*- lexical-binding: t; -*-

(use-package corfu
  :ensure
  :init
  (global-corfu-mode)
  :config
  (setq tab-always-indent 'complete))


(use-package cape
  :ensure
  :bind ("C-<tab>" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))


(use-package bash-completion
  :ensure
  :config
  (bash-completion-setup))


(provide 'kjs-complete)

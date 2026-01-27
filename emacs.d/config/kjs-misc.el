;;  -*- lexical-binding: t; -*-

(use-package doom-modeline
  :ensure t
  :init
  (setq nerd-icons-font-family "PragmataPro")
  :hook (after-init . doom-modeline-mode))


(use-package substitute
  :ensure t
  :demand t
  :bind (("C-c s" . substitute-prefix-map)))


(use-package diminish
  :ensure t)


(use-package helpful
  :ensure
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))


(use-package ace-window
  :ensure
  :bind ("M-o". ace-window))


(use-package avy
  :ensure
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char))


(use-package change-inner
  :ensure
  :bind (("M-i" . change-inner)))


(use-package jinx
  :ensure
  :diminish jinx-mode
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-\"" . jinx-correct)))


(use-package vundo
  :ensure)


(use-package whole-line-or-region
  :ensure t
  :diminish
  (whole-line-or-region-mode whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode nil))


(use-package pdf-tools
  :ensure
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))


(use-package lin
  :ensure t
  :config
  (setq lin-mode-hooks
        '(dired-mode-hook
          git-rebase-mode-hook
          grep-mode-hook
          ibuffer-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          package-menu-mode-hook
          pdf-outline-buffer-mode-hook
          proced-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1))


(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-subtle-frame-lines t)
  (spacious-padding-mode 1))


(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  ;; (load-theme 'modus-operandi t)
  )


(use-package ef-themes
  :ensure t
  :config
  ;; (load-theme 'ef-dream t)
  )


(use-package standard-themes
  :ensure t)


(use-package monokai-pro-theme
  :ensure t
  :config
  (load-theme 'monokai-pro t))


(use-package rainbow-delimiters
  :ensure
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package ansi-color
  :ensure t
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))


(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'gemini-2.5-pro
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (getenv "GEMINI_API_KEY")
                        :stream t))
  :hook
  (gptel-post-stream-hook . gptel-auto-scroll))


(use-package transient
  :ensure t)


;; Probably break this out to a new file
(use-package notmuch
  :preface
  (defun kjs-sendmail-via-gmi ()
    (let ((sendmail-program "gmi-send.sh"))
      (message-send-mail-with-sendmail)))

  :ensure t
  :bind (("C-c m" . notmuch)
         :map notmuch-search-mode-map
         ("r" . (lambda ()
                  (interactive)
                  (notmuch-search-tag '("-unread")))))

  :config
  (setq notmuch-database-path "~/.mail/canonical.gmail/")
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "sent" :query "tag:sent" :key "t")
          (:name "all mail" :query "*" :key "a")))

  (setq sendmail-program "gmi-send.sh"
        message-send-mail-function #'kjs-sendmail-via-gmi
        notmuch-fcc-dirs nil
        notmuch-always-prompt-for-sender 'nil)

  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;       smtpmail-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-service 587
  ;;       smtpmail-stream-type 'starttls
  ;;       )
  (add-hook 'notmuch-show-hook
            (lambda ()
              (notmuch-show-tag-all '("-unread"))))
  (setq smtpmail-debug-info t
        smtpmail-debug-verb t)
  (setq notmuch-address-internal-completion '("from" "to" "cc" "bcc"))
  (notmuch-address-setup))


(use-package w3m
  :ensure t
  :config
  ;; (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-default-display-inline-images t)
  (setq w3m-use-cookies t)
  (setq mm-text-html-renderer 'w3m)
  (setq mm-inline-large-images 'resize))


(use-package notmuch-indicator
  :ensure t
  :config
  (notmuch-indicator-mode)
  )


(use-package smtpmail
  :ensure t)


(use-package jira
  :ensure t
  :config
  (setq jira-base-url "https://warthogs.atlassian.net")
  (setq jira-username "karl.smeltzer@canonical.com")
  (setq jira-token (funcall (plist-get (car (auth-source-search :host "jira" :max 1)) :secret)))
  (setq jira-api-version 3))


(use-package eat
  :ensure t
  :bind (("C-c t" . eat)))


(provide 'kjs-misc)

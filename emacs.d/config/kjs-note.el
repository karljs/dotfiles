;;; -*- lexical-binding: t; -*-

;;; kjs-note.el --- note-taking, org, denote

;;; Commentary

;; This is configuration for note-taking packages, which basically
;; means org and denote.  I don't do much with org currently, because
;; trying to keep it in sync with other tools like online issue
;; trackers and calendars just isn't worth it for me.

;;; Code

(use-package org
  :ensure
  :config
  (setq org-pretty-entities t)
  (setq org-directory (expand-file-name "~/Documents/notes"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))


(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:map global-map
        ("C-c n n" . denote)
        ("C-c n r" . denote-rename-file)
        ("C-c n l" . denote-link)
        ("C-c n b" . denote-backlinks)
        ("C-c n d" . denote-dired)

        :map dired-mode-map
        ("C-c C-d C-i" . denote-dired-link-marked-notes)
        ("C-c C-d C-r" . denote-dired-rename-files)
        ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
        ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("emacs" "ubuntu" "personal"))
  (setq denote-sort-keywords t)
  (denote-rename-buffer-mode 1))


(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1)
  (setq consult-denote-grep-command 'consult-ripgrep))


(provide 'kjs-note)

;;; tagutils.el --- Automatically generate appropriate tag files

;; Author: Karl Smeltzer <karl.smeltzer@gmail.com>
;; Package-Version: 0.1
;; URL: http://github.com/karljs/tagutils

;;; Commentary:

;; This package provides some simple helper utilities for automatically
;; generating tag files appropriate for the language being used.  This is
;; useful for languages that aren't covered by Exuberant Ctags, such as
;; Haskell.
;;
;; This is very primitive and not really intended for wider use, as it does not
;; support many languages, nor mixed-language projects, or anything else fancy.

;;; Code:

;;;###autoload
(defun tagutils-haskell-tags (dir)
  "Generate Haskell tags using Hasktags"
  (interactive)
  (shell-command (concat "cd " dir "&& hasktags -e -o ./TAGS ."))
  (message "Created Haskell tags."))

;;;###autoload
(defun tagutils-ctags-tags (dir)
  "Create ctags"
  (interactive)
  (shell-command (concat "cd " dir "&& ctags -e -R ."))
  (message "Created ctags."))

;;;###autoload
(defun tagutils-gen-tags (dir)
  "Look at the current major mode and generate tags for the project"
  (interactive (list (read-directory-name "Directory: ")))
  (if (equal major-mode 'haskell-mode)
      (htags dir)
    (ctags dir)))
;; (global-set-key (kbd "C-c t") 'tags)

(provide 'tagutils)

;;; tagutils.el ends here

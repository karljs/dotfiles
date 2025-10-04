;;  -*- lexical-binding: t; -*-


(defvar kjs-last-compile-dir nil)


(defun kjs-get-compile-dir ()
  (let* ((initial-dir (or kjs-project-compile-dir default-directory))
         (compile-dir (completing-read)))))


(defun kjs-get-compile-dir (prompt)
  "Prompt for a directory using PROMPT, with `kjs-last-compile-dir` as the default.
Updates `kjs-last-compile-dir` with the user's choice and returns it."
  (let ((dir (read-directory-name prompt kjs-last-compile-dir)))
    (setq kjs-last-compile-dir dir)
    dir))


(defun kjs-source-package (dir)
  "Use dpkg-buildpackage to build a source package"
  (interactive
   (list (kjs-get-compile-dir "Working directory")))
  (let ((default-directory dir))
    (kjs-run-compile-command "dpkg-buildpackage -S -I -i -nc -d -sa" "dpkg-buildpackage")))


(defun kjs-lintian (dir)
  "Run lintian on a deb package"
  (interactive
   (list (kjs-get-compile-dir "Working directory")))
  (let ((default-directory dir))
    (kjs-run-compile-command "lintian -i --tag-display-limit 0 --color never" "lintian")))


(defun kjs-sbuild (distro dir)
  "Build the complete deb"
  (interactive
   (list
    (completing-read "Select distro target: " '("noble" "plucky" "jammy"))
    (list (kjs-get-compile-dir "Working directory"))))
  (let ((default-directory dir))
    (kjs-run-compile-command (concat "sbuild . -Ad " distro) "sbuild")))


(defun kjs-subst-var (var value)
  "Slightly simplified replace"
  (interactive
   (list
    (completing-read "Variable to replace: " '("@RUST_VERSION@"))
    (read-string "Variable value: ")))
  (substitute-target-in-buffer (regexp-quote var) value))


(defun kjs-run-compile-command (command buffer-name)
  "Run COMMAND, sending output to a uniquely named buffer."
  (let ((compilation-buffer-name-function
         (lambda (mode)
           (if (string-match-p "^\*" buffer-name)
               buffer-name
             (format "*%s-%s*" buffer-name (format-time-string "%Y%m%d-%H%M%S"))))))
    (compile command)))


(transient-define-prefix kjs-popup ()
  "A nice little menu for my helper functions"
  [
   ["Debian packaging"
    ;; The individual options are defined as lists (...)
    ("s" "Build source package (dpkg-buildpackage)" kjs-source-package)
    ("l" "Run linter (lintian)" kjs-lintian)
    ("b" "Build package (sbuild)" kjs-sbuild)]
   ["Variable replacement"
    ("v" "Substitue variable" kjs-subst-var)]])


(global-set-key (kbd "C-c C-k") 'kjs-popup)


(provide 'kjs-deb)

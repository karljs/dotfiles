;;; -*- lexical-binding: t; -*-

;;; kjs-deb.el --- code to simplify Debian/Ubuntu package maintenance

;;; Commentary

;; Since taking on some Ubuntu packaging duties, I've been frustrated
;; at the ecosystem of tooling that exists for maintaining `deb'
;; packages.  Doing basic tasks requires remembering tons of obtuse
;; flags and switches, which takes up valuable space in my brain.
;;
;; Accordingly, this is a very raw collection of simple helper
;; functions to run various tools via Emacs compilation buffers, and
;; my attempt to add a few of the important configuration options into
;; transients (from URL `https://magit.vc/manual/transient/').

;;; Code

(use-package kjs-transient
  :ensure nil)


(defvar kjs-last-compile-dir nil
  "The last-used working directory.")


(defun kjs--get-compile-dir ()
  "Get the working directory for a compilation command.
Often, these tools need to run in a directory outside of the git
repository that holds the actual package source, which makes it
difficult to use something like `package.el'.  It's often the parent
directory of the package root, which might be a nice default to
implement in the future."
  (let ((dir (read-directory-name "Working directory: " kjs-last-compile-dir)))
    (setq kjs-last-compile-dir dir)
    dir))


(defun kjs--run-dpkg-buildpackage (dir args)
  "Call `dpkg-buildpackage' as a compile command."
  (let ((default-directory dir))
    (kjs-run-compile-command (string-join (cons "dpkg-buildpackage" args) " ") "dpkg-buildpackage")))


(defun kjs--prep-dpkg-buildpackage (&optional args)
  "Call `dpkg-buildpackage' with transient args."
  (interactive (list (transient-args 'kjs--dpkg-buildpackage-transient)))
  (let* ((build-types '())
         (final-args '()))
    (dolist (arg args)
      (cond
       ((string= arg "-S") (push "source" build-types))
       ((string= arg "-B") (push "any" build-types))
       ((string= arg "-A") (push "all" build-types))
       (t (push arg final-args))))
    (when build-types
      (push (format "--build-type=%s"
                    (string-join (nreverse build-types) ","))
            final-args))
    (kjs--run-dpkg-buildpackage (kjs--get-compile-dir) final-args)))


(defun kjs-lintian (dir)
  "Run `lintian' on a deb package"
  (interactive
   (list (kjs--get-compile-dir)))
  (let ((default-directory dir))
    (kjs-run-compile-command "lintian -i --tag-display-limit 0 --color never" "lintian")))


(defun kjs-sbuild (distro dir)
  "Build the complete deb"
  (interactive
   (list
    (completing-read "Select distro target: " '("noble" "plucky" "jammy"))
    (list (kjs--get-compile-dir))))
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
             (format "*%s-%s*"
                     buffer-name
                     (format-time-string "%Y%m%d-%H%M%S"))))))
    ;; (compile command)
    (message "%S" command)
    ))


;;; Transient prefixes to call these functions

(transient-define-prefix kjs--dpkg-buildpackage-transient ()
  "Options to pass to `dpkg-buildpackage'.
This is not comprehensive, but instead just exposes some of the
most-used configuration options."
  :value '("-S" "-I" "-i" "-nc" "-d" "-sa")
  ;; top row
  [["Build type (1+)"
    ("-S" "source build" "-S")
    ("-B" "any binary (arch specific)" "-B")
    ("-A" "all binary (arch indep)" "-A")]

   ["dpkg-source"
    ("-I" "tar ignore pattern" "-I"
     :class kjs--transient-tristate)
    ("-i" "diff ignore pattern" "-i"
     :class kjs--transient-tristate)]]

  ;; bottom row
  [["dpkg-genchanges"
    ("-sa" "include source" "-sa")]

   ["Other flags"
    ("-nc" "no pre-clean" "-nc")
    ("-d" "no check builddeps" "-d")]

   ["Run command"
    ("b" "Run build" kjs--prep-dpkg-buildpackage)]])


(transient-define-prefix kjs-deb-transient ()
  "Transient for deb helper functions"
  [
   ["Debian packaging"
    ("b" "dpkg-buildpackage" kjs--dpkg-buildpackage-transient)
    ("l" "lintian" kjs-lintian)
    ("s" "sbuild" kjs-sbuild)]
   ["Variable replacement"
    ("v" "Substitute variable" kjs-subst-var)]])


(global-set-key (kbd "C-c C-k") 'kjs-deb-transient)


(provide 'kjs-deb)

;;; kjs-deb.el ends here

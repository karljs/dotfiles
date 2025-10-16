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


(defvar kjs--last-compile-dir nil
  "The last-used working directory.")


(defun kjs--get-compile-dir ()
  "Get the working directory for a compilation command.
Often, these tools need to run in a directory outside of the git
repository that holds the actual package source, which makes it
difficult to use something like `package.el'.  It's often the parent
directory of the package root, which might be a nice default to
implement in the future."
  (let ((dir (read-directory-name "Working directory: "
                                  kjs--last-compile-dir)))
    (setq kjs--last-compile-dir dir)
    dir))


(defvar kjs--last-target-file nil
  "The last-used target file.")


(defun kjs--get-target-file ()
  "Prompt the user for a file.
The reason this exists is forward-compatibility. I am planning to enable
some basic, configurable validation."
  (let ((file (read-file-name "Target: "
                              kjs--last-target-file)))
    (setq kjs--last-target-file file)
    file))


(defun kjs-run-compile-command (command buffer-name)
  "Run COMMAND, sending output to a compilation buffer."
  (let ((compilation-buffer-name-function
         (lambda (mode)
           (if (string-match-p "^\*" buffer-name)
               buffer-name
             (format "*%s-%s*"
                     buffer-name
                     (format-time-string "%Y%m%d-%H%M%S"))))))
    (compile command)))


(defun kjs-subst-var (var value)
  "Extra simple variable replacement"
  (interactive
   (list
    (completing-read "Variable to replace: " '("@RUST_VERSION@"))
    (read-string "Variable value: ")))
  (substitute-target-in-buffer (regexp-quote var) value))


;;; dpkg-buildpackage

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
      (push (format "--build=%s"
                    (string-join (nreverse build-types) ","))
            final-args))
    (kjs--run-dpkg-buildpackage (kjs--get-compile-dir) final-args)))


(defun kjs--run-dpkg-buildpackage (dir args)
  "Call `dpkg-buildpackage' as a compile command."
  (let ((default-directory dir))
    (kjs-run-compile-command
     (string-join (cons "dpkg-buildpackage" args) " ")
     "dpkg-buildpackage")))


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

   ["Build package"
    ("b" "Run build" kjs--prep-dpkg-buildpackage)]])


;;; lintian

(transient-define-prefix kjs--lintian-transient ()
  "Options to pass to `lintian'.
Not comprehensive, but just some of the flags I tend to use. "
  :value '("-i" "--tag-display-limit=0" "--color=never")
  [["Output options"
    ("-i" "info" "-i")
    ("-t" "tag display limit" "--tag-display-limit=")
    ("-c" "color" "--color=" :choices (auto never always html) :always-read t :allow-empty nil)
    ]]
  [["Run lintian"
    ("l" "lint" kjs--prep-lintian)]])


;;; sbuild

(defclass kjs--sbuild-url-arg (transient-option)
  ())

(defconst kjs--ppa-template "deb [trusted=yes] http://ppa.launchpadcontent.net/%s/ubuntu/ %s main")

(defconst kjs--extra-repo-arg "--extra-repository=")


(transient-define-argument kjs--sbuild-ppa-arg ()
  "Simplify adding PPA to sbuild environment."
  :class 'kjs--sbuild-url-arg
  :argument kjs--extra-repo-arg
  :reader (lambda (prompt initial-input history)
            (completing-read
             prompt
             '("rust-toolchain/staging")
             nil t initial-input history)))


(cl-defmethod transient-format-value ((obj kjs--sbuild-url-arg))
  "Ensure custom PPA arg has the right face"
  (let ((value (oref obj value)))
    (if (and value (not (string-empty-p value)))
        (propertize value 'face 'transient-argument)
      "")))


(defun kjs--prep-sbuild (&optional args)
  "Call `sbuild' with transient args."
  (interactive (list (transient-args 'kjs--sbuild-transient)))
  (let* ((dist (transient-arg-value "--dist=" args))
         (ppa (transient-arg-value kjs--extra-repo-arg args)))
    (let ((modified-args
           (mapcar (lambda (arg)
                     (if (string-prefix-p kjs--extra-repo-arg arg)
                         (concat kjs--extra-repo-arg
                                 (shell-quote-argument
                                  (format kjs--ppa-template ppa dist)))
                       arg))
                   args)))
      (kjs--run-sbuild (kjs--get-target-file) modified-args))))


(defun kjs--run-sbuild (target args)
  "Run `sbuild' on a package."
  (let ((default-directory (file-name-directory target)))
    (kjs-run-compile-command
     (concat
      (string-join (cons "sbuild" args) " ")
      " "
      target)
     "sbuild")))


(transient-define-prefix kjs--sbuild-transient ()
  "Options to pass to `sbuild'."
  :value '("-A" "--dist=noble")
  [["Target options"
    ("-A" "arch-all" "-A")
    ("-d" "distro" "--dist="
     :choices (questing noble jammy focal)
     :always-read t
     :allow-empty nil)]
   ["PPA"
    ("-e" "extra repo" kjs--sbuild-ppa-arg)
    ]
   ;; TODO: pick chroot from a list
   [ "Run sbuild"
     ("s" "sbuild" kjs--prep-sbuild)
     ]
   ]
  )


;;; uscan

(defun kjs--prep-uscan (&optional args)
  "Call `uscan` with transient args."
  (interactive (list (transient-args 'kjs--uscan-transient)))
  (kjs--run-uscan (kjs--get-compile-dir) args))


(defun kjs--run-uscan (dir args)
  "Run `uscan` on your package"
  (let ((default-directory dir))
    (kjs-run-compile-command
     (string-join (cons "uscan" args) " ") "uscan")))


(transient-define-prefix kjs--uscan-transient ()
  "Options to pass to `uscan'."
  :value '("--download-version=1.87")
  [
   ["Download"
    ("-d" "download version" "--download-version=" :prompt "Version: " :always-read t :allow-empty nil)
    ("u" "uscan" kjs--prep-uscan)]]
  )


;;; General


(transient-define-prefix kjs-deb-transient ()
  "Transient for deb helper functions"
  [
   ["Debian packaging"
    ("b" "dpkg-buildpackage" kjs--dpkg-buildpackage-transient)
    ("l" "lintian" kjs--lintian-transient)
    ("s" "sbuild" kjs--sbuild-transient)
    ("u" "uscan" kjs--uscan-transient)]
   ["Variable replacement"
    ("v" "Substitute variable" kjs-subst-var)]])


(global-set-key (kbd "C-c `") 'kjs-deb-transient)


(provide 'kjs-deb)

;;; kjs-deb.el ends here

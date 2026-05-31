;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Startup Performance

;; Maximize GC threshold during startup for speed
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Disable file-name-handler during startup
(defvar kjs--default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore reasonable values after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024)) ; 128MB (matches your init.el preference)
            (setq gc-cons-percentage 0.5)
            (setq file-name-handler-alist kjs--default-file-name-handler-alist)))

;;; Prevent UI Flicker

;; Disable UI elements before first frame is created
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (width . 140)
        (height . 42)))

;; Re-enable menu-bar on macOS (will be set properly in init.el)
(when (eq system-type 'darwin)
  (push '(menu-bar-lines . 1) default-frame-alist))

(setq frame-inhibit-implied-resize t)

;;; Native Compilation

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent))

;;; macOS Native Compilation Library Paths

(defun homebrew-gcc-paths ()
  "Return GCC library paths from Homebrew installations.
Detects paths for gcc and libgccjit packages to be used in LIBRARY_PATH."
  (let* ((paths '())
         (brew-bin (or (executable-find "brew")
                       (let ((arm-path "/opt/homebrew/bin/brew")
                             (intel-path "/usr/local/bin/brew"))
                         (cond
                          ((file-exists-p arm-path) arm-path)
                          ((file-exists-p intel-path) intel-path))))))

    (when brew-bin
      ;; Get gcc paths.
      (let* ((gcc-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix gcc"))))
             (gcc-lib-current (expand-file-name "lib/gcc/current" gcc-prefix)))
        (push gcc-lib-current paths)

        ;; Find apple-darwin directory.
        (let* ((default-directory gcc-lib-current)
               (arch-dirs (file-expand-wildcards "gcc/*-apple-darwin*/*[0-9]")))
          (when arch-dirs
            (push (expand-file-name
                   (car (sort arch-dirs #'string>)))
                  paths))))

      ;; Get libgccjit paths
      (let* ((jit-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix libgccjit"))))
             (jit-lib-current (expand-file-name "lib/gcc/current" jit-prefix)))
        (push jit-lib-current paths)))

    (nreverse paths)))

(defun setup-macos-native-comp-library-paths ()
  "Set up LIBRARY_PATH for native compilation on macOS.
Includes Homebrew GCC paths and CommandLineTools SDK libraries."
  (let* ((existing-paths (split-string (or (getenv "LIBRARY_PATH") "") ":" t))
         (gcc-paths (homebrew-gcc-paths))
         (clt-paths '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
         (unique-paths (delete-dups
                        (append existing-paths gcc-paths clt-paths))))

    (setenv "LIBRARY_PATH" (mapconcat #'identity unique-paths ":"))))

;; Set up library paths for native compilation on macOS.
(when (eq system-type 'darwin)
  (setup-macos-native-comp-library-paths))

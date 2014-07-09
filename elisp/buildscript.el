;;------------------------------------------------------------------------------
;; Premake
(defun premake-find-exec ()
  "Find the Premake-generated makefile and grab the name of the executable."
  (interactive)
  (let ((ext (lambda ()
               (progn (search-forward "PROJECTS := ")
                      (buffer-substring (point) (line-end-position))))))
    (extract-exe-from-buildscript "Makefile" ext)))

(defun premake-run-exec ()
  "Find and run the executable built by Premake."
  (interactive)
  (run-buildscript-exe (lambda () (premake-find-exec))))

;;------------------------------------------------------------------------------
;; Scons stuff
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(defun scons-build ()
  "Find the 'SConstruct' file and run SCons on it."
  (interactive)
  (let ((sc-dir (find-file-upward-dir "SConstruct")))
    (cd sc-dir)
    (shell-command "scons")))

(defun scons-find-exec ()
  "Find the executable produced by SCons in a very failure-prone manner."
  (interactive)
  (let ((ext (lambda () (let ((strt (progn (search-forward "target = '")
                                           (point)))
                              (end (progn (search-forward "'")
                                          (backward-char)
                                          (point))))
                          (buffer-substring strt end)))))
    (message (extract-exe-from-buildscript "SConstruct" ext))))

(defun scons-run-exec ()
  "Find and run the executable built by SCons"
  (interactive)
  (run-buildscript-exe (lambda () (scons-find-exec))))


;;------------------------------------------------------------------------------
;; Configuration utilities
(defun find-file-upward-full (file)
  "Walks up from current directory until it finds a particular file."
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

(defun find-file-upward-dir (file)
  "Walks up from current directory until it finds a particular
file.  Return just the directory in which it's found."
  (file-name-directory (find-file-upward-full file)))

(defun extract-exe-from-buildscript (scriptname extractor)
  "Machinery to extract part of a build script."
  (interactive)
  (let ((buildscript (find-file-upward-full scriptname)))
    (with-temp-buffer
      (insert-file-contents buildscript)
      (split-string (buffer-string) "\n" t)
      (funcall extractor))))

(defun run-buildscript-exe (exe-getter)
  "Run the executable returned by EXE-GETTER"
  (let* ((exe-name (funcall exe-getter))
         (exe (find-file-upward-full exe-name)))
    (cd (file-name-directory exe))
    (shell-command exe)))

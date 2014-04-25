;; @file pathnames.cl
;; @breif Function definitions for the "com.gigamonkeys.pathnames" package
;; @package com.gigamonkeys.pathnames
;;
(in-package :com.gigamonkeys.pathnames)

; Check whether or not the specific component is presented, that is, not NIL
; and not equal to `:unspecific`
(defun component-present-p (component)
  (and component (not (eql component :unspecific))))

; Check whether or not the pathname specified is of directory form
(defun directory-pathname-p (p)
  (and
    (not (component-present-p (pathname-name p)))
    (not (component-present-p (pathname-type p)))
    p))

; Convert the pathname(or path string) specified to pathname of directory form
(defun pathname-as-directory (path)
  (let ((p (pathname path)))
    (when (wild-pathname-p p)
      (error "Can't reliably convert wild pathname."))
    (if (not (directory-pathname-p p))
      (make-pathname
       :directory (append (or (pathname-directory p) (list :relative))
                          (list (file-namestring p)))
       :name      nil
       :type      nil
       :defaults  p)
      p)))
; Convert the pathname(or path string) to pathname of file form
(defun pathname-as-file (path)
  (let ((p (pathname path)))
    (when (wild-pathname-p p)
      (error "Can't reliably convert wild pathname."))
    (if (directory-pathname-p p)
      (let* ((directory-part (pathname-directory p))
             (name-and-type (pathname (first (last directory-part)))))
        (make-pathname
         :directory (butlast directory-part)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults p))
      p)))

; Use read-time conditionalization to work around the `DIRECTORY` quirk in
; CLISP, that is, instead of `:wild`, NIL must be set on `:type` component to
; list files with no extension name, such as 'README'.
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

; List all files in the directory specified, whatever form it is in
; (file/directory).
; Many read-time conditionals were used to return subdirectories in directory
; form
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory name."))
  (let ((wildcard (directory-wildcard dirname)))
    #+(or sbcl cmucl lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (append
      ; sub-directories
      (directory (clisp-sub-directories-wildcard dirname))
      ; files
      (directory wildcard))

    #-(or sbcl cmucl lispworks allegro clisp)
    (error "list-directory not implemented")))
; Sub-directories wildcard for CLISP conditional, to be passed to `DIRECTORY`
; to list sub-directories
(defun clisp-sub-directories-wildcard (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory name."))
  (let ((wildcard (directory-wildcard dirname)))
    (make-pathname
     :name nil
     :type nil
     :directory (append (pathname-directory wildcard) (list :wild))
     :defaults wildcard)))

; Probe the pathname specified, return a file form pathname if the pathname
; names a file(in whichever form) and exists, or return a directory form
; pathname if the path names a directory(in whichever form) and exists,
; otherwise, returns NIL.
; Special handlings are given to CLISP, cause it again has some quirks
(defun file-exists-p (path)
  #+(or sbcl lispworks openmcl)
  (probe-file path)

  #+(or allegro cmucl)
  (or (probe-file (pathname-as-directory path))
      (probe-file pathname))

  #+clisp
  (or
    (ignore-errors
      (let ((directory-form (pathname-as-directory path)))
        (when (ext:probe-directory directory-form) directory-form)))
    (ignore-errors (probe-file (pathname-as-file path))))

  #-(or sbcl lispworks openmcl allegro cmucl clisp)
  (error "file-exists-p not implemented"))

; Walk through the directory, apply the function specified to every file in the
; directory, recursively. Two keywords are supported: :directories and :test.
; If ':directories' is true, the function would be applied to directories as
; well. If ':test' is speicified, the function would be applied only when the
; function specified by ':test' returns true.
(defun walk-directory (dirname fn &key ((:directories apply-to-directories) nil)
                                       ((:test to-apply-p) nil))
  (dolist (p (list-directory dirname))
    (when (or (not to-apply-p) (funcall to-apply-p p))
      (if (component-present-p (pathname-name p))
        ; file
        (funcall fn p)
        ; directory
        (progn
          (if apply-to-directories (funcall fn p))
          (walk-directory p fn :directories apply-to-directories
                              :test to-apply-p))
      ))))
; [Book] Implementations from the book (use `labels` and inner recursive).
; There are differences between the two implementations. The impl above does
; nothing when a file path is passed in as 'dirname', while the impl below
; would treat the file as directory when `.directories` is true. Besides,
; when `:directories` is true, impl below would process the directory passed
; in, while impl above would not.
; @note that the impl below uses `labels` and innner recursive so that it does
; not need to pass most the arguments when invoking recursion, this is quite
; handy when there are many arguments.
(defun walk-directory-1 (dirname fn &key directories (test (constantly t)))
  (labels
    ((walk (name)
       (cond
         ((directory-pathname-p name)
          (when (and directories (funcall test name))
            (funcall fn name))
          (dolist (x (list-directory name)) (walk x)))
         ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

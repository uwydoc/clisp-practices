;; @file packages.cl
;;
;; @breif Package definition and function declarations, also to avoid name
;; collision.
;;
(in-package :cl-user)

(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export
    :list-directory
    :file-exists-p
    :directory-pathname-p
    :file-pathname-p
    :pathname-as-directory
    :pathname-as-file
    :walk-directory
    :walk-directory-1
    :directory-p
    :file-p))

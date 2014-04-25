;; @file load-files.cl
;;
;; @breif Compile and load multiple files
;;
(defun compile-load (&rest files)
  (dolist (file files)
    (load (compile-file file))))

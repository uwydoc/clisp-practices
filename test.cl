;; @file test.cl
;;
;; @breif Misc. tests
;;

; Use `nreverse` in return value
(defun upto (max)
  (let ((result nil))
    (do ((n 1 (1+ n)))
        ((> n max) (nreverse result))
      (push n result))))
;

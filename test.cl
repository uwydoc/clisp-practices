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
; Test the invocation order of the three auxiliary methods (:before, :after,
; :around)
(defgeneric test-aux-order (name)
  (:documentation "Test invocation order of the three auxiliary methods."))
; Primary method (t)
(defmethod test-aux-order ((name t))
  (format t "[primary] ~a~%" t))
; Auxiliary methods (t)
(defmethod test-aux-order :before ((name t))
  (format t "[before] ~a~%" t))
(defmethod test-aux-order :after ((name t))
  (format t "[after] ~a~%" t))
(defmethod test-aux-order :around ((name t))
  (format t "[around] ~a~%" t)
  (call-next-method))
; Primary method (string)
(defmethod test-aux-order ((name string))
  (format t "[primary] ~a~%" name)
  (call-next-method))
; Auxiliary methods (string)
(defmethod test-aux-order :before ((name string))
  (format t "[before] ~a~%" name))
(defmethod test-aux-order :after ((name string))
  (format t "[after] ~a~%" name))
(defmethod test-aux-order :around ((name string))
  (format t "[around] ~a~%" name)
  (call-next-method))

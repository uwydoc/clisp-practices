;; @file macros.cl
;;
;; @breif Examples using macros
;;

; `progn` implementation with lambda function
(defmacro progn1 (&rest body)
  `(funcall (lambda () ,@body)))
; `when` macro
(defmacro when1 (condition &rest body)
  `(if ,condition (progn1 ,@body)))
; `unless` macro
(defmacro unless1 (condition &rest body)
  `(if (not ,condition) (progn1 ,@body)))
; `cond` macro
(defmacro cond1 (&rest branches)
  `(dolist (branch ,branches)
    (let ((condition (pop branch)) (body branch))
      (if condition (progn @body) break))))
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
; `cond1` macro
(defmacro cond1 ((condition body))
  (let ((condition-value condition))
    (when condition-value `(progn ,body ,condition-value))))
; `cond` macro
(defmacro cond-a (&body branches)
  (dolist (branch branches)
    (let ((condition (pop branch)) (body branch))
      (when condition (return `(progn ,@body))))))
(defmacro cond-b (&body branches)
  (with-gensyms (branch condition body)
    (dolist (branch branches)
      (let ((condition (pop branch)) (body branch))
        (when condition (return `(progn ,@body)))))))
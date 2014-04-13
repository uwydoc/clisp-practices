;; @file test.cl
;;
;; @breif Places to test things
;;

; Optionals function parameters &optional
(defun test-opt-func-params (a b &optional (c 3 c-supplied-p))
 "Optional function parameters with default value and flag indicating whether
 or not the optional argument is supplied"
 (list a b c c-supplied-p))

; Rest function parameter &rest
(defun test-rest-func-params (a &optional (b 2) &rest numbers)
 "Rest parameter catches all remaining arguments not consumed by required and
 optional parameters"
 (list a b :rest numbers))

; Keyword function parameters &key
(defun test-key-func-params (width height
                             &key ((:square s) (* width height) s-supplied-p))
 "Unlike optional parameters, keyword parameters is not positional"
 (list width height :keyword (list s s-supplied-p)))

; Function as argument, aka, high-order function
(defun plot (fn min max step)
 "Plot ASCII-art graph of the results of the function with arguments from min
 to max, with step as the increment step"
 (loop for i from min to max by step do
  (loop repeat (funcall fn i) do (format t "*"))
  (format t "~%")))
; Usage
(funcall #'plot #'exp 0 4 1/2)
(defvar plot-data (list #'exp 0 4 1/2))
(apply #'plot plot-data)

;; @file unit-testing-framework.cl
;;
;; @breif Simple unit testing framework to show the usage of macro
;;

; Original test codes
(defun test+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -2) -3) '(= (+ -1 -2) -3)))
; Factor the 'format ...' expression out
(defun report-result (result label)
  (format t "~:[FAIL~;pass~] ... ~a~%" result label))
; So now the 'test+' function is simplified a little
(defun test+1 ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -2) -3) '(= (+ -1 -2) -3)))
; To factor out the repeated contents in result and label, use macro
(defmacro check (form)
  `(report-result ,form ',form))
; Now the 'test' function is simplified further
(defun test+2 ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -2) -3)))
; Well, it seems natural to make check take an arbitrary number of forms
; @note It is a common macro idiom to wrap `progn` around a series of forms in
; order to turn them into a single form. Notice also how `,@` can be used to
; splice in the result of an expression that returns a list of expressions that
; are themselves generated with a backquote template.
(defmacro check1 (&body forms)
  `(progn
    ,@(loop for form in forms collect `(report-result ,form ',form))))
; And finally, the 'test+' function is as simple as follows:
(defun test+-final ()
  (check1
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -3)))
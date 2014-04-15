;; @file unit-testing-framework-impl.cl
;;
;; @breif [Impl.] Simple unit testing framework, the lib.
;;

; Common macro-writting macro that generates unique symbols for temporary
; variables used in the macro definition, to avoid name collision.
(defmacro with-gensyms ((&rest names) &body body)
  "Generate unique symbols for temporary variables used in macro definition, to
   avoid name collision in the body forms when defining macro."
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body))
; `and`-like macro without the short-circuiting trait, to combine multiple
; results
(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for form in forms collect `(unless ,form (setf ,result nil)))
      ,result)))

; Dynamic variable holding current test suites stack
(defvar *test-name* nil)

; Define new test case and test suite
; @note by default, `combine-results` is applied to `,@body`, to simplify the
; defintion of test suites that contains multiple test suites/cases
(defmacro deftest (test-name params &body body)
  "Define a test function. Within a test function we can call other test
   functions or use `check` to run individual test cases."
  `(defun ,test-name ,params
    (let ((*test-name* (append *test-name* (list ',test-name))))
      (combine-results ,@body))))

; Format output with label and test suite stack, and report result
; @note the result is returned to support further judge or combine
(defun report-result (result label)
  "Report the result of a single test case. Called by `check`."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* label)
  result)

; Perform one or more check(s), report and combine the results
(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for form in forms collect `(report-result ,form ',form))))

; Simple usage:
; test cases
(deftest test-+ ()
  "Test the add(`+`) operator."
  (check
    (= (+ 1 2) 3)
    (= (+ 0 2) 2)
    (= (+ -2 3) 1)))
(deftest test-* ()
  "Test the multiply(`*`) operator."
  (check
    (= (* 2 3) 6)
    (= (* 1 2) 2)
    (= (* 0 3) 0)
    (= (* -2 2) -4)))
(deftest test-/ ()
  "Test the divide(`/`) operator."
  (check
    (= (/ 3 1) 3)
    (= (/ 4 2) 2)
    (= (/ 4 -2) -2)))
; test suites
(deftest test-arithmetic-1 ()
  (test-+)
  (test-/))
(deftest test-arithmetic-2 ()
  (test-*)
  (test-/))
; test suites consisted with test suites
(deftest test-math ()
  (test-arithmetic-1)
  (test-arithmetic-2))
; run test
(test-math)
;; @file unit-testing-framework.cl
;;
;; @breif Simple unit testing framework to show the usage of macro
;;

; Original test codes
(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -2) -3) '(= (+ -1 -2) -3)))
; Factor the 'format ...' expression out
(defun report-result (result label)
  (format t "~:[FAIL~;pass~] ... ~a~%" result label))
; So now the 'test-+' function is simplified a little
(defun test-+-a ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -2) -3) '(= (+ -1 -2) -3)))
; To factor out the repeated contents in result and label, use macro
(defmacro check (form)
  `(report-result ,form ',form))
; Now the 'test' function is simplified further
(defun test-+-b ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -2) -3)))
; Well, it seems natural to make check take an arbitrary number of forms
; @note It is a common macro idiom to wrap `progn` around a series of forms in
; order to turn them into a single form. Notice also how `,@` can be used to
; splice in the result of an expression that returns a list of expressions that
; are themselves generated with a backquote template.
(defmacro check-a (&body forms)
  `(progn
    ,@(loop for form in forms collect `(report-result ,form ',form))))
; And finally, the 'test-+' function is as simple as follows:
(defun test-+-c ()
  (check-a
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -3)))
;
; Fix the return value
(defun report-result-a (result label)
  (format t "~:[FAIL~;pass~] ... ~a~%" result label)
  result)
; Common macro-writting macro that generate unique symbols for temporary
; variables used in the macro definition, to avoid name collision
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body)
)
; `and`-like macro without the short-circuiting trait
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for form in forms collect `(unless ,form (setf ,result nil)))
      ,result)))
; Replace `progn` with `combine-results` in the `check` macro
(defmacro check-b (&body forms)
  `(combine-results
    ,@(loop for form in forms collect `(report-result-a ,form ',form))))
; Now, the `test-+` is defined as follows:
(defun test-+-d ()
  (check-b
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -3)))
;
; More test cases
(defun test-* ()
  (check-b
    (= (* 2 2) 4)
    (= (* -3 5) -15)
    (= (* 1 3) 3)
    (= (* 0 5) 0)))
; Run all test cases together, that is, a test suite
(defun test-arithmetic ()
  (combine-results
    (test-+-d)
    (test-*)))
; As more and more test cases are added in, it would be nice to know where a
; failed test comes from, namely, the name of the function containing the test.
(defun report-result-b (result label test-name)
  (format t "~:[FAIL~;pass~] ... ~a:~a~%" result test-name label)
  result)
; However, to make this possible, name of the function has to be passed to the
; `check` macro, which in turn pass the name to the function `report-result`,
; and at last the name is printed. This is so inconvenient, are there any other
; solutions? And the answer is yes, 'dynamic variables' is best friend. Actually,
; this is exactly the kind of problem dynamic variables were designed to solve.
;
; First, declare the variable at the top level
(defvar *test-name* nil)
; Second, change `report-result` to use the dynamic variable
(defun report-result-c (result label)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* label)
  result)
; Redeclare other macros and functions
(defmacro check-c (&body forms)
  `(combine-results
    ,@(loop for form in forms collect `(report-result-c ,form ',form))))
; Remeber to binding '*test-name*' to the appropriate before run test
(defun test-+-e ()
  (let ((*test-name* 'test-+-e))
    (check-c
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -2) -3))))
;;;
(defun test-*-a ()
  (let ((*test-name* 'test-*-a))
    (check-c
      (= (* 2 2) 4)
      (= (* -3 5) -15)
      (= (* 1 3) 3)
      (= (* 0 5) 0))))
;;;
(defun test-arithmetic-a ()
  (combine-results
    (test-+-e)
    (test-*-a)))
;
; Well now, everything seems fairly good, except for the crummy duplication that
; every test has exactly the same three lines of codes that bind '*test-name*'
; to the function name of itself.
; This is because the abstraction of a test is only in the developer's mind, and
; to implement the abstraction, certain patterns has to be followed. To concrete
; the mind abstraction, `macro` is the best choice.
;
; The `deftest` macro expands to a `defun` declaration
(defmacro deftest (test-name params &body body)
  `(defun ,test-name ,params
    (let ((*test-name* ',test-name))
      ,@body)))
; With the `deftest` defined above, new test could be defined as follows:
(deftest test-+-f ()
  (check-c
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -3)))
(deftest test-*-b ()
  (check-c
    (= (* 2 2) 4)
    (= (* -3 5) -15)
    (= (* 1 3) 3)
    (= (* 0 5) 0)))
;;;
(defun test-arithmetic-b ()
  (combine-results
    (test-+-f)
    (test-*-b)))
;
; Test hierarchy
; It is not uncommon for a test to be included in two test suites(context), and
; it's also not unheard of when the test succeeded in one context while failed
; in another. In such situation, there is no way to tell in what context the
; test failed.
; So we need a way to reflect the test hierarchy in the output, for example:
;
; pass ... (TEST-ARITHMETIC TEST-+): (= (+ 1 2) 3)
;
; To implement, we'll play with '*test-name*', and redefine `deftest`.
; @note `combine-results` is applied to `,@body` for this implementation, to
; simplify test suites definition(no `combine-results` is needed in the body).
(defmacro deftest (test-name params &body body)
  `(defun ,test-name ,params
    (let ((*test-name* (append *test-name* (list ',test-name))))
      (combine-results ,@body))))
; Some test cases
(deftest test-+-g ()
  (check-c
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -3)))
(deftest test-*-c ()
  (check-c
    (= (* 2 2) 4)
    (= (* -3 5) -15)
    (= (* 1 3) 3)
    (= (* 0 5) 0)))
(deftest test-/-a ()
  (check-c
    (= (/ 2 1) 2)
    (= (/ 6 3) 2)))
; New test suites can now be defined with `deftest` macro as follows:
(deftest test-arithmetic-c ()
  (test-+-g)
  (test-*-c))
(deftest test-arithmetic-d ()
  (test-/-a)
  (test-*-c))
(deftest test-math ()
  (test-arithmetic-c)
  (test-arithmetic-d))
;
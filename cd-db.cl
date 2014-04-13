;; cd-db.cl
;; A simple database for CD records
;;

; construct a cd record
(defun make-cd (title artist rating ripped)
 (list :title title :artist artist :rating rating :ripped ripped)
)

; define a global var
(defvar *db* nil)

; add a cd record to db
(defun add-record (cd)
 (push cd *db*)
)

;; test content
;(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
;(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
;(add-record (make-cd "Home" "Dixie Chicks" 9 t))

; output the db in a more human-readable way
; equivlant one-line function is:
; (format t "~{~{~a:~10t~a~%~}~%~}" *db*)
(defun dump-db ()
 (dolist (cd *db*)
  (format t "~{~a:~10t~a~%~}~%" cd))
)

; prompt the user for an input
(defun prompt-read (prompt)
 (format *query-io* "~a: " prompt)
 (force-output *query-io*)
 (read-line *query-io*)
)

; prompt the user for a new record
(defun prompt-for-cd ()
 (make-cd
  (prompt-read "Title")
  (prompt-read "Artist")
  (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
  (y-or-n-p "Ripped [y/n]: "))
)

; prompt the user for a series of CDs
(defun prompt-for-cds ()
 (loop (add-record (prompt-for-cd))
  (if (not (y-or-n-p "Another? [y/n]: ")) (return)))
)

; save db to file
(defun save-db (filename)
 (with-open-file (out filename :direction :output :if-exists :supersede)
  (with-standard-io-syntax (print *db* out)))
)

; load db back from file
(defun load-db (filename)
 (with-open-file (in filename)
  (with-standard-io-syntax (setf *db* (read in))))
)

; select by artist
(defun select-by-artist (artist)
 (remove-if-not #'(lambda (cd) (equal artist (getf cd :artist))) *db*)
)

; general select with selector as argument
(defun select (selector-fn)
 (remove-if-not selector-fn *db*)
)

; artist selector
(defun artist-selector (artist)
 #'(lambda (cd) (equal artist (getf cd :artist)))
)

; title selector
(defun title-selector (title)
 #'(lambda (cd) (equal title (getf cd :title)))
)

; rating selector
(defun rating-selector (rating)
 #'(lambda (cd) (equal rating (getf cd :rating)))
)

; SQL-like where selector generator
(defun where (&key title (artist nil artist-p)
              (rating nil rating-p) (ripped nil ripped-p))
 #'(lambda (cd)
     (and
      (if title (equal (getf cd :title) title) t)
      (if artist-p (equal (getf cd :artist) artist) t)
      (if rating-p (equal (getf cd :rating) rating) t)
      (if ripped-p (equal (getf cd :ripped) ripped) t)))
)

; update existing record
(defun update (selector-fn
               &key title (artist nil artist-p)
               (rating nil rating-p) (ripped nil ripped-p))
 (setf *db*
  (mapcar
   #'(lambda (cd)
       (when (funcall selector-fn cd)
        (if title (setf (getf cd :title) title))
        (if artist-p (setf (getf cd :artist) artist))
        (if rating-p (setf (getf cd :rating) rating))
        (if ripped-p (setf (getf cd :ripped) ripped)))
       cd)
   *db*))
)

; delete
(defun delete-rows (selector-fn)
 (setf *db* (remove-if selector-fn *db*))
)

; remove duplication
(defun make-cmp-expr (field value)
 `(equal (getf cd ,field) ,value)
)

(defun make-cmp-list (clauses)
 (loop while clauses
  collecting (make-cmp-expr (pop clauses) (pop clauses)))
)

(defmacro where-1 (&rest clauses)
 `#'(lambda (cd) (and ,@(make-cmp-list clauses)))
)

;; @file spam.cl
;;
;; @breif Core functionalities of a spam filtering program.
;;
(in-package :com.gigamonkeys.spam)

; Given the content of a email, report whether it is a spam, a ham, or unsure.
(defun classify (text)
  (classification (score (extract-features text))))
;
(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classification (score)
  (cond
    ((>= score *min-spam-score*) 'spam)
    ((<= score *max-ham-score*) 'ham)
    (t 'unsure)))

; To represent a feature of word, with number of the times the word has
; appeared in spam/ham emails
(defclass word-feature ()
  ((word
    :initarg :word
    :initform (error "Must supply :word")
    :accessor word
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :initform 0
    :accessor spam-count
    :documentation "Number of spams this feature has been seen in.")
   (ham-count
    :initarg :ham-count
    :initform 0
    :accessor ham-count
    :documentation "Number of hams this feature has been seen in.")))

; Reference to the hash table storing all the featuress ever encountered
; @note `defvar` is used instead of `defparameter` to avoid reset when
; reloading
(defvar *feature-database* (make-hash-table :test #'equal))

; Because `defvar` is used, so provide a function to reset
(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

; Look the word up in the database, create a new feature with the word if not
; found. Return the existing or newly created feature.
(defun intern-feature (word)
  (or
    (gethash word *feature-database*)
    (setf (gethash word *feature-database*)
      (make-instance 'word-feature :word word))))

; Extract words with regular expression
(defun extract-words (text)
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
    :test #'string=))

; Combining 'extract-words' and 'intern-feature' with `mapcar` produces
; 'extract-features'.
(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

; Pretty print the 'word-feature' object
(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word spam-count ham-count) object
      (format stream "~a :hams ~d :spams ~d" word ham-count spam-count))))

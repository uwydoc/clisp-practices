;; @file generics-and-classes.cl
;;
;; @breif Generics and classes
;;

; For auto-incrementing account-number, used internally only.
(defvar *account-number* 0)
; User-defined class which subclasses `STANDARD-OBJECT`
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Customer name must be supplied.")
    :accessor customer-name
    :documentation "Name of the customer.")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current balance.")
   (account-number
    :initform (incf *account-number*)
    :reader account-number
    :documentation "Internal account number, unique in the bank.")
   (account-type
    :reader account-type
    :documentation "Account type, one of :glod :silver :bronze and :iron .")))
; Determine account type according to initial balance
; @note the use of '&key' to make 'opening-bonus-percentage' a valid keyword
; argument of the `MAKE-INSTANCE` function
; @note that the use of `:afte` auxiliary method to access existing slot
; values while creating the instance.
(defmethod initialize-instance :after ((account bank-account)
                                       &key opening-bonus-percentage)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
      (cond
        ((> balance 100000) :glod)
        ((> balance 50000) :silver)
        ((> balance 5000) :bronze)
        (t :iron)))
    (when opening-bonus-percentage
      (incf (slot-value account 'balance)
        (* balance (/ opening-bonus-percentage 100))))))
;;
; Slot inheritance and merge
; Basically, `:initarg`s are combined, others are set to the most specific
; options respectively. Besides, `:allocation` option could be changed while
; inheriting.
(defclass foo ()
  ((a :initarg :a :initform "A" :accessor a)
   (b :initarg :b :initform "B" :accessor b)))
(defclass bar (foo)
  ((a :initform (error "Must supply a value for a"))
   (b :initarg :the-b :accessor the-b :allocation :class)))
;;
; Multiple inheritance
; Saving account with interest
(defclass savings-account (bank-account)
  ((interest
    :initarg :interest
    :reader interest
    :documentation "Current accumulated interest.")))
; Give bonus interest on acount created
(defmethod initialize-instance :after ((account savings-account) &key)
  (setf (slot-value account 'interest) (* (balance account) 0.001)))
; Checking account with checking functionality and record checks number
(defclass checking-account (bank-account)
  ((checks
    :initform 0
    :reader checks
    :documentation "Number of total check operations.")))
; Check operation on checking-account, defined as generic
(defgeneric check (account)
  (:documentation "Perform a check operation"))
(defmethod check ((account checking-account))
  (format t "#~4d checking now" (incf (slot-value account 'checks))))
; Now is the interesting part
; money-market-account inherits both savings-account and checking-account
(defclass money-market-account (checking-account savings-account) ())
; Suppose a operation named 'print-statement' can be performed all kinds of
; bank-account, and specializations on savings-account and checking-account
; are different.
; As money-market-account inherits both savings-account and checking-account,
; it is natural that its specialization implements both specializations of its
; direct-superclasses.
; Well, frankly speaking, the solution is not obvious. To solve this problem,
; the trick of using auxiliary method could be applied as follows:
;
; Define generic function 'print-statement'
(defgeneric print-statement (account)
  (:documentation "Print (monthly) general statement of the account."))
; Base implementations in primary method of the common superclass
(defmethod print-statement ((account bank-account))
  (with-slots (customer-name balance account-type) account
    (format t "[bank-account]~15tcustomer-name: ~a~%~15tbalance: ~d~%~15taccount-type: ~a~%"
      customer-name balance account-type)))
; Detailed implementations in :after auxiliary method of the two subclasses
; @note choosing between :before and :after affects the order of
; implementations of in the class's precedence class list. With :before, the
; order is most-to-least, while with :after, the order is least-to-most.
;
; savings-account: print the current interest
(defmethod print-statement :after ((account savings-account))
  (with-slots (interest) account
    (format t "[savings-account]~18tinterest: ~d~%" interest)))
; checkings-account: print the number of total check operations
(defmethod print-statement :after ((account checking-account))
  (with-slots (checks) account
    (format t "[checking-account]~19tchecks: ~d~%" checks)))
; If needed, detailed implementations of money-market-account must also be
; defined as auxiliary method
(defmethod print-statement :after ((account money-market-account))
  (format t "[money-market-account]~%"))

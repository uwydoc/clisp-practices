;; @file toy-bank.cl
;; @breif Toy bank app to demonstrate basic usage of `defgeneric` and
;; `defmethod`
;;
; Assume there are three classes - bank-account, checking-account and
; savings-account.
(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount of money from the account.
Signal an error if current balance is less than the amount."))

; Method specialization
(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))
;
(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance accout))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))
; EQL specializer
(defmethod withdraw ((account (eql *account-of-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft))))
  (call-next-method))

;Да се напише функция (toDecimal n), която превръща дадено естествено число от двоична в десетична бройна система:

(define (toDecimal n)
  (if (= n 0) 0
      (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))))
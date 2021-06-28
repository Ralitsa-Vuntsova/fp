; Да се напише функция (toBinary n), която превръща дадено естествено число в двоична бройна система:

(define (toBinary n)
  (if (= n 0) 0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))
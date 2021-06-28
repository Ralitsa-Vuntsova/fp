; Търсим процедура, която проверява дали дадено число завършва на дадено друго.

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))
      ))

(define (without-first n)
  (remainder n (expt 10 (- (count-digits n) 1))))

(define (ends-with n m)
  (cond ((> m n) #f)
        ((= (remainder n m) 0) #t)
        (else (ends-with (without-first n) m))))
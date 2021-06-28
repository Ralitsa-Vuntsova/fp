; Да се напише функция (perfect? n), която проверява дали дадено естествено число е съвършено.
; Съвършени числа са тези, за които сумата на всичките им делители (без самото число) е равна на същото число.

(define (divisors-sum-iter n)
  (define (for sum i)
    (cond ((>= i n) sum)
          ((= (remainder n i) 0) (for (+ sum i) (+ i 1)))
          (else (for sum (+ i 1)))))
  (for 0 1))

(define (perfect? n)
  (if (= (divisors-sum-iter n) n)
  #t
  #f))
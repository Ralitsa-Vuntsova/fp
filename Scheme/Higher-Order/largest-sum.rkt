;а) Да се реализира функция sum-common-divisors, която намира сумата от общите делители на две естествени числа.
;б) Да се реализира функция greatest-sum, която намира най-голямата сума на общи делители на две различни числа в интервала [a; b],
;където a и b са параметри на функцията. Пример:
;(greatest-sum 21 34) ; -> 15 (за числата 24 и 32)

(define (divides? a b)
  (if (= (remainder a b) 0) #t #f))

(define (sum-common-divisors a b)
  (define (divisors-up-to k)
    (cond ((= k 0) 0)
          ((and (divides? a k) (divides? b k)) (+ k (divisors-up-to (- k 1))))
          (else (divisors-up-to (- k 1)))))
  (if (> a b)
      (divisors-up-to b)
      (divisors-up-to a))
  )

(define (1+ x) (+ 1 x))

(define (my-max a b)
  (if (> a b) a b))

(define (max-div a-fix a b)
  (if (> a b) 0
      (my-max (sum-common-divisors a-fix (+ a 1)) (max-div a-fix (+ a 1) b))))

(define (greatest-sum a b)
  (if (> a b)
      0
      (my-max (max-div a a b) (greatest-sum (+ a 1) b))))
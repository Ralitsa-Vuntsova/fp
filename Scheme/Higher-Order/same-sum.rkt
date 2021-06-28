;а) Да се реализира функция sum-digit-divisors, която намира сумата на тези от положителните цифри на дадено естествено число, които са му делители.
;б) Да се реализира функция same-sum, която намира броя на двойките числа (m, n), за които a ≤ m < n ≤ b и функцията sum-digit-divisors от предното подусловие връща
;един и същ резултат, където a и b са параметри на функцията. Пример:
;(same-sum 28 35) ; -> 2 (двойките са (28, 32) и (29, 34))

(define (divide? n k)
  (if (= (remainder n k) 0) #t #f))

(define (sum-digit-divisors n)
  (define (divisors-up-to k)
    (cond
      ((= k 0) 0)
      ((= (remainder k 10) 0) (divisors-up-to (quotient k 10)))
      ((divide? n (remainder k 10)) (+ (remainder k 10) (divisors-up-to (quotient k 10))))
      (else (divisors-up-to (quotient k 10)))))
  (divisors-up-to n))

(define (isEqual? a-fix a b)
  (and (<= a b)
       (or (= (sum-digit-divisors a-fix) (sum-digit-divisors (+ a 1)))
           (isEqual? a-fix (+ a 1) b)
           )))

(define (same-sum a b)
  (cond
    ((> a b) 0)
    ((isEqual? a a b) (+ 1 (same-sum (+ a 1) b)))
    (else (same-sum (+ a 1) b))      
  ))
; Съчинете процедура, която обръща цифрите на дадено число.
; Трябва да работи и за отрицателни числа. - iter функция-та

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))
      ))

(define (reverse-digits n)
  (if (= n 0)
      0
      (+ (* (remainder n 10) (expt 10 (- (count-digits n) 1))) (reverse-digits (quotient n 10)))
      )
  )

(define (reverse-digits-iter n)
  (define (for result i)
    (if (= i 0)
        result
        (for (+ (* result 10) (remainder i 10)) (quotient i 10))))
   (if (< n 0)
       (for 0 (- n))
       (for 0 n)))



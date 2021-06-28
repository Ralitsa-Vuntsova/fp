; Търсим процедура, която намира броя цифри на дадено число
; Трябва да работи и за отрицателни числа. - iter функция-та

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))
      ))

(define (count-digits-iter n)
  (define (for counter i)
    (if (< i 10)
        counter
        (for (+ counter 1) (quotient i 10))))
  (if (< n 0)
      (for 1 (- n))
      (for 1 n)))

;sum-digits
(define (sum-digits n)
  (if (< n 10)
      n
      (+ (remainder n 10) (sum-digits (quotient n 10)))))

(define (sum-digits-iter n)
  (define (for sum i)
    (if (= i 0)
        sum
        (for (+ sum (remainder i 10)) (quotient i 10)))
    )
  (if (< n 0)
      (for 0 (- n))
      (for 0 n)))
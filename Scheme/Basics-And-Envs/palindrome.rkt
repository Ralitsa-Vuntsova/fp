; Търсим процедура, която проверява дали едно число е палиндром.
; Трябва да работи и за отрицателни числа.

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

(define (palindrome? n)
  (if (= (reverse-digits n) n)
      #t
      #f))
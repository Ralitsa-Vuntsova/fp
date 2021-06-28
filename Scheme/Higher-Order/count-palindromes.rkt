; Да се напише процедура, която брои броя на палиндромите в интервала а b

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

(define (count predicate a b)
  (cond ((> a b) 0)
        ((predicate a) (+ 1 (count predicate (+ a 1) b)))
        (else (count predicate (+ a 1) b))))

(define (count-palindromes a b)
  (count palindrome? a b))
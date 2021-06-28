;Higherorder and lambda

;(define (accumulate-i init op a next b)
;  (define (loop result i)
;    (if (<= i b)
;        (loop (op result i) (next i))
;        result))
;  (loop init a))

;(define (accumulate-r init op a next b)
;  (define (loop i)
;    (if (<= i b)
;        (op i (loop (next i)))
;        init))
;  (loop a))

(define (accumulate-i op term init a next b)
  (define (loop result i)
    (if (<= i b)
        (loop (op result (term i)) (next i))
        result))
  (loop init a))

(define (accumulate-r op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a))

(define (id x) x)
(define (1+ x) (+ 1 x))

; sum from a to b
(define (sum a b)
  (accumulate-r + id 0 a 1+ b))

; x^a + x^(a+1) + ... + x^b
(define (sum-row x a b)
  (define (term t) (expt x t))
  (accumulate-r + term 0 a 1+ b))

; sum from a to b even
(define (2+ x) (+ 2 x))

(define (sum-even a b)
  (accumulate-r
   +
   id
   0
   (if (even? a) a (+ a 1))
   2+
   b))

; sum of primes from a to b
(define (divides? k n)
  (= (remainder n k) 0))

(define (count-divisors n)
  (define (count-divisors-up-to k)
    (cond ((= k 0) 0)
          ((divides? k n)
           (+ 1 (count-divisors-up-to (- k 1))))
          (else (count-divisors-up-to (- k 1)))))
  (count-divisors-up-to n))

(define (prime? n)
  (= (count-divisors n) 2))

(define (next-prime x)
  (if (prime? (+ x 1))
              (+ x 1)
              (next-prime (+ x 1))))

;(define (sum-primes a b)
;  (accumulate-r
;   +
;   id
;   0
;   (if (prime? a) a (next-prime a))
;   next-prime
;   b))

(define (filter-accumulate p? op term init a next b)
  (define (loop i)
    (cond ((> i b) init)
          ((p? i) (op (term i) (loop (next i))))
          (else (loop (next i)))
        ))
  (loop a))

(define (sum-primes a b)
  (filter-accumulater prime? + id 0 a 1+ b))

(define (fixed-point? f x)
  (if (= (f x) x) #t #f))

;(define (derive f x dx)
;  (/ (- (f (+ x dx))
;        (f x))
;     dx)

(define (derive f dx)
  (lambda (x)
    (/ (- (f (+ x dx))
        (f x))
     dx)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 0) (lambda (x) x))
        ((= n 1) f)
        (else (compose f (repeated f (- n 1))))
        ))

;derive s preciznost
(define dx 0.0000001)

(define (simple-derive f)
  (derive f dx))

;deriveN
(define (deriveN f n)
  ((repeated simple-derive n) f))

(define (count predicate a b)
  (cond ((> a b) 0)
        ((predicate a) (+ 1 (count predicate (+ a 1) b)))
        (else (count predicate (+ a 1) b))))

(define (double f)
  (lambda (x) (f (f x))))
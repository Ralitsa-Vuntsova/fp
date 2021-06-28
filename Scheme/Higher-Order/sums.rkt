(define (accumulate-r op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a))

(define (id x) x)
(define (1+ x) (+ 1 x))
(define (square x) (* x x))

(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))

; sum from i = 0 to n (x^i/i!)
(define (sum1 x n)
  (define (term i) (/ (expt x i) (fact i)))
  (accumulate-r + term 0 0 1+ n))

; sum from i = k to 100 (k^2)
(define (sum2 k)
  (accumulate-r + square 0 k 1+ 100))

; sum from a to b dx*((f(a) + f(a +dx) + f(a + 2dx) + ... + f(b)))
(define (sum3 a b f dx)
  (define (term i) (* dx (f i)))
  (define (next i) (+ i dx))
  (accumulate-r + term 0 a next b))

; sum from x to 10^1000 (i, where next i = e^i)
(define (sum4 x)
  (accumulate-r + id 0 x exp (expt 10 1000)))

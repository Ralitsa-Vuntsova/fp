;Basics and envs

(define (sum a b)
  (+ a b))

(define (mult a b)
  (* a b))

(define (my-abs x)
  (if (>= x 0)
      x
      (- x)))

(define (my-even? x)
  (if (= (remainder x 2) 0) #t #f))

(define (my-odd? x)
  (if (= (remainder x 2) 0) #f #t))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fact-iter n)
  (define (for result i)
    (if (> i n)
        result
        (for (* result i) (+ 1 i)))
    )
  (for 1 1))

(define (fib n)
  (cond ((= n 1) 1)
        ((= n 2) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib-iter n)
  (define (for n-1 n-2 current)
    (if (> current n)
        n-2
        (for n-2 (+ n-1 n-2) (+ current 1))))
  (for 1 1 3))

(define (pow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (pow x (- n))))
        (else (* x (pow x (- n 1))))))

(define (pow-iter x n)
  (define (for result i)
    (if (> i n)
        result
        (for (* result x) (+ i 1))))
  (if (< n 0)
      (/ 1 (pow-iter x (- n)))
      (for 1 1)))

(define (square x)
  (* x x))

(define (fast-pow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (fast-pow x (- n))))
        ((even? n) (square (fast-pow x (quotient n 2))))
        (else (* x (fast-pow x (- n 1))))))

(define (fast-pow-iter x n)
  (define (for result x i)
    (cond ((= i 0) result)
          ((even? i) (for result (square x) (/ i 2)))
          (else (for (* result x) x (- i 1)))))
    (if (< n 0)
        (/ 1 (fast-pow-iter x (- n)))
        (for 1 x n)))

(define (divisors-count n)
  (define (divisors-up-to k)
    (cond ((= k 0) 0)
          ((= (remainder n k) 0) (+ 1 (divisors-up-to (- k 1))))
          (else (divisors-up-to (- k 1)))))
  (divisors-up-to n))

(define (divisors-count-iter n)
  (define (for counter i)
    (cond ((> i n) counter)
          ((= (remainder n i) 0) (for (+ counter 1) (+ i 1)))
          (else (for counter (+ i 1)))))
  (for 0 1))

(define (divisors-sum n)
  (define (divisors-up-to k)
    (cond ((= k 0) 0)
          ((= (remainder n k) 0) (+ k (divisors-up-to (- k 1))))
          (else (divisors-up-to (- k 1)))))
  (divisors-up-to n))

(define (divisors-sum-iter n)
  (define (for sum i)
    (cond ((> i n) sum)
          ((= (remainder n i) 0) (for (+ sum i) (+ i 1)))
          (else (for sum (+ i 1)))))
  (for 0 1))

(define (month m)
  (cond ((equal? m "January") 1)
        ((equal? m "February") 2)
        ((equal? m "March") 3)
        ((equal? m "April") 4)
        ((equal? m "May") 5)
        ((equal? m "June") 6)
        ((equal? m "July") 7)
        ((equal? m "August") 8)
        ((equal? m "September") 9)
        ((equal? m "October") 10)
        ((equal? m "November") 11)
        ((equal? m "December") 12)
        (else "Wrong input")))

; 3x^2-2x-1 s koreni -1/3 i 1
(define (is-root? x)
  (zero? (- (- (* 3 (* x x)) (* 2 x)) 1)))

(define (divisible? n y)
  (if (= (remainder n y) 0)
      #t
      #f))

(define (leap? y)
  (and (divisible? y 4)
       (or (not (divisible? y 100))
           (divisible? y 400)
           )))

(define (days-in-month m y)
  (case m
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (else (if (leap? y) 29 28))))

(define (dist x1 y1 x2 y2)
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (area x1 y1 x2 y2 x3 y3)
  (let* ((a (dist x1 y1 x2 y2))
         (b (dist x2 y2 x3 y3))
         (c (dist x1 y1 x3 y3))
         (p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))

; f(n) = n, n < 3; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3)
(define (f n)
  (if (< n 3) n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f-iter n)
  (define (for r i)
    (if (> i 3)
        r
        (for (+ r (* i (f (- n i)))) (+ i 1))))
  (if (< n 3)
      n
      (for 0 1)))

;    1
;   1 1
;  1 2 1
; 1 3 3 1
;1 4 6 4 1
(define (binomial-coefficient row index)
  (if (or (= index 1) (= index row))
      1
      (+ (binomial-coefficient (- row 1) (- index 1))
         (binomial-coefficient (- row 1) index))))

; Едно число е просто, ако има точно 2 делителя.
(define (prime? n)
  (= (divisors-count n) 2))

; Числото n е просто, ако не е 1 и няма нито един делител в интервала [2, n - 1].
(define (divides? x y)
  (if (= (remainder x y) 0)
      #t
      #f))

(define (prime?-iter n)
  (define (iter from to)
    (cond ((> from to) #t)
          ((divides? n from) #f)
          (else (iter (+ from 1) to))))
  (and (not (= n 1))
       (iter 2 (- n 1))))

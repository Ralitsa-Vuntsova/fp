(load "./basics.rkt")

;Дефинирайте процедура (prime-in-each-column? matrix), която проверява дали във всяка колона в матрицата matrix има просто число.

(define (divides? x y)
  (if (= (remainder x y) 0)
      #t
      #f))

(define (prime? n)
  (define (iter from to)
    (cond ((> from to) #t)
          ((divides? n from) #f)
          (else (iter (+ from 1) to))))
  (and (not (= n 1))
       (iter 2 (- n 1))))

(define (every? p l)
  (or (null? l)
      (and (p (car l))
           (every? p (cdr l)))))

(define (any? p l)
  (and (not (null? l))
       (or
        (p (car l))
        (any? p (cdr l)))))

(define (for-all-columns? p matrix)
  (every? p (transpose matrix)))

(define (prime-in-each-column? matrix)
  
  (define (prime-exists? l)
    (any? prime? l))

  (for-all-columns? prime-exists? matrix))



(load "./basics.rkt")

(define (count-leaves tree)
  (cond
    ((null? tree) 0)
    ((leaf? tree) 1)
    (else (+ (count-leaves (cadr tree))
             (count-leaves (caddr tree))))
  ))
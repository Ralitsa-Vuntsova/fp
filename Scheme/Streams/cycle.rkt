(load "./basics.rkt")

; Дефинирайте процедура (cycle l), която връща безкраен поток, който генерира елементите на списъка l.

(define (cycle l)
  (if (null? l)
      empty-stream
      (cons-stream (car l)
                   (cycle (append (cdr l)
                                  (list (car l)))))))
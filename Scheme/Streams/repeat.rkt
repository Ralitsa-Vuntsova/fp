(load "./basics.rkt")

; Дефинирайте процедура (repeat value), която връща безкраен поток, който генерира стойности value.

(define (repeat value)
  (cons-stream value (repeat value)))
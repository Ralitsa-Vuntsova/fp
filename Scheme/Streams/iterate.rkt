(load "./basics.rkt")

; Дефинирайте процедура (iterate f x), която връща безкрайния поток x, f(x), f(f(x)), f(f(f(x))), ....

(define (iterate f x)
  (cons-stream x (iterate f (f x))))



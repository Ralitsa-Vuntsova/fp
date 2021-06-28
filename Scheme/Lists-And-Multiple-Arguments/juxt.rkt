;Дефинирайте процедура (juxt . fns).
;Резултатът от (juxt f g h ...) e процедура с променлив брой аргументи,
;която връща списък с резултатите от прилагането на всяка една от процедурите f, g, h, ... върху тези аргументи.

(define (juxt . fns)
  (lambda args
    (map (lambda (fn)
           (apply fn args))
         fns)))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (double x) (* 2 x))
(define (square x) (* x x))

((juxt) 5)
((juxt list) 1 2 3)
((juxt inc dec double square) 5)
((juxt + *) 3 4 5)
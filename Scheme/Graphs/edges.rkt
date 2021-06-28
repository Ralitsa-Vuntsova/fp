(load "./basics.rkt")

; Дефинирайте процедура (edges g), която връща списък с всички ребра на ориентирания граф g.

(define (flatmap f l)
  (apply append (map f l)))

(define (edges g)
  (flatmap (lambda (u)
             (map-children u
                           (lambda (v) (cons u v))
                           g))
           (vertices g)))
(load "./basics.rkt")
(load "./edges.rkt")

; Дефинирайте процедура (invert g), която инвертира ориентирания граф g, тоест връща нов граф, който има ребра от вида (v, u) за всяко ребро (u, v) от g.

(define (foldl op init lst)
  (if (null? lst) init
      (foldl op (op init (car lst)) (cdr lst))
      ))

(define (invert g)
  (foldl (lambda (inverted edge)
           (add-edge (cdr edge) (car edge) inverted))
         (make-graph (vertices g))
         (edges g)))
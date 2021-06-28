(load "./basics.rkt")

(define (height tree)
  (if (empty-tree? tree)
      0
      (+ 1
         (max (height (left-tree tree))
              (height (right-tree tree))))))

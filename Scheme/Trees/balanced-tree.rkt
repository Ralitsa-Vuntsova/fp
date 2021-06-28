(load "./basics.rkt")

(define (height tree)
  (if (empty-tree? tree)
      0
      (+ 1
         (max (height (left-tree tree))
              (height (right-tree tree))))))

(define (balanced? tree)
  (or (empty-tree? tree)
      (and (< (abs (- (height (left-tree tree))
                      (height (right-tree tree))))
              2)
           (balanced? (left-tree tree))
           (balanced? (right-tree tree)))))
(load "./basics.rkt")

(define (map-tree fn tree)
  (if (empty-tree? tree)
      empty-tree
      (make-tree (fn (root-tree tree))
                 (map-tree fn (left-tree tree))
                 (map-tree fn (right-tree tree)))))
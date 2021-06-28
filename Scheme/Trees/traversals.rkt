(load "./basics.rkt")

(define (pre-order tree)
  (if (empty-tree? tree)
      empty-tree
      (cons (root-tree tree)
            (append (pre-order (left-tree tree))
                    (pre-order (right-tree tree))))))

(define (in-order tree)
  (if (empty-tree? tree)
      empty-tree
      (append (in-order (left-tree tree))
              (list (root-tree tree))
              (in-order (right-tree tree)))))

(define (post-order tree)
  (if (empty-tree? tree)
      empty-tree
      (append (post-order (left-tree tree))
              (post-order (right-tree tree))
              (list (root-tree tree)))))
(load "./basics.rkt")

(define (sum-leaves-tree tree)
  (cond
    ((empty-tree? tree) 0)
    ((leaf? tree) (root-tree tree))
    (else (+ (sum-leaves-tree (left-tree tree))
             (sum-leaves-tree (right-tree tree))))))

(define (sum-tree tree)
  (cond
    ((empty-tree? tree) 0)
    (else (+
           (root-tree tree)
           (sum-tree (left-tree tree))
           (sum-tree (right-tree tree))))))


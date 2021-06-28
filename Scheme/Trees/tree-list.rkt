(load "./basics.rkt")

(define (tree->list tree)
  (cond
    ((empty-tree? tree) '())
    (else (append (list (root-tree tree))
                  (tree->list (left-tree tree))
                  (tree->list (right-tree tree))))))

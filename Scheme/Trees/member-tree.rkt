(load "./basics.rkt")

(define (member-tree? x tree)
  (cond
    ((empty-tree? tree) #f)
    ((equal? x (root-tree tree)) #t)
    (else (or (member-tree? x (left-tree tree))
              (member-tree? x (right-tree tree))))))
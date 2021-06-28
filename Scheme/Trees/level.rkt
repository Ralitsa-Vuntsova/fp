(load "./basics.rkt")

(define (level n tree)
  (cond
    ((empty-tree? tree) empty-tree)
    ((= n 0) (list (root-tree tree)))
    (else (append (level (- n 1) (left-tree tree))
                  (level (- n 1) (right-tree tree))))))

(define (height tree)
  (if (empty-tree? tree)
      0
      (+ 1
         (max (height (left-tree tree))
              (height (right-tree tree))))))

(define (level-order tree)
  (define h (height tree))
  (define (helper i n)
    (if (> i n)
        '()
        (append (level i tree)
                (helper (+ i 1) n))))
  (helper 0 (- h 1))
  )
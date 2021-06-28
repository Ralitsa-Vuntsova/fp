(load "./tree.rkt")

(define (balanced? tree)
  (define (height t)
    (if (or (null? t) (null? (car t)) (null? (cdr t)))
        0
        (+ 1 (max (height (cadr t))
                  (height (caddr t))))
        )
  )
  (if (null? tree)
      #t
      (if (and (<= (abs (- (height (cadr tree)) (height (caddr tree)))) 1)
               (balanced? (cadr tree))
               (balanced? (caddr tree)))
          #t
          #f))
  )
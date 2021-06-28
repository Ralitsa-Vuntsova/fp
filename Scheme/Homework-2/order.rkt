(load "./tree.rkt")

(define (order? tree)
  (define (helper tr left right)
    (cond
      ((null? tr) #t)
      ((and (not (null? left)) (<= (car tr) (car left))) #f)
      ((and (not (null? right)) (>= (car tr) (car right))) #f)
      (else (and
             (helper (cadr tr) left tr)
             (helper (caddr tr) tr right)))
    ))
  (helper tree '() '())
  )

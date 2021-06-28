(load "./basics.rkt")

;Дефинирайте процедура (reverse-columns matrix), която обръща реда на колоните в матрицата matrix.

(define (reverse-columns m)
    (if (null? m)
        '()
        (cons (reverse (car m)) (reverse-columns (cdr m)))
    )
  )
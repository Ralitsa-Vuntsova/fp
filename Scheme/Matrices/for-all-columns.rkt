(load "./basics.rkt")

;Дефинирайте процедура (for-all-columns? p matrix), която проверява дали за всяка колона в матрицата matrix е изпълнен предикатът p.

(define (every? p l)
  (or (null? l)
      (and (p (car l))
           (every? p (cdr l)))))

(define (for-all-columns? p matrix)
  (every? p (transpose matrix)))

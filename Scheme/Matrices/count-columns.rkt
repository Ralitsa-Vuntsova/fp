(load "./basics.rkt")

;Дефинирайте процедура (count-columns matrix), която намира броя на колоните на матрицата matrix,
;за които е вярно, че всичките им елементи се срещат в някой от редовете на матрицата matrix.

(define (every? p l)
  (or (null? l)
      (and (p (car l))
           (every? p (cdr l)))))

(define (any? p l)
  (and (not (null? l))
       (or (p (car l))
           (any? p (cdr l)))))

(define (filter p l)
  (cond
    ((null? l) '())
    ((p (car l)) (cons (car l) (filter p (cdr l))))
    (else (filter p (cdr l)))))

(define (count-columns matrix)
  
  (define (subset? column row)
    (every? (lambda (x)
              (member x row))
            column))
  
  (define (subset-of-row? column)
    (any? (lambda (row)
            (subset? column row))
          matrix))

  (length (filter subset-of-row?
                  (transpose matrix))))
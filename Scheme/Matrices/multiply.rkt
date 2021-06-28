(load "./basics.rkt")

;Дефинирайте процедура (multiply a b), която връща произведението на двете матрици a и b.

(define (multiply-vectors u v)
  (apply + (map * u v)))

(define (multiply a b)
  (let ((b-transpose (transpose b)))
    (map (lambda (row)
           (map (lambda (column)
                  (multiply-vectors row column))
                b-transpose))
         a)))
; Съчинете процедура, която по дадени три числа, намира сумата от квадратите на по-големите две от тях.
; За по-удобно, може да разбиете задачата на по-малки такива.

(define (sum a b)
  (+ a b))

(define (square a)
  (* a a))

(define (sum-of-squares a b c)
  (cond ((and (> a c) (> b c)) (sum (square a) (square b)))
        ((and (> a b) (> c b)) (sum (square a) (square c)))
        (else (sum (square c) (square b)))))
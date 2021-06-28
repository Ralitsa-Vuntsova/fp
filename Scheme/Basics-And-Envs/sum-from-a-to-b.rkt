; Съчинете процедура, която намира сумата на числата в даден затворен интервал.

(define (sum-from-a-to-b a b)
  (if (> a b)
      0
      (+ a (sum-from-a-to-b (+ a 1) b))
      ))

(define (sum-from-a-to-b-iter a b)
  (define (for result i)
    (if (> i b)
        result
        (for (+ result i) (+ i 1))))
  (for 0 1))
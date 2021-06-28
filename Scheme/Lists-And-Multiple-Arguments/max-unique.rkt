;(max-unique lst) - по списък от списъци от цели числа намира най-голямото от тези от тях,
;които са уникални в рамките на списъка, в който се срещат.
;Ако в никой списък няма уникални числа, функцията да връща #f

(define (count-occurrencies x l)
  (length (filter (lambda (y)
                    (equal? y x))
                  l)))

(define (unique? x lst)
  (if (= (count-occurrencies x lst) 1) #t #f))

(define (filter p? l)
  (cond
    ((null? l) l)
    ((p? (car l)) (cons (car l) (filter p? (cdr l))))
    (else (filter p? (cdr l)))))

; premahva neunikalnite i posle absolutni vsichki gi slaga v edin spisuk
(define (flat-unique lst)
    (apply
      append
      (map (lambda (l) (filter (lambda (x) (unique? x l)) l)) lst)))

(define (max-unique lst)
  (if (null? (flat-unique lst))
       #f
       (apply max (flat-unique lst))))
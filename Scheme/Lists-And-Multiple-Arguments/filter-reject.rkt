;Да се дефинира процедура (filter p? lst), която връща списък с елементите на списъка l, които удовлетворяват предиката p.
;Да се дефинира процедура (reject p? lst), която връща списък с елементите на списъка l, които не удовлетворяват предиката p.

(define (filter p? lst)
  (cond
      ((null? lst) '())
      ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
      (else (filter p? (cdr lst)))
      ))

(define (reject p? lst)
  (cond
      ((null? lst) '())
      ((not (p? (car lst))) (cons (car lst) (reject p? (cdr lst))))
      (else (reject p? (cdr lst)))
      ))

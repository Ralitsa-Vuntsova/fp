; Да се дефинира процедура (scale lst x), която връща списък с елементите на списъка l, умножени по числото x.

(define (scale lst x)
  (if (null? lst) '()
      (cons (* (car lst) x) (scale (cdr lst) x))))


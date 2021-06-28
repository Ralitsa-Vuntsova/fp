; Да се дефинира процедура (sum lst), която намира сумата на елементите на списъка l.

(define (sum lst)
  (if (null? lst) 0
      (+ (car lst) (sum (cdr lst)))))
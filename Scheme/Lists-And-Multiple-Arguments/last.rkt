; Да се дефинира процедура (last lst), която връща последния елемент на списъка l.

(define (last lst)
  (if (null? (cdr lst)) (car lst)
      (last (cdr lst))
  ))
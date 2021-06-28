; Да се дефинира процедура (add-last lst x), коята добавя елемент x на края на списъка l.

(define (add-last lst x)
  (if (null? lst) (list x)
      (cons (car lst) (add-last (cdr lst) x))
  ))
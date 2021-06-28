; Да се дефинира процедура (reverse-list lst), която връща списък, чиито елементи са елементите на списъка l в обратен ред.

(define (reverse-list lst)
  (if (null? lst) '()
      (append (reverse-list (cdr lst)) (list (car lst)))))
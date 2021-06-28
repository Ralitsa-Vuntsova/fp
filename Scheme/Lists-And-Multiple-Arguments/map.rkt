; Да се дефинира процедура (map-list f lst), която прилага едноаргументната процедура f върху всеки елемент на списъка l.

(define (map-list f lst)
  (if (null? lst) '()
  (cons (f (car lst)) (map-list f (cdr lst)))))
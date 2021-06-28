;Да се напише функция (zipWith* f . lsts), която работи аналогично на zipWith, но с произволен брой списъци като аргументи:
;Упътване: можете да подавате аргументи на такъв тип функции с apply:
;(apply + '(2 3 5))

(define (any? p? lst)
  (and (not (null? lst))
       (or (p? (car lst)) (any? p? (cdr lst)))))

(define (zipWith* f . lsts)
  (if (or (null? lsts)
          (any? null? lsts))
      '()
      (cons (apply f (map car lsts))
            (apply zipWith* f (map cdr lsts)))))

(zipWith* list '(1 2 3) '(a b) '(7 8 9 10)) ; -> '((1 a 7) (2 b 8))
(zipWith* + '(1 2 3)) ; -> '(1 2 3) ; все пак броят списъци е произволен

;Отворен числов интервал (a;b) се описва с наредената двойка (a . b).
;Да се напише функция longest-interval-subsets, която по даден списък от интервали il връща нов списък,
;който съдържа всички интервали от il, които са подинтервали на най-дългия интервал в списъка.
;Бонус: Функцията longest-interval-subsets да връща подинтервалите подредени в нарастващ ред по началната си точка.

; дали i1 е подинтервал на i2
(define (sub? i1 i2)
  (and (>= (car i1) (car i2))
       (<= (cdr i1) (cdr i2))))

; дължина на интервал
(define (int-length i)
  (- (cdr i) (car i)))

; сравнение за интервали - връща по-големия интервал
(define (>-int i1 i2)
  (if (> (int-length i1) (int-length i2)) i1 i2))

; максимален по дължина интервал
(define (max-interval lst)
  ; При търсене на мин/макс взимаме първия елемент
  ; като първоначален и обхождаме останалите.
  (foldr >-int (car lst) (cdr lst)))

(define (filter p? lst)
  (cond
    ((null? lst) '())
    ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
    (else (filter p? (cdr lst)))))

(define (longest-interval-subsets lst)
  (define longest (max-interval lst))
  (filter (lambda (i) (sub? i longest)) lst))
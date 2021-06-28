;Да се напише функция (zip lst1 lst2), която приема два списъка и връща списък от наредени двойки от техните съответни елементи:
;(zip '(1 2 3 4) '(#t #f #f)) -> '((1 . #t) (2 . #f) (3 . #f))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons
       (cons (car lst1) (car lst2))
       (zip (cdr lst1) (cdr lst2)))))

;Да се напише функция (zipWith f lst1 lst2), която връща списъка, получен от прилагането на f върху съответните елементи на двата списъка lst1 и lst2.
;(zipWith + '(1 2 3 4) '(7 10 12)) -> '(8 12 15)

(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons
       (f (car lst1) (car lst2))
       (zipWith f (cdr lst1) (cdr lst2)))))
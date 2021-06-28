;Да се напише функция (insert val lst), която вмъква стойността val на правилното място в сортирания в ненамаляващ ред списък lst:
;(insert 5 '(1 4 10)) -> '(1 4 5 10)
;(insert 12 '(1 4 10)) -> '(1 4 10 12)

(define (insert val lst)
  (cond
    ((null? lst) (list val))
    ((< val (car lst)) (cons val lst))
    (else (cons (car lst) (insert val (cdr lst))))
    ))
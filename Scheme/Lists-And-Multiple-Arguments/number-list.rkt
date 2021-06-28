; Да се напише процедура, която преобразува число в списък
;(number->list 12345) -> '(1 2 3 4 5)

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (number->list n)
  (define (helper i)
    (if (= (count-digits i) 1)
        (list i)
        (cons (remainder i 10) (helper (quotient i 10)))))
 (reverse (helper n)))

; Да се напише процедура, която преобразува списък в число
;(list->number '(1 2 3 4 5)) -> 12345

(define (list->number lst)
  (define (helper l len)
    (if (= len 0)
        0
        (+ (* (car l) (expt 10 (- len 1))) (helper (cdr l) (- len 1))) 
    ))
  (helper lst (length lst)))
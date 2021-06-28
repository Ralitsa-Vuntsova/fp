;Да се напише функция (uniques lst), която оставя само уникалните стойности в даден списък. Можете да проверявате за еднаквост с equal? за най-сигурно.
;(uniques '(1 2 2 "iei" 1 3 "iei" 'oops)) -> '(1 2 "iei" 3 'oops) ; подредбата в резултата няма значение

; Решение с member
(define (uniques lst)
  (if (null? lst) '()
      (let ((rest (uniques (cdr lst)))) ; да не преизчисляваме нещо повече от веднъж
        (if (member (car lst) (cdr lst))
            rest
            (cons (car lst) rest)))))

; Решение с filter
(define (uniques* lst)
  (if (null? lst) '()
      (cons (car lst)
            (uniques* (filter (lambda (x) (not (equal? x (car lst)))) (cdr lst))))))

; Итеративно решение
(define (uniques** lst)
  (define (helper lst res)
    (cond ((null? lst) res)
          ((member (car lst) res) (helper (cdr lst) res))
          (else (helper (cdr lst) (cons (car lst) res)))))
  (helper lst '()))
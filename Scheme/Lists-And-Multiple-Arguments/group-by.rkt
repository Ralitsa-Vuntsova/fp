;Да се напише функция (group-by f lst), която групира елементите на списъка lst по стойността, която f връща за тях:

(define (uniques lst)
  (if (null? lst) '()
      (let ((rest (uniques (cdr lst))))
        (if (member (car lst) (cdr lst))
            rest
            (cons (car lst) rest)))))

(define (get-results-list el f lst result)
  (cond
    ((null? lst) result)
    ((equal? el (f (car lst))) (get-results-list el
                                                 f
                                                 (cdr lst)
                                                 (cons (car lst) result)))
    (else (get-results-list el
                            f
                            (cdr lst)
                            result))))

(define (group-by f lst)
  (map (lambda (x) (list x (get-results-list x f lst '())))
       (uniques (map f lst))
  ))

(group-by even? '(1 2 3 4 5)); -> ((#f (1 3 5)) (#t (2 4))) ; подредбата няма значение
(group-by length '((1 2 3) (4) (5 6 7))); -> '((1 ((4)))(3 ((1 2 3) (5 6 7))))

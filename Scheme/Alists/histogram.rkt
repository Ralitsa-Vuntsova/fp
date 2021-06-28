(load "./basics.rkt")

; Да се дефинира процедура (histogram l), която връща хистограма на срещанията на всички елементи в l под формата на асоциативен списък.

(define (count-occurrencies x l)
  (length (filter (lambda (y)
                    (equal? y x))
                  l)))

(define (histogram l)
  (if (null? l)
      '()
      (cons (cons (car l)
                  (count-occurrencies (car l) l))
            (histogram (filter (lambda (y)
                                 (not (equal? y (car l))))
                               l)))))
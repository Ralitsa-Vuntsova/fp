; chunk - разбива списъка lst на подсписъци с дължина n

(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

(define (chunk n lst)
  (if (null? lst) '()
      (cons (take n lst) (chunk n (drop n lst))))
)
; Да се напише функция, която премахва n-тия елемент от списък. Индексирането е от 0.

(define (remove n lst)
  (cond
    ((null? lst) lst)
    ((= n 0) (cdr lst))
    (else (cons (car lst)
                (remove (- n 1) (cdr lst))))))
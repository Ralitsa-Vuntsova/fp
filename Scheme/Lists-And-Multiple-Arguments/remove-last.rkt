; Да се напише функция, която премахва последния елемент от списък.

(define (remove-last lst)
  (if (null? (cdr lst)) '()
      (cons (car lst) (remove-last (cdr lst)))))


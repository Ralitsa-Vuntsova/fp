; Да се напише функция, която проверява дали дадено число е елемент на списък.

(define (member elem lst)
  (and (not (null? list))
       (or (equal? (car lst) elem)
           (member elem (cdr lst)))))
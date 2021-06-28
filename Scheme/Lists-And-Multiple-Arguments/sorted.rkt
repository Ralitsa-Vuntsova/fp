;Да се напише функция (sorted? lst), която проверява дали списък е сортиран в ненамаляващ ред.
; 1 2 3 4 5

(define (sorted? lst)
  (if (null? lst) #t
      (if (null? (cdr lst)) #t
          (if (> (car lst) (cadr lst))
              #f
              (sorted? (cdr lst))
      ))))
;Да се напишат функциите (all? p? lst) и (any? p? lst), които проверяват съответно дали всички или някои елементи на даден списък изпълняват предиката p?:

(define (all? p? lst)
  (or (null? lst)
      (and (p? (car lst)) (all? p? (cdr lst)))))

(define (any? p? lst)
  (and (not (null? lst))
       (or (p? (car lst)) (any p? (cdr lst)))))
; Да се напише процедура foldr - дясно натрупване за списъци

(define (foldr op init lst)
  (if (null? lst) init
      (op (car lst) (foldr op init (cdr lst)))
      ))

(define (foldl op init lst)
  (if (null? lst) init
      (foldl op (op init (car lst)) (cdr lst))
      ))
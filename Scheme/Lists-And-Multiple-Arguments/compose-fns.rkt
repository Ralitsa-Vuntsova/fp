;Да се напише функция (compose . fns), която приема произволен брой функции като аргументи и връща тяхната композиция:
(define (sq x) (* x x))
(define (1+ x) (+ x 1))

(define (foldr op init lst)
  (if (null? lst)
      init
      (op  (car lst) (foldr op init (cdr lst)))
      ))

(define (compose . fns)
  (define (single-compose f1 f2) (lambda (x) (f1 (f2 x))))
  (foldr single-compose (lambda (x) x) fns) ;прилагаме от дясно на ляво
  )

(define f (compose sq 1+ (lambda (x) (* x 2)) 1+))
(f 5)
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (deep-foldr init term op dl)
  (cond ((null? dl) init)
        ((atom? dl) (term dl))
        (else (op (deep-foldr init term op (car dl))
                  (deep-foldr init term op (cdr dl))))))

(define (snoc x l)
  (append l (list x)))

(define (id x) x)

(define (deep-reverse-foldr dl)
  (deep-foldr '()
              id
              snoc
              dl))

(define (deep-reverse dl)
  (cond
    ((null? dl) '())
    ((atom? dl) dl)
    (else (append (deep-reverse (cdr dl))
                  (list (deep-reverse (car dl)))))))
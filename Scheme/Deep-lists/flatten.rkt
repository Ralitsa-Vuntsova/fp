(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (deep-foldr init term op dl)
  (cond ((null? dl) init)
        ((atom? dl) (term dl))
        (else (op (deep-foldr init term op (car dl))
                  (deep-foldr init term op (cdr dl))))))

(define (flatten-foldr dl)
  (deep-foldr '()
              list
              append
              dl))

(define (flatten dl)
  (cond
    ((null? dl) '())
    ((atom? dl) (list dl))
    (else (append (flatten (car dl))
                  (flatten (cdr dl))))))
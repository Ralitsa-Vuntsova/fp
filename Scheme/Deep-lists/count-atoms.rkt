(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (deep-foldr init term op dl)
  (cond ((null? dl) init)
        ((atom? dl) (term dl))
        (else (op (deep-foldr init term op (car dl))
                  (deep-foldr init term op (cdr dl))))))

(define (count-atoms-foldr dl)
  (deep-foldr 0
              (lambda (x) 1)
              +
              dl))

(define (count-atoms dl)
  (cond
    ((null? dl) 0)
    ((atom? dl) 1)
    (else (+ (count-atoms (car dl)) (count-atoms (cdr dl))))))

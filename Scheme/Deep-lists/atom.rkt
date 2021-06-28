(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

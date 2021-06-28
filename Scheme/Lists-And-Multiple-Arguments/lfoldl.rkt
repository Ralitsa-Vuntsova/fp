(define (lfoldl op init lst)
  
  (define (loop left result)
    (if (null? left)
        (cdr (reverse result))
        (loop (cdr left)
              (cons (op (car result) (car left))
                     result))))
  
  (loop lst (list init)))
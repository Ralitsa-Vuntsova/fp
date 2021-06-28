(load "./tree.rkt")

(define (tree->string tree)
  
  (define (helper tr)
    (if (null? tr)
        "*"
        (string-append (string open-figure-bracket)
                       (number->string (car tr))
                       " "
                       (helper (cadr tr))
                       " "
                       (helper (caddr tr))
                       (string close-figure-bracket)))
    )
  
  (helper tree)
  )


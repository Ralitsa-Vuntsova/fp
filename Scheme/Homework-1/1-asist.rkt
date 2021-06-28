(define (accumulate operation
                    transform
                    initial
                    start
                    next
                    end)
  (define (helper i)
    (if (> i end)
        initial
        (operation (transform i)
                   (helper (next i))))
    )
  (helper start)
  )

(define (accumulate-rev operation
                        transform
                        initial
                        start
                        next
                        end)
  (define (helper i)
    (if (> i end)
        initial
        (operation (helper (next i))
                   (transform i)))
    )
  (helper start)
  )

(define (id i) i)
(define (++ i) (+ i 1))
(define (-- i) (- i 1))

(define (squares n)
  (define upleft    #\u250C)
  (define upright   #\u2510)
  (define downleft  #\u2514)
  (define downright #\u2518)
  (define dash      #\u2500)
  (define bar       #\u2502)
  (define (noop a b) (display ""))
  (define (-1+2* i) (-- (* 2 i)))

  (define (corner isLeft isUp)
    (if isLeft
        ;ляво
        (if isUp
            ;горе
            upleft
            ;долу
            downleft)
        ;дясно
        (if isUp
            ;горе
            upright
            ;долу
            downright))
    )

  (define (print-line-generator isUp)
    (lambda (row)
      (define (print-sym-genenerator isLeft)
        (lambda (column)
          (cond
            ((= column (-1+2* row)) (display (corner isLeft isUp)))
            ((< column (-1+2* row)) (display (if (odd? column) bar " ")))
            (else (display dash)))
          )
        )
      
      (accumulate     noop (print-sym-genenerator #t) (display "") 1 ++ (-1+2* n))
      (display dash)
      (accumulate-rev noop (print-sym-genenerator #f) (display "") 1 ++ (-1+2* n))
      (display "\n")
      )
    )

  ;горе
  (accumulate     noop (print-line-generator #t) (display "") 1 ++ n)
  ;долу
  (accumulate-rev noop (print-line-generator #f) (display "") 1 ++ n)
  )
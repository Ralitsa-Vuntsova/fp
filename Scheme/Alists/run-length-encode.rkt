;Да се дефинира процедура (run-length-encode l), която кодира списъка l в асоциативен списък - списък от наредени двойки (<ключ> . <стойност>),
;където <ключ>-ът e пореден елемент от списъка l, а <стойност>-та е броят на последователните повторения на <ключ>.

(define (take-while p l)
  (if (or (null? l)
          (not (p (car l))))
      '()
      (cons (car l)
            (take-while p (cdr l)))))

(define (drop-while p l)
  (if (or (null? l)
          (not (p (car l))))
      l
      (drop-while p (cdr l))))

(define (run-length-encode l)
  (define (take-first-equals l)
    (take-while (lambda (x)
                  (= x (car l)))
                l))

  (define (drop-first-equals l)
    (drop-while (lambda (x)
                  (= x (car l)))
                l))
  
  (if (null? l)
      '()
      (cons (cons (car l)
                  (length (take-first-equals l)))
            (run-length-encode (drop-first-equals l)))))
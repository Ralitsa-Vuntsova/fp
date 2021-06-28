(load "./basics.rkt")

;Да се дефинира процедура (group-by f l), която връща асоциативен списък, в който ключовете са стойностите
;на процедурата f след прилагането ѝ върху елементи от списъка l, а срещу ключовете стои списък от елементите,
;за които процедурата f дава стойността от ключа.

(define (unique l)
  (if (null? l)
      '()
      (cons (car l)
            (unique (filter (lambda (x)
                              (not (equal? x (car l))))
                            l)))))

; (make-alist f keys)
(define (group-by f l)
  (make-alist (lambda (key)
                (filter (lambda (x)
                          (equal? (f x) key))
                        l))
              (unique (map f l))))
(define (number-rows M)
  (length M))

(define (number-cols M)
  (if (null? M)
      0
      (length (car M))))

(define (first-row M)
  (car M))

(define (without-first-row M)
  (cdr M))

(define (get-row n M)
  (list-ref M n))

(define (first-col M)
  (map car M))

(define (without-first-col M)
  (map cdr M))

(define (get-col n M)
  (map (lambda (x) (list-ref x n)) M))

(define (get-elem n m matrix)
  (list-ref (list-ref matrix n) m))

(define (remove n lst)
  (cond
    ((null? lst) lst)
    ((= n 0) (cdr lst))
    (else (cons (car lst)
                (remove (- n 1) (cdr lst))))))

(define (remove-row n M)
  (remove n M))

(define (remove-col n M)
  (map (lambda (x) (remove n x)) M))

(define (transpose M)
  (apply map list M))
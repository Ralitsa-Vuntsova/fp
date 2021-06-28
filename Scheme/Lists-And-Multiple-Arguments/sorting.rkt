;selection-sort

; remove-first - връща ни lst, но без първото срещане на x
(define (remove-first x lst)
  (cond ((null? lst) '())
        ((equal? x (car lst)) (cdr lst))
        (else (cons (car lst) (remove-first x (cdr lst))))))

; find-min - връща ни най-малкото число от непразен списък
(define (find-min lst)
  (cond ((null? (cdr lst)) (car lst))
        (else (min (car lst) (find-min (cdr lst))))))

(define (selection-sort lst)
  (if (null? lst)
      '()
        (let ((min-element (find-min lst)))
             (cons min-element (selection-sort (remove-first min-element lst))))))

;insertion-sort

(define (insert val lst)
  (cond
    ((null? lst) (list val))
    ((< val (car lst)) (cons val lst))
    (else (cons (car lst) (insert val (cdr lst))))
    ))

(define (foldr op init lst)
  (if (null? lst) init
      (op (car lst) (foldr op init (cdr lst)))))

(define (insertion-sort lst)
  (foldr insert '() lst))

;quick-sort

(define (filter p? lst)
  (cond
    ((null? lst) '())
    ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
    (else (filter p? (cdr lst)))))

(define (quick-sort lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((pivot (car lst))
            (rest (cdr lst)))
      (append (quick-sort (filter (lambda (x) (< x pivot))
                                 rest))
              (list pivot)
              (quick-sort (filter (lambda (x) (>= x pivot))
                                 rest))))))

;merge-sort

; merge - да се слеят два сортирани списъка
(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
        (else (cons (car l2) (merge l1 (cdr l2))))
  )
)

(define (drop n l)
  (if (null? l) '()
      (if (= n 0) l
          (drop (- n 1) (cdr l))
      )
  )
)

(define (take n l)
  (if (or (= n 0) (null? l)) '()
      (cons (car l) (take (- n 1) (cdr l)))
  )
)

(define (merge-sort l)
  (let ((len (length l)))
     (if (< len 2) l
         (merge (merge-sort (take (quotient len 2) l))
                (merge-sort (drop (quotient len 2) l))
         )
     )
  )
)

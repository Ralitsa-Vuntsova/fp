; Да се дефинира процедура (append l1 l2), която конкатенира списъците l1 и l2.

(define (append-list l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append-list (cdr l1) l2))))
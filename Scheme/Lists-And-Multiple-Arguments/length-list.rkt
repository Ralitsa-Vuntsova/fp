; Да се дефинира процедура (length-list lst), която намира броя на елементите на списъка l.

(define (length-list lst)
  (if (null? lst) 0
      (+ 1 (length-list (cdr lst)))))
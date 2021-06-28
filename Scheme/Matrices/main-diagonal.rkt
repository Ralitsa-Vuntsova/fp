;Дефинирайте процедура (main-diagonal matrix), която връща списък с елементите в главния диагонал на матрицата matrix.

(define (main-diagonal matrix)
  (if (null? matrix)
      '()
      (cons (caar matrix) (main-diagonal (map cdr (cdr matrix))))))
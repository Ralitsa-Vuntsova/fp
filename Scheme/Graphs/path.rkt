(load "./basics.rkt")

; Дефинирайте процедура (path? u v g), която проверява дали има път между върховете u и v в ориентирания граф g.

(define (path? u v g)
  (define (dfs path)
    (let ((current (car path)))
      (or (equal? current v)
          (and (not (member current (cdr path)))
               (search-child current
                             (lambda (child)
                               (dfs (cons child path)))
                             g)))))
  
  (dfs (list u)))
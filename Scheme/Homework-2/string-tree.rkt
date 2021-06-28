(load "./tree.rkt")

(define (string->tree str)

  (define string-without-spaces (remove-whitespace str))

  (define (root start i)
    (if (char-numeric? (string-ref str i))
        (root start (+ i 1))
        (string->number (substring (remove-whitespace str) start i))
        )
    )

  (define (index start i)
    (if (char-numeric? (string-ref str i))
        (index start (+ i 1))
        i
        )
    )
  
  (define (helper s i new-tree)
    (cond  
          ((and (char=? (string-ref s i) open-figure-bracket) (= i 0))
           (helper s (+ i 1) new-tree))
          ((char=? (string-ref s i) open-figure-bracket)
           (let
               ((index-with-tree (helper s (+ i 1) '())))
             (helper s (car index-with-tree) (append new-tree (list (cdr index-with-tree))))
             ))
          ((char=? (string-ref s i) char-empty-tree)
           (helper s (+ i 1) (append new-tree (list '()))))
          ((char-numeric? (string-ref s i))
             (helper s (index i i) (append new-tree (list (root i i))))
          )
          (else 
           (if (>= (+ i 1) (string-length s))
               new-tree
               (cons (+ i 1) new-tree) 
               )
           )
          )
    )
  (if (tree? str)
      (if (and (= (string-length string-without-spaces) 1) (char=? (string-ref string-without-spaces 0) char-empty-tree))
          '()
          (helper string-without-spaces 0 '()))
      #f)
  )

;'()
;(string->tree "*")
;(list 5 (list 22 (list 2 '() '()) (list 6 '() '())) (list 1 '() (list 3 (list 111 '() '()) '())))
;(string->tree "{5{22{2**}{6**}}{1*{3{111   **}*}}}")
;(list 2 (list 4 '() '()) '())
;(string->tree "{2{4**}*}")
;(list 2 '() (list 4 '() '()))
;(string->tree "{2*{4**}}")
;(list 222 (list 8 '() '()) (list 4 (list 11 '() '()) '()))
;(string->tree "{222{8**}{4{11*   *}*}}")
;(list 222 (list 8 (list 23 '() '()) '()) '())
;(string->tree "{222{8{23*   *}*}*}")
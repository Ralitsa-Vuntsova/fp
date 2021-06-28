(load "./basics.rkt")

;Да се напише функция (bloom t), която заменя всяко листо със стойност x със следното дърво:
;  x
; / \
;x   x

(define (bloom t)
  (cond
    ((null? t) '())
    ((leaf? t) (make-tree (root-tree t) (leaf (root-tree t)) (leaf (root-tree t))))
    (else (make-tree
           (root-tree t)
           (bloom (left-tree t))
           (bloom (right-tree t))))
  ))
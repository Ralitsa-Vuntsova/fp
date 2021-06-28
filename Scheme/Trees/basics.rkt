(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define empty-tree '())
(define empty-tree? null?)

(define (make-tree root left right)
  (list root left right))

(define (leaf root)
  (make-tree root '() '()))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define (leaf? tree)
  (and (not (empty-tree? tree))
       (empty-tree? (left-tree tree))
       (empty-tree? (right-tree tree))))


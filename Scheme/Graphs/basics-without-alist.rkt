(define G '((a b c d e m)
            (b a)
            (c a d f)
            (d a c e f)
            (e a d)
            (f c d)
            (h m)
            (m h a)))

(define G2 '((a b c)
             (b a d)
             (c a b)
             (d b)
             (e f)
             (f e)
             (m)))

(define H '((a b d e)
            (b a f)
            (c f)
            (d a)
            (e a)
            (f b c)))

(define (get-nodes g)
  (map car g))

(define (get-succs node g)
  (cond
    ((null? g) '())
    ((equal? node (caar g)) (cdar g))
    (else (get-succs node (cdr g)))))

(define (get-edges g)
  (foldr append '()
         (map
          (lambda (node) (map
                          (lambda (succ) (cons node succ)) (get-succs node g)))
          (get-nodes g)))
  )

; Претеглен граф
(define G2W '((a (b . 1) (c . 2))
              (b (a . 1) (c . 3) (d . 1))
              (c (a . 2) (b . 3))
              (d (b . 1))
              (e (f . 4))
              (f (e . 4))
              (m)))

; dava mi vurhovete
(define (get-nodes-w g)
  (map car g))

(define (get-node edge) (car edge))
(define (get-weight edge) (cdr edge))
(define (get-names-w succs) (map get-node succs))

; dava mi decata na vruh
(define (get-succs-w node graph)
  (get-names-w (get-succs node graph))
)

(define (filter p? lst)
  (cond
    ((null? lst) '())
    ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
    (else (filter p? (cdr lst)))))

; dava tegloto na rebro mejdu dva vyrha
(define (get-edge-weight begin end graph)
  (let ((succs (get-succs begin graph)))
    (if (null? succs) #f
        (let ((edge (filter (lambda (succ) (equal? (get-node succ) end)) succs)))
          (if (null? edge) #f
              (cdar edge))
        )
    )
  )
)


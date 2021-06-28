#lang racket/base
(require racket/stream)

(define (tree->stream tree order)
  
  (define (inorder stream tr)
    (if (not (null? tr))
        (and (inorder stream (cadr tr))
             (stream-cons stream (car tr)) 
             (inorder stream (caddr tr)))
        stream))

  (define (postorder stream tr)
    (if (not (null? tr))
        (and (postorder stream (cadr tr))
             (postorder stream (caddr tr))
             (stream-cons stream (car tr)) 
             )
        stream))

  (define (preorder stream tr)
    (if (not (null? tr))
        (and (stream-cons stream (car tr)) 
             (preorder stream (cadr tr))
             (preorder stream (caddr tr)))
        stream))

  (cond
    ((equal? order 'inorder) (inorder empty-stream tree))
    ((equal? order 'postorder) (postorder empty-stream tree))
    ((equal? order 'preorder) (preorder empty-stream tree))
    (else (display "Invalid tree or order"))
  ))

(stream->list (tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) 'inorder))
;2, 22, 6, 5, 1, 111, 3
(stream->list (tree->stream '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) 'inorder))
;2, 15, 7, 10, 2, 22, 6, 5, 1, 111, 3
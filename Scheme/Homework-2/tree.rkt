(define char-empty-tree #\*)
(define open-figure-bracket #\{)
(define close-figure-bracket #\})

(define (remove-whitespace str)
  
    (define (loop i cleared-str)
      (if (= i (string-length str))
          cleared-str
          (if (char-whitespace? (string-ref str i))
              (loop (+ i 1) cleared-str)
              (loop (+ i 1) (string-append cleared-str (string (string-ref str i)))))))
  
    (loop 0 ""))

(define (number-of-symbols symbol str)
  
  (define (helper i counter)
    (cond
      ((= i (string-length str)) counter)
      ((char=? (string-ref str i) symbol) (helper (+ i 1) (+ 1 counter)))
      (else (helper (+ i 1) counter))
    ))
  
  (helper 0 0)
  )

(define (is-there-whitespace-between-digits? str)
  
  (define (helper i char-before-whitespace)
    (cond
      ((= i (- (string-length str) 2)) #f)
      ((and (char-numeric? (string-ref str i)) (char-whitespace? (string-ref str (+ i 1))) (char-numeric? (string-ref str (+ i 2)))) #t)
      ((and (char-numeric? (string-ref str i)) (char-whitespace? (string-ref str (+ i 1)))) (helper (+ i 1) #t))
      ((and (char-whitespace? (string-ref str i)) (char-numeric? (string-ref str (+ i 1))) char-before-whitespace) #t)
      ((not (char-whitespace? (string-ref str i))) (helper (+ i 1) #f))
      (else (helper (+ i 1) char-before-whitespace))
    ))
  
  (helper 0 #f)
  )

(define (tree? str)
  
  (define (first-char str) (string-ref str 0))
  (define (last-char str) (string-ref str (- (string-length str) 1)))
  (define (number-open-brackets str) (number-of-symbols open-figure-bracket str))
  (define (number-close-brackets str) (number-of-symbols close-figure-bracket str))
  (define string-without-spaces (remove-whitespace str))

  (define (helper i counter-tree counter-subtree string)
    (cond
      ((and (= i (- (string-length string) 1))
            (char=? (string-ref string i) close-figure-bracket)
            )
       (if (and (= counter-tree 3) (>= counter-subtree 3))
           #t
           #f))
      ((char=? (string-ref string i) close-figure-bracket) (helper (+ i 1) (- counter-tree 2) counter-subtree string))      
      ((char=? (string-ref string i) open-figure-bracket) (helper (+ i 1) counter-tree 0 string))
      ((char-numeric? (string-ref string i))
       (if (char-numeric? (string-ref string (+ i 1)))
           (helper (+ i 1) counter-tree counter-subtree string)
           (helper (+ i 1) (+ counter-tree 1) (+ counter-subtree 1) string)))
      ((char=? (string-ref string i) char-empty-tree) (helper (+ i 1) (+ counter-tree 1) (+ counter-subtree 1) string)) 
      (else #f)
      )
    )
  
  (cond
    ((equal? string-without-spaces "*") #t)
    ((equal? string-without-spaces "{***}") #f)
    ((not (= (number-open-brackets string-without-spaces)
             (number-close-brackets string-without-spaces))) #f)
    ((not (equal? (first-char string-without-spaces) open-figure-bracket)) #f)
    ((not (equal? (last-char string-without-spaces) close-figure-bracket)) #f)
    ((is-there-whitespace-between-digits? str) #f)
    (else (helper 0 0 0 string-without-spaces))))

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

(define (balanced? tree)
  
  (define (height t)
    (if (or (null? t) (null? (car t)) (null? (cdr t)))
        0
        (+ 1 (max (height (cadr t))
                  (height (caddr t))))
        )
  )
  
  (if (null? tree)
      #t
      (if (and (<= (abs (- (height (cadr tree)) (height (caddr tree)))) 1)
               (balanced? (cadr tree))
               (balanced? (caddr tree)))
          #t
          #f))
  )

(define (order? tree)
  
  (define (helper tr left right)
    (cond
      ((null? tr) #t)
      ((and (not (null? left)) (<= (car tr) (car left))) #f)
      ((and (not (null? right)) (>= (car tr) (car right))) #f)
      (else (and
             (helper (cadr tr) left tr)
             (helper (caddr tr) tr right)))
    ))
  
  (helper tree '() '())
  )

(define (tree->string tree)
  
  (define (helper tr)
    (if (null? tr)
        "*"
        (string-append (string open-figure-bracket)
                       (number->string (car tr))
                       " "
                       (helper (cadr tr))
                       " "
                       (helper (caddr tr))
                       (string close-figure-bracket)))
    )
  
  (helper tree)
  )

;(define (tree->stream tree order)
;  
;  (define (inorder stream tr)
;    (if (not (null? tr))
;        (and (inorder stream (cadr tr))
;             (stream-cons stream (car tr)) 
;             (inorder stream (caddr tr)))
;        stream))
;
;  (define (postorder stream tr)
;    (if (not (null? tr))
;        (and (postorder stream (cadr tr))
;             (postorder stream (caddr tr))
;             (stream-cons stream (car tr)) 
;             )
;        stream))
;
;  (define (preorder stream tr)
;    (if (not (null? tr))
;        (and (stream-cons stream (car tr)) 
;             (preorder stream (cadr tr))
;             (preorder stream (caddr tr)))
;        stream))
;
;  (cond
;    ((equal? order 'inorder) (inorder empty-stream tree))
;    ((equal? order 'postorder) (postorder empty-stream tree))
;    ((equal? order 'preorder) (preorder empty-stream tree))
;    (else (display "Invalid tree or order"))
;  ))

;(stream->list (tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) 'inorder))
;2, 22, 6, 5, 1, 111, 3
;(stream->list (tree->stream '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) 'inorder))
;2, 15, 7, 10, 2, 22, 6, 5, 1, 111, 3
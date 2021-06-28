; Да се напише функция (2^ n), която изчислява 2^n (където n е естествено), използвайки:
; accumulate, директно
; nchk

(define (accumulate-r op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a))

(define (id x) x)

(define (1+ x) (+ 1 x))

(define (2^-ac n)
  (accumulate-r * (lambda (x) 2) 1 1 1+ n))

; 2^n is a sum from k=0 to n (n-choose-k)
(define (nchk-ac n k)
  (define (term i) (/ (- (+ n 1) i) i))
  (accumulate-r * term 1 1 1+ k))


(define (2^ n)
  (define (term i) (nchk-ac n i))
  (accumulate-r + term 0 0 1+ n))
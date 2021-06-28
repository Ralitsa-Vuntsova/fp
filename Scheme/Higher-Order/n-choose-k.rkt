; Да се напише функция (nchk n k), която за дадени естествени числа n и k изчислява биномния коефициент 'n над k' (using factorial)
; Упътване: тук accumulate се ползва индиректно от функцията факториел
; името идва от n-choose-k - по колко начина можем да изберем k неща измежду n

(define (accumulate-r op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a))

(define (id x) x)

(define (1+ x) (+ 1 x))

(define (fact n)
  (accumulate-r
   *
   id
   1
   1
   1+
   n))

(define (nchk n k)
  (/ (fact n) (* (fact (- n k)) (fact k))))

; Да се напише функцията nchk с едно извикване на accumulate (product from i=1 to k ((n+1-i)/i))

(define (nchk-ac n k)
  (define (term i) (/ (- (+ n 1) i) i))
  (accumulate-r * term 1 1 1+ k))
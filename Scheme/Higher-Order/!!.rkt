; Да се напише функция (!! n), която по дадено естествено число n изчислява n!! - произведението на всички числа, по-малки или равни на n, със същата четност:
;(!! 5) -> 15    ; =1*3*5
;(!! 10) -> 3840 ; =2*4*6*8*10

(define (accumulate-r op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a))

(define (2+ n) (+ 2 n))

(define (id x) x)

(define (!! n)
  (accumulate-r
   *
   id
   1
   (if (even? n) 2 1)
   2+
   n))
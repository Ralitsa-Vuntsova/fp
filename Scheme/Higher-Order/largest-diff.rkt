;а) Да се реализира функция product-digits, която намира произведението от цифрите на дадено естествено число.
;б) Нека с {n} означим разликата на n и произведението на цифрите на n. Да се реализира функция largest-diff,
;която намира най-голямата разлика {m} – {n} за m, n ∈ [a; b], където a и b са параметри на функцията. Пример:
;(largest-diff 28 35) ; -> 19 (= {30} - {29} = (30 - 0) - (29 - 18))

(define (accumulate-r op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a))

(define (1+ x) (+ 1 x))

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (ith-element i n)
  (remainder (quotient n (expt 10 (- (count-digits n) i))) 10))

(define (product-digits n)
  (define (term i) (ith-element i n))
  (accumulate-r * term 1 1 1+ (count-digits n)))

(define (my-max a b)
  (if (> a b) a b))

(define (my-min a b)
  (if (< a b) a b))

(define (max-prod a b)
  (if (> a b) 0
      (my-max (- a (product-digits a)) (max-prod (+ a 1) b))))

(define (min-prod a b max)
  (if (> a b) max
      (my-min (- a (product-digits a)) (min-prod (+ a 1) b max))))

(define (largest-diff a b)
  (- (max-prod a b) (min-prod a b (max-prod a b))))
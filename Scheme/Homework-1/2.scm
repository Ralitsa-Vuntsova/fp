(define (toBinary n)
  (if (= n 0) 0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))

(define (toDecimal n)
  (if (= n 0) 0
      (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))))

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))
      ))

(define (ith-element i n)
  (remainder (quotient n (expt 10 (- (count-digits n) i))) 10))

(define (add-or-remove-digit zero-or-one op position binaryNum)
(if (= zero-or-one (ith-element (- (count-digits binaryNum) position) binaryNum))
      binaryNum
      (op binaryNum (* 1 (expt 10 position)))))

(define (add-digit position binaryNum)
  (add-or-remove-digit 1 + position binaryNum))

(define (set-add set elem)
  (define binary-elem (toBinary set))
  (toDecimal(add-digit elem binary-elem)))

(define (remove-digit position binaryNum)
 (add-or-remove 0 - position binaryNum))

(define (set-remove set elem)
  (define binary-elem (toBinary set))
  (toDecimal(remove-digit elem binary-elem)))

(define (contains? i n)
  (if (= (ith-element i n) 1) #t #f))

(define (set-contains? set elem)
  (define binary-elem (toBinary set))
  (contains? elem binary-elem))

(define (set-empty? set)
  (if (= set 0) #t #f))

(define (count-ones n)
  (cond
       ((= n 0) 0)
       ((= (remainder n 10) 1) (+ 1 (count-ones (quotient n 10))))
       (else (count-ones (quotient n 10)))))

(define (set-size set)
  (define binary-elem (toBinary set))
  (count-ones binary-elem))

(define (two-sets-generic op s1 s2)
  (define max-size
    (max (count-digits (toBinary s1)) (count-digits (ToBinary s2))))
  (define (loop i set end)
    (define update-set
      (if (op (set-contains? s1 i) (set-contains? s2 i))
          (set-add set i)
          set))
    (if (> i end)
        set
        (loop (+ i 1) update-set end)))
  (loop 0 0 max-size))

(define (set-intersect s1 s2)
  (two-sets-generic (lambda (x y) (and x y)) s1 s2))

(define (set-union s1 s2)
  (two-sets-generic (lambda (x y) (or x y)) s1 s2))

(define (set-difference s1 s2)
  (two-sets-generic (lambda (x y) (and x (not y))) s1 s2))

(define (w n)
  (case n
    ((0) 1)
    ((1) 3)
    ((2) 5)
    ((3) 4)
    ((4) 1)
    ((5) 3)
    ((6) 2)))

(define (p n)
  (case n
    ((0) 5)
    ((1) 10)
    ((2) 15)
    ((3) 7)
    ((4) 8)
    ((5) 9)
    ((6) 4)))

(define (maximum x1 x2)
  (if (> x1 x2) x1 x2))

(define (price c n w p)
  (cond ((or (= n 0) (= c 0)) 0)
        ((> (w (- n 1)) c) (price c (- n 1) w p))
        (else (maximum (+ (p (- n 1)) (price (- c (w (- n 1))) (- n 1) w p))
                       (price c (- n 1) w p)))))

(define (knapsack-helper c priceOfSubjects set n w p)
  (cond
        ((= priceOfSubjects 0) set)
        ((or (= n 0) (= c 0)) 0)
        ((> (p (- n 1)) priceOfSubjects) (knapsack-helper c priceOfSubjects set (- n 1) w p))
        (else
         (maximum (knapsack-helper (- c (w (- n 1))) (- priceOfSubjects (p (- n 1))) (set-add set (- n 1)) (- n 1) w p)
                  (knapsack-helper c priceOfSubjects set (- n 1) w p)))))

(define (knapsack c n w p)
  (define priceOfSubjects (price c n w p))
  (knapsack-helper c priceOfSubjects 0 n w p))
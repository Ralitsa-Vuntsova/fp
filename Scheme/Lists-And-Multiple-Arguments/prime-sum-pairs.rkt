;Процедура (prime-sum-pairs n), която по дадено цяло положително число n намира всички наредени тройки (i, j, i + j), за които:
;i и j са цели положителни числа
;1 ≤ j < i ≤ n
;i + j е просто

(define (flatmap f l)
  (foldr append '() (map f l)))

(define (prime? n)
  
  (define (count-divisors n)
    
    (define (divides? k n)
      (= (remainder n k) 0))
    
    (define (divisors-up-to k)
      (cond ((= k 0) 0)
            ((divides? k n)
             (+ 1 (divisors-up-to (- k 1))))
            (else (divisors-up-to (- k 1)))))
    
    (divisors-up-to n))
  
  (= (count-divisors n) 2))

(define (prime-sum-pairs n)
  
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  
  (define pairs
    (flatmap (lambda (i)
               (map (lambda (j) (list i j))
                    (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))
  
  (map make-pair-sum (filter prime-sum? pairs)))
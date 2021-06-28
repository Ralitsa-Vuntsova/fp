; Функцията, която ще напишете, очаква за вход две цели положителни числа, кръстени за по-удобно "first" и "second", и бинарна функция.
; Ако f1,f2,f3,...,fk и s1,s2,s3,...,sl са цифрите на съответните числа, а g е нашата бинарна функция, търсим резултатът от g(f1,s1) + g(f2,s2) + g(f3,s3) + ...
; Функцията да терминира при достигане края на едно от числата.

(define (accumulate-r op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a))

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))
      ))

(define (id x) x)

(define (1+ x) (+ 1 x))

(define (n-digit i n)
  (remainder (quotient n (expt 10 (- (count-digits n) i))) 10))

(define (combine-numbers first second g)
  (define min-digits-count
    (if (> (count-digits first) (count-digits second)) (count-digits second) (count-digits first)))
  (define (term i) (g (n-digit i first) (n-digit i second)))
  (accumulate-r + term 0 1 1+ min-digits-count))
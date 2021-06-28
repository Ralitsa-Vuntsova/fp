; Напишете функция, която проверява дали има 2 еднакви цифри в число

(define (count-digits num)
  (if (< num 10)
      1
      (+ 1 (count-digits (quotient num 10)))))

(define (nth-position position num)
  (remainder (quotient num (expt 10 (- (count-digits num) position))) 10))

(define (has-same-digit? number)
  (define (helper a-fix a b)
    (cond
      ((>= a-fix b) #f)
      ((>= a b) (helper (+ a-fix 1) (+ a-fix 1) b))
      ((= (nth-position a-fix number) (nth-position (+ a 1) number)) #t)
      (else (helper a-fix (+ a 1) b))))
  (helper 0 0 (count-digits number)))
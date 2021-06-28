; Да се напише функция (increasing? n), която проверява дали цифрите на дадено естествено число са в нарастващ ред, четени отляво-надясно:
(define (last-digit n)
  (remainder n 10))

(define (increasing? n)
  (cond ((< n 10) #t)
        ((< (last-digit n) (last-digit (quotient n 10))) #f)
        (else (increasing? (quotient n 10)))))
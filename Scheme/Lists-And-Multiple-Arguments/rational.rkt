;Създаването на рационално число ще е просто слагането на числител и знаменател в двойка.
;Първият елемент на двойката означава числител, а втория - знаменател.

(define (make-rational num denom)
  (cons num denom))

;Достъпването на числителя и знаменателя се случва с car и cdr

(define (num rational)
  (car rational))

(define (denom rational)
  (cdr rational))

;Събиране, изваждане, умножение, делениe

(define (sum-rationals first second)
  (/ (+ (* (num first) (denom second))
        (* (num second) (denom first)))
     (* (denom first) (denom second))
  )
)

(define (diff-rationals first second)
  (/ (- (* (num first) (denom second))
        (* (num second) (denom first)))
     (* (denom first) (denom second))
  )
)

(define (mult-rationals first second)
  (/ (* (num first) (num second))
     (* (denom first) (denom second))
  )
)

(define (div-rationals first second)
  (/ (* (num first) (denom second))
     (* (num second) (denom first))
  )
)

;Съкращаване
(define (gcd-rat rat)
  (define g (gcd (num rat) (denom rat)))
  (cons (quotient (num rat) g) (quotient (denom rat) g)))

;Примери
(define rational-pi (make-rational 22 7))
(define random-number (make-rational 1 3))
(num rational-pi)
(denom random-number)
(mult-rationals rational-pi random-number)
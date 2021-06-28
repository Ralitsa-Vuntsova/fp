;Да се напише процедура (middle-digit n), която намира средната цифра от записа на подадено естествено число n.
;Ако n е с четен брой цифри, процедурата да връща -1. Примери:
#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (nth-element i n)
  (remainder (quotient n (expt 10 (- (count-digits n) i))) 10))

(define (middle-digit n)
  (define middle (quotient (count-digits n) 2))
  (if (even? (count-digits n))
      -1
      (nth-element (+ middle 1) n)))

(define tests
  (test-suite "Middle digit tests"
              (check-equal? (middle-digit 452) 5)
              (check-equal? (middle-digit 4712) -1)))

(run-tests tests 'verbose)
      
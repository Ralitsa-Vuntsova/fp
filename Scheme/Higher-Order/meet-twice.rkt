;Да се напише процедура (meet-twice? f g a b), която проверява дали в целочисления интервал [a, b]
;съществуват две различни цели числа x и y такива, че f(x) = g(x) и f(y) = g(y). Примери:
;(meet-twice? (lambda (x) x) (lambda (x) (- x)) -3 1) ; -> #f
;(meet-twice? (lambda (x) x) sqrt 0 5) ; -> #t

(define (exists p? a b)
  (and (<= a b)
       (or (p? a) (exists p? (+ a 1) b))))

(define (for-all p? a b)
  (or (> a b)
      (and (p? a) (for-all p? (+ a 1) b))))

(define (meet-twice? f g a b)
  (exists (lambda (x)
            (exists (lambda (y)
                      (and (not (= x y))
                           (= (f x) (g x))
                           (= (f y) (g y))))
                    a
                    b))
            a
            b))
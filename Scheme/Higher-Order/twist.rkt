; Да се напише функция (twist k f g), която за дадени
; едноместни функции f и g и четно число k връща функция, еквивалентна на f(g(f(g(...(x)...)))),
; където общият брой извиквания на f и g е k.

(define (id x) x)

(define (twist k f g)
  (if (= k 0) id
      (lambda (x) (f (g ((twist (- k 2) f g) x))))))
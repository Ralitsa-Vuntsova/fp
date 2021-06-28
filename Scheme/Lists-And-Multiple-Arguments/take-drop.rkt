;Да се напишат функциите (take n lst) и (drop n lst), който съответно взимат или премахват първите n елемента на списък. drop е същото като list-tail.
;(take 3 '(1 2 3 4 5)) -> '(1 2 3);
;(take 10 '(1 2 3 4 5)) -> '(1 2 3 4 5)
;(drop 3 '(1 2 3 4 5)) -> '(4 5)
;(drop 10 '(1 2 3 4 5)) -> '()

(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

; Напишете функция (take-while p? lst), която приема предикат и списък и взима първите му елементи, които отговарят на предиката.
; Използваме я, когато не знаем точно колко елемента от началото искаме да вземем (напр. първите 6),
; а имаме условие, което трябва да е изпълнено (например първите четни елементи).
; Напишете функция (drop-while p? lst)

(define (take-while p? lst)
  (if (or (null? lst) (not (p? (car lst))))
      '()
      (cons (car lst) (take-while p? (cdr lst)))))

(define (drop-while p? lst)
  (if (or (null? lst) (not (p? (car lst))))
      lst
      (drop-while p? (cdr lst))))
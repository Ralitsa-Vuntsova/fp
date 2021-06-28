; Да се напише функцията от по-висок ред (complement p), която по даден предикат връща неговото отрицание:

(define (complement p)
  (lambda (x) (not (p x))))
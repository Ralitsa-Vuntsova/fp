; Да се напише функция от по-висок ред (constantly c), която по дадена константа c връща функцията f(x)=c:

(define (constantly c)
  (lambda (x) c))
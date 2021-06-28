(load "./basics.rkt")

; Питагорова тройка наричаме наредената тройка от числа (a, b, c) такава, че a2 + b2 = c2.
; Дефинирайте поток pythagorean-triples от Питагоровите тройки (a, b, c) такива, че 1 ≤ a ≤ b ≤ c, където a, b и c са цели числа.

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define (range-stream from to)
  (take-stream (+ to (- from) 1)
               (integers-from from)))

(define (map-stream f s)
  (if (empty-stream? s)
      empty-stream
      (cons-stream (f (head s))
                   (map-stream f (tail s)))))

(define (filter-stream p s)
  (cond ((empty-stream? s) empty-stream)
        ((p (head s)) (cons-stream (head s)
                                   (filter-stream p (tail s))))
        (else (filter-stream p (tail s)))))

(define (append-stream s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-stream (tail s1) s2))))

(define (concat-streams ss)
  (cond ((empty-stream? ss) empty-stream)
        ((empty-stream? (head ss)) (concat-streams (tail ss)))
        (else (cons-stream (head (head ss))
                           (append-stream (tail (head ss))
                                          (concat-streams (tail ss)))))))

(define (flatmap-stream f s)
  (concat-streams (map-stream f s)))

(define triples
  (flatmap-stream (lambda (c)
                    (flatmap-stream (lambda (b)
                                      (map-stream (lambda (a)
                                                    (list a b c))
                                                  (range-stream 1 b)))
                                    (range-stream 1 c)))
                  (integers-from 1)))

(define (square x)
  (* x x))

(define pythagorean-triples
  (filter-stream (lambda (triple)
                   (= (+ (square (car triple))
                         (square (cadr triple)))
                      (square (caddr triple))))
                 triples))
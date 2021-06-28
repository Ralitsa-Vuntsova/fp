(require racket/stream)

;(define (infinite n)
;  (cons n (delay (infinite (+ n 1)))))

(define (infinite n)
  (stream-cons n (infinite (+ n 1))))

;(define (run p n)
;  (cond ((> n 0) (printf "~a: ~a\n" n (car p))
;                 (run (force (cdr p)) (- n 1)))
;        (else (display "End\n"))))

(define (run p n)
  (cond ((> n 0) (printf "~a: ~a\n" n (stream-first p))
                 (run (stream-rest p) (- n 1)))
        (else (display "End\n"))))

; stream и stream* са аналогични на list и list*
;(stream-length s) Дължина на потока s (s трябва да е краен)
;(stream? v) Проверява дали v е поток
;(stream-empty? s) Проверява дали s е празен
;(stream->list s) Преобразува поток до списък (s трябва да е краен)
;(stream-ref s i) Връща i-ия елемент на s
;(stream-tail s i) Поток получен от s, след премахване на първите iелемента
;(stream-take s i) Поток от първите i-елемента на s

(define (interval a b)
  (if (> a b) empty-stream
      (stream-cons a (interval (+ a 1) b))))

(define (fib-stream)
  (define (gen n-2 n-1)
    (stream-cons n-2 (gen n-1 (+ n-1 n-2))))
  (gen 1 1))

(define (stream-append s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons
       (stream-head s1)
       (stream-append (stream-rest s1) s2))))

(define (stream-map f s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (f (stream-first s))
                   (stream-map f (stream-rest s)))))

(define (stream-filter pred? s)
  (cond
    ((stream-empty? s)
     empty-stream)
    ((pred? (stream-first s))
     (stream-cons (stream-first s)
                  (stream-filter pred? (stream-rest s))))
    (else (stream-filter pred? (stream-rest s)))
))

(define (stream-fold f init s)
  (if (stream-empty? s)
      init
      (stream-fold f
                   (f init (stream-first s))
                   (stream-rest s))))

(define (stream-sum s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (stream-cons (+ (stream-first s1)
                         (stream-first s2))
                      (stream-sum (stream-rest s1)
                                  (stream-rest s2))))
))

(define (ones)
  (stream-cons 1 (ones)))

(define (nat)
  (stream-cons 0
               (stream-sum (nat) (ones))))

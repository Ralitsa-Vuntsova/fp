(define (accumulate op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a))

(define (1+ x)(+ 1 x))

(define (noop a b)
  (display ""))

(define (display-n-times n symbol)
  (define (term i) (display symbol))
  (accumulate noop term 0 1 1+ n))

(define (display-n-times-symbols n symbol1 symbol2)
  (define (term i) (display symbol1) (display symbol2))
  (accumulate noop term 0 1 1+ n))

(define (first-row number-of-cols)
  (display #\┌)
  (display-n-times (- number-of-cols 2) #\─)
  (display  #\┐)
  (display #\newline))

(define (last-row number-of-cols)
  (display  #\└)
  (display-n-times (- number-of-cols 2) #\─)
  (display #\┘)
  (display #\newline))

(define (upper-half row number-of-cols)
  (display-n-times-symbols (- row 1) #\│ #\space)
  (display #\┌)
  (display-n-times (- (- number-of-cols 2) (* 4 (- row 1))) #\─)
  (display #\┐)
  (display-n-times-symbols (- row 1) #\space #\│)
  (display #\newline))

(define (lower-half row number-of-cols number-of-rows)
  (display-n-times-symbols (- number-of-rows row) #\│ #\space)
  (display #\└)
  (display-n-times (- (- number-of-cols 2) (* 4 (- number-of-rows row))) #\─)
  (display #\┘)
  (display-n-times-symbols (- number-of-rows row) #\space #\│)
  (display #\newline))

; area = (horizontal_size * vertical_size) + number_of_newlines
; horizontal_size = 4n-1
; vertical_size = 2n
(define (squares n)
  (define number-of-rows (* 2 n))
  (define init (display  ""))
  (define (term i)
                    (cond
                      ((= i 1) (first-row (- (* 4 n) 1)))
                      ((= i (* 2 n)) (last-row (- (* 4 n) 1)))
                      ((<= i n) (upper-half i (- (* 4 n) 1)))
                      (else (lower-half i (- (* 4 n) 1) (* 2 n)))
                    ))
  (accumulate noop term init 1 1+ number-of-rows))

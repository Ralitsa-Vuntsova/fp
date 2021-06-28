;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 1
; 2020-11-07
;
; Начален час на контролното: 10:00
; Име: Ралица Альошева Вунцова
; ФН: 62312
; Специалност: Софтуерно инженерство
; Курс: 3
; Административна група: 4
;

;Едно естествено число ще наричаме "валидно", ако в неговия запис никъде не се срещат две последователни нули. Например 0, 5, 123, 1230 и 1023012301 са валидни числа.
;12003 и 120000000051 не са валидни числа.
;Реализирайте следните функции:
;А) (number-valid? n), която проверява дали естественото число n е валидно.

(define (number-valid? n)
  (cond
    ((= n 0) #t)
    ((= (remainder n 10) (remainder (quotient n 10) 10) 0) #f)
    (else (number-valid? (quotient n 10)))))

;Б) (valid->nset n), която получава естестено число n. Ако то е валидно, функцията извлича от записа му всички числа, които
;се намират между нулите в записа му и от тях формира множество S, което се връща като резултат. Ако n не е валидно число, функцията да връща #f.
;Примери:
;(valid->nset 5050123050) → множеството {5,123}
;(valid->nset 0) → празното множество
;(valid->nset 120034) → #f

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))
    ))

(define (ith-position i n)
  (remainder (quotient n (expt 10 (- (count-digits n) i))) 10))

(define (toBinary n)
  (if (= n 0) 0
  (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))

(define (toDecimal n)
  (if (= n 0) 0
  (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))))

(define (set-add elem set)
  (define binaryNum (toBinary set))
  (if (= (ith-position (- (count-digits binaryNum) elem) binaryNum) 1)
      set
      (toDecimal (+ binaryNum (expt 10 elem)))
  ))

; slagam v set poziciite na nulite ot chisloto
(define (set-of-nulls set n)
  (cond
    ((= n 0) set)
    ((= (remainder n 10) 0) (set-of-nulls (set-add (count-digits n) set) (quotient n 10)))
    (else (set-of-nulls set (quotient n 10)))))

(define (number-of-nulls n)
  (cond
    ((= n 0) 0)
    ((= (remainder n 10) 0) (+ 1 (number-of-nulls (quotient n 10))))
    (else (number-of-nulls (quotient n 10)))))

(define (helper set a-fix a b binaryNulls n)
  (cond
    ((>= a-fix b) set)
    ((>= a b) (helper set (+ a-fix 1) (+ a-fix 1) b binaryNulls n))
    ((= (ith-position a-fix binaryNulls) (ith-position (+ a 1) binaryNulls) 1)
     (helper (set-add (string->number (substring (number->string n) (- (count-digits binaryNulls) (+ a 1)) (- (- (count-digits binaryNulls) a-fix) 1))) set)
             (+ a 1)
             (+ a 1)
             b
             binaryNulls
             n))
    (else (helper set a-fix (+ a 1) b binaryNulls n))
      ))

(define (substring-set n set)
  (define binaryNulls (toBinary (set-of-nulls 0 n)))
  (helper set 1 1 (count-digits binaryNulls) binaryNulls n)
  )

(define (valid->nset n)
  (cond
    ((= n 0) 0)
    ((< (number-of-nulls n) 2) #f)
    ((number-valid? n) (substring-set n 0))
    (else #f)
      ))

;Напишете функция (make-nset a b pred?), която добавя в сет всички елементи между а и b, за които е изпълнено pred?
;Използвайте accumulate.

(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

(define (id x) x)
(define (1+ x) (+ 1 x))

(define (filter-accumulate pred? set a b)
  (define (loop i set)
      (cond
        ((> i b) set)
        ((pred? i) (loop (+ 1 i) (set-add i set)))
        (else (loop (+ i 1) set))
  ))
  (loop a set)
  )

(define (make-nset a b pred?)
  (define set 0)
  (filter-accumulate pred? set a b))

; drugo reshenie
(define (set-add-changed elem set)
  (set-add elem set)) ; ako set ne dobavya nuli

(define (make-nset a b pred?)
  (accumulate set-add-changed
              (lambda (i) (if (pred? i) i -1))
              0
              a
              (lambda (x) (+ 1 x))
              b))
;Аритметичен израз ще представяме като символен низ по следния начин:
;Празният низ е валиден израз.
;Ако n е естествено число, то n е валиден израз.
;Ако a и b са непразни аритметични изрази, то и следните изрази също са: a+b, a-b, a*c, a/c, a^c.
;Те представят съответно събиране, изваждане, умножение, деление и степенуване.
;Преди и след всяко число в израз позволяваме да има произволен брой празни (whitespace) символи. По-долу са дадени примери за валидни изрази:
;""
;"10"
;"    10   "
;"10+5*2"
;"  10 + 5       *2"
;Изразите оценяваме съгласно стандартните правила на аритметиката.
;За пълнота считаме, че (1) празният израз има оценка нула, (2) всички операции са ляво-асоциативни.

;(expr-valid? expr) ; проверява дали expr е валиден израз. Например:
                    ; (expr-valid? "10   + 20") → #t
                    ; (expr-valid? "10 20 + 5") → #f
                    ; (expr-valid? "++++ 5") → #f
                    ; (expr-valid? "+++") → #f

; string "", char #\
(define (char-operation? c)
  (cond ((char=? c #\+) #t)
        ((char=? c #\-) #t)
        ((char=? c #\*) #t)
        ((char=? c #\/) #t)
        ((char=? c #\^) #t)
        (else #f)))

(define (remove-whitespace-and-leading-0 str)
  
  (define (remove-whitespace str)
    (define (loop i cleared-str)
      (if (= i (string-length str))
          cleared-str
          (if (char-whitespace? (string-ref str i))
              (loop (+ i 1) cleared-str)
              (loop (+ i 1) (string-append cleared-str (string (string-ref str i)))))))
    (loop 0 ""))
  
  (define (remove-leading-0 str)
    (define (loop i first-digit? cleared-str)
      (cond ((= 0 (string-length str)) "")
            ;dobavyam posledniya element (da ne izpuskam poslednata nula)
            ((= (+ i 1) (string-length str)) (string-append cleared-str (string (string-ref str i))))
            ;append-va si operaciyata
            ((char-operation? (string-ref str i)) (loop (+ i 1) #t (string-append cleared-str (string (string-ref str i)))))
            ;ako purvata cifra e 0, propuska ya
            ((and first-digit? (char=? (string-ref str i) #\0) (char-numeric? (string-ref str (+ i 1)))) (loop (+ i 1) #t cleared-str))
            ;ako purvata cifra ne e 0, append-va ya
            (else (loop (+ i 1) #f (string-append cleared-str (string (string-ref str i)))))))
    (loop 0 #t ""))
  
  (remove-leading-0 (remove-whitespace str)))

(define (expr-valid? expr)
  
  (define (all-chars-valid?)
    (define (char-valid? c)
      (cond ((char-numeric? c) #t)
            ((char-whitespace? c) #t)
            ((char-operation? c) #t)
            (else #f)))
    (define (loop i)
      (cond ((= i 0) #t)
            ((char-valid? (string-ref expr (- i 1))) (loop (- i 1)))
            (else #f)))
    (loop (string-length expr)))
  
  (define (correct-whitespaces?)
    (define (loop i was-there-whitespace? last-char-before-whitespace)
      (cond ((= i 0) #t)
            ;ako i e prazno prostranstvo, was-there-whitespace stava true
            ((char-whitespace? (string-ref expr (- i 1))) (loop (- i 1) #t last-char-before-whitespace))
            ;proveryava dali ima prazno prostranstvo mejdu cifri
            ((and (char-numeric? (string-ref expr (- i 1))) (char-numeric? last-char-before-whitespace) was-there-whitespace?) #f)
            ;ako e nqmalo prazni prostranstva, zapazvame s last-char-before-whitespace i
            (else (loop (- i 1) #f (string-ref expr (- i 1))))))
    (loop (string-length expr) #f #\space))
  
  (define (correct-operations?)
    (define (loop i was-there-operation?)
      (if (= i 0)
          ; ako purviya element na stringa e operaciya => false
          (if was-there-operation? #f #t)
          (if (char-operation? (string-ref expr (- i 1)))
              ; ako e operaciya stringa => 2 vuzmojnosti // false ako ima dve operacii edna do druga, produljavame v protiven sluchai
              (if was-there-operation? #f (loop (- i 1) #t))
              ; ako ne e operaciya, produljavame
              (loop (- i 1) #f))))
    (loop (string-length expr) #t))
  
  (define (division-by-0?)
    (define (loop i expr)
      (cond ((= 0 (string-length expr)) #f)
            ((= (+ i 1) (string-length expr)) #f)
            ((and (char=? (string-ref expr i) #\/) (char=? (string-ref expr (+ i 1)) #\0)) #t)
            (else (loop (+ i 1) expr))))
    (loop 0 (remove-whitespace-and-leading-0 expr)))
  
  (if (= 0 (string-length (remove-whitespace-and-leading-0 expr)))
      #t
      (and (all-chars-valid?) (correct-whitespaces?) (correct-operations?) (not (division-by-0?)))))

;(expr-rp expr)     ; връща представянето на expr в обратен полски запис.
                    ; Ако expr е невалиден израз, да се върне #f.
                    ; За разделител между аргументите да се използва запетая.Например:
                    ; (expr-rp "10+20*30") → "10,20,30*+"

(define (expr-rp expr)
  
  (define (first-char str)
    (if (< 0 (string-length str))
        (string-ref str 0)
        #\space))
  
  (define (remove-first-char str)
    (if (< 1 (string-length str))
        (substring str 1 (string-length str))
        ""))
  
  ; operator 1 s po-golqm prioritet li ot operator 2
  (define (operator-precedes? op1 op2)
    ; ako purviya operator e ^ => true, obache ako i vtoriya e ^ => false
    (cond ((char=? op1 #\^) (if (char=? op2 #\^) #f #t))
          ; ako purviya operator e * ili / i ako vtoriya e ^, * ili / => false, inache true
          ((or (char=? op1 #\*) (char=? op1 #\/))
           (if (or (char=? op2 #\^) (char=? op2 #\*) (char=? op2 #\/)) #f #t))
          ; ako purviya operator e + ili - i ako vtoriya e ^, *, /, + ili - => false, inache true
          ((or (char=? op1 #\+) (char=? op1 #\-))
           (if (or (char=? op2 #\^) (char=? op2 #\*) (char=? op2 #\/) (char=? op2 #\+) (char=? op2 #\-)) #f #t))
          ; v protiven sluchai => false
          (else #f)))
  
  (define (loop i reversed-polish-expr operators-stack expr)
    
    (define (loop-precedence operator reversed-polish-expr operators-stack)
      (if (operator-precedes? operator (first-char operators-stack))
          ; ako operatora e s po-golqm prioritet ot pyrviya v steka, izvikai loop sus sledvashtoto i, kato v obratniya polski zapis append-nesh zapetaya i v steka vkarash operatora
          (loop (+ i 1)
                ; vajna e posledovatelnostta argumentite na string-append
                (string-append reversed-polish-expr ",")
                (string-append (string operator) operators-stack)
                expr)
          ; ako operatora ne e s po-golqm prioritet, izvikai loop-precedence kato dobavqm v obratniya polski zapis purviya operator ot steka i go maham ot samiya stek
          (loop-precedence operator
                           (string-append reversed-polish-expr (string (first-char operators-stack)))
                           (remove-first-char operators-stack))))
    
    ; ako sme stignali kraya, vurni append-vaneto na obratniya polski zapis i operaciite
    (if (= i (string-length expr))
        (string-append reversed-polish-expr operators-stack)
        ; v protiven sluchai
        ; ako i e chislo, izvikai loop sus sledvashtoto i kato si append-vam v sushtoto vreme i-to kum obratniya polski zapis
        (if (char-numeric? (string-ref expr i))
            (loop (+ i 1)
                  (string-append reversed-polish-expr (string (string-ref expr i)))
                  operators-stack
                  expr)
            ; ako i ne e chislo, izvikai loop-precedence
            (loop-precedence (string-ref expr i) reversed-polish-expr operators-stack))))
  
  (if (expr-valid? expr)
      (loop 0 "" "" (remove-whitespace-and-leading-0 expr))
      #f))

;(expr-eval expr)  ; Връща стойността на израза expr. Например:
                   ; (expr-eval "10+20*30") → 610
                   ; (expr-eval "") → 0
                   ; Ако expr е невалиден израз, да се върне #f.

(define (expr-eval expr)
  
  (define (eval-op op-char op-to-exec expr)
    
    (define (char-part-of-number? c)
      (if (or (char-numeric? c)
              (and (char=? op-char #\+) (char=? c #\/))
              (and (char=? op-char #\-) (char=? c #\/))
              (and (char=? op-char #\*) (char=? c #\/))) #t #f))
    
    ;purvonachalno izvikva loop s false, "", 0, "", ""
    (define (loop op? new-expr i left-number right-number)
      
      (define (extract-number i number)
        (if (or (= i (string-length expr)) (not (char-part-of-number? (string-ref expr i))))
            (if (= 0 (string-length left-number))
                (loop op? new-expr i number right-number)
                (if (= 0 (string-length right-number))
                    (loop op? new-expr i left-number number)
                    (loop op? new-expr i right-number number)))
            (extract-number (+ i 1) (string-append number (string (string-ref expr i))))))
      
      ;body of loop
      (if (< i (string-length expr))
          (if (char-part-of-number? (string-ref expr i))
              (extract-number i "")
              (if (char=? (string-ref expr i) op-char)
                  (if op?
                      (loop #t new-expr (+ i 1)
                            (number->string (op-to-exec (string->number left-number) (string->number right-number))) "")
                      (loop #t new-expr (+ i 1) left-number right-number))
                  (if op?
                      (loop #f
                            (string-append new-expr
                                           (number->string (op-to-exec (string->number left-number) (string->number right-number)))
                                           (string (string-ref expr i))) 
                            (+ i 1) "" "")
                      (loop #f
                            (string-append new-expr left-number (string (string-ref expr i)))
                            (+ i 1) right-number ""))))
          ; ako sme stignali kraya
          (if op?
              (string-append new-expr (number->string (op-to-exec (string->number left-number) (string->number right-number))))
              (string-append new-expr left-number))))
    
    ;body of eval-op
    (loop #f "" 0 "" ""))
  
  ;body of expr-eval
  (if (expr-valid? expr)
      (if (= 0 (string-length (remove-whitespace-and-leading-0 expr)))
          0
          (string->number (eval-op #\- -
                                   (eval-op #\+ +
                                            (eval-op #\* *
                                                     (eval-op #\/ /
                                                              (eval-op #\^ expt (remove-whitespace-and-leading-0 expr))))))))
      #f))

; Examples
(remove-whitespace-and-leading-0 "   012 +050+   0120")
(remove-whitespace-and-leading-0 "0 +0  -0")
(remove-whitespace-and-leading-0 "10   + 20")

(expr-valid? "0 +0  -0")
(expr-valid? "+2+3+4+5")
(expr-valid? "2+3+4+  05")
(expr-valid? "20/2002")
(expr-valid? "20/0*5")

(expr-rp "10   + 20 -6^7*24/34325-0")
(expr-rp "10^2+200-25/5/5")
(expr-rp "3+4*2/4^2^3")
(expr-rp "10+20*30^40+50*60-70+80*90^100+110")
(expr-rp "1+6^34545^4*3/5-23/5*6+80^0")
(expr-rp "1^6^5^4")

(expr-eval " 63/2/3 - 4*3+ 2^2^3^2  -8  ")
(expr-eval " 63/2/3 - 5 - 1/2 - 3^2 -5")
(expr-eval "10^2+200-25/5/5")
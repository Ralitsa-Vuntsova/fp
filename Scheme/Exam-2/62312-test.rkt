;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 2
; 2020-12-12
;
; Име: Ралица Альошева Вунцова
; ФН: 62312
; Специалност: Софтуерно инженерство
; Курс: 3
; Административна група: 4
; Начален час на контролното: 9:45
;

#lang racket/base

(require "62312.rkt") 

(require rackunit rackunit/gui)

(test/gui

 (test-suite
  "height works correctly"

  (test-equal? "Height should be 0" (height '()) 0)
  (test-equal? "Height should be 1" (height '(2 () ())) 1)
  (test-equal? "Height should be 2" (height '(2 (3 () ()) ())) 2)
  (test-equal? "Height should be 2" (height '(2 (3 () ()) (4 () ()))) 2)
  (test-equal? "Height should be 3" (height '(2 (3 () ()) (4 (5 () ()) ()))) 3)
  )
 
 (test-suite
  "weight-balanced? validation"

  (test-true   "It should be true" (weight-balanced? '()))
  (test-true   "It should be true" (weight-balanced? '(2 () ())))
  (test-true   "It should be true" (weight-balanced? '(1 (2 () ()) (3 () ()))))
  (test-true   "It should be true" (weight-balanced? '(1 (2 (4 () ()) (5 () ())) (3 () ()))))
  (test-true   "It should be true" (weight-balanced? '(1 (2 (4 () ()) ()) (3 () (5 () ())))))
  
  (test-false  "It should be false" (weight-balanced? '(1 (2 (4 (6 () ()) (7 () ())) (5 () ())) (3 () ()))))
  (test-false  "It should be false" (weight-balanced? '(1 (2 (4 (6 () ()) (7 () ())) (5 (6 () ()) ())) (3 () ()))))
 )

 ;filter
 ;map
 ;member
 ;zip
 ;uniques
 ;faculty-numbers
 ;number-of-exams

 (test-suite
  "attemps works correctly"

  (test-equal? "attemps should return two pairs"
               (attempts "a" (list (list 12345 "a" 5)
                                   (list 12345 "b" 6)
                                   (list 12345 "b" 6)
                                   (list 54321 "a" 5)
                                   (list 54321 "b" 6)
                                   (list 54321 "a" 5)
                                   (list 54321 "b" 6)
                                   (list 54321 "c" 6)))
               (list (cons 12345 1) (cons 54321 2)))

  (test-equal? "attempts should return the empty list"
               (attempts "d" (list (list 12345 "a" 5)
                                   (list 12345 "b" 6)
                                   (list 54321 "b" 6)))
               '())
  )
)
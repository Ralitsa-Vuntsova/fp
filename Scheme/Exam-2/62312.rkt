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

(provide (all-defined-out))

;Задача 1
(define (height tree)
  (if (null? tree)
      0
      (+ 1
         (max (height (cadr tree))
              (height (caddr tree))))))

(define (weight-balanced? tree)
  (cond
    ((null? tree) #t)
    ((>= (abs (- (height (cadr tree))
                (height (caddr tree))))
        2) #f)
    (else (and (weight-balanced? (cadr tree))
               (weight-balanced? (caddr tree)))))
  )

;Задача 2

(define (filter p? lst)
  (cond
    ((null? lst) '())
    ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
    (else (filter p? (cdr lst)))))

(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map f (cdr lst)))))

(define (member x lst)
  (cond
    ((null? lst) #f)
    ((equal? x (car lst)) #t)
    (else (member x (cdr lst)))))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (uniques lst)
  (if
    (null? lst)
    '()
    (let ((rest (uniques (cdr lst))))
      (if (member (car lst) rest)
          rest
          (cons (car lst) rest)))))

(define (faculty-numbers lst)
  (map car lst)
  )

(define (number-of-exams lst subj res)
  (map (lambda (fn) (length (filter (lambda (x) (and (equal? (cadr x) subj) (equal? (car x) fn))) res))) lst)
  )

(define (attempts subj res)
  (define unique-fns-subj (uniques (faculty-numbers (filter (lambda (x) (equal? (cadr x) subj)) res))))
  (if
   (null? unique-fns-subj) '()
   (zip unique-fns-subj (number-of-exams unique-fns-subj subj res))))
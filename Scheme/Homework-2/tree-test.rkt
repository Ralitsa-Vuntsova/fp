#lang racket/base

(require rackunit rackunit/gui racket/include)

(include "tree.rkt")

(define (expect-correct-string str)
  (test-true
    (string-append "String '" str "' should be correct")
    (tree? str)
  )
)

(define (expect-incorrect-string str)
  (test-false
    (string-append "String '" str "' should be incorrect")
    (tree? str)
  )
)

(define (expect-correct-balanced tree)
  (test-true
    (string-append "The tree should be balanced")
    (balanced? tree)
  )
)

(define (expect-incorrect-balanced tree)
  (test-false
    (string-append "The tree should not be balanced")
    (balanced? tree)
  )
)

(define (expect-correct-order tree)
  (test-true
    (string-append "The tree should be ordered")
    (order? tree)
  )
)

(define (expect-incorrect-order tree)
  (test-false
    (string-append "The tree should not be ordered")
    (order? tree)
  )
)

(test/gui

 (test-suite
  "remove-whitespace works correctly"

  (test-equal?
   "Strings without whitespace are left intact"
   (remove-whitespace "proba88proba")
   "proba88proba")

  (test-equal?
   "Whitespace is removed"
   (remove-whitespace " \t  proba88 \t proba \t  ")
   "proba88proba")

  (test-equal?
  "An all-whitespace string is processed correctly"
  (remove-whitespace "  \n \t  ")
  "")
 )

 (test-suite
  "number-of-symbols works correctly"

  (test-equal?
   "The number of the given symbol (character) is correct"
   (number-of-symbols #\a "proba88proba")
   2)

  (test-equal?
   "The number of the given symbol (digit) is correct"
   (number-of-symbols #\9 "**9FDSG{{}gdsg}*")
   1)
 
  (test-equal?
   "The number of the given symbol (space) is correct"
   (number-of-symbols #\space "  2345  56  5")
   6)
 )

 (test-suite
  "is-there-whitespace-between-digits? works correctly"

  (test-false
   "There is no space in the string between the digits"
   (is-there-whitespace-between-digits? "555555"))

  (test-true
   "There is space in the middle of the string between the digits"
   (is-there-whitespace-between-digits? "555 555"))
 
  (test-true
   "There is space at the beginning of the string between the digits"
   (is-there-whitespace-between-digits? "5 55555"))

  (test-true
   "There is more than one space between the digits"
   (is-there-whitespace-between-digits? "555    555"))

  (test-false
   "There is no space between the digits"
   (is-there-whitespace-between-digits? "{5*{    55555"))
 )

  (test-suite
  "String validation"
  (expect-correct-string   "{2{4**}*}")
  (expect-correct-string   "{2*{4**}}")
  (expect-correct-string   "{1  * {3**}  }")
  (expect-correct-string   "{1{3**}*}")
  (expect-correct-string   "{5{22{2**}{6**}}{1*{3{111**}*}}}")
  (expect-correct-string   "{5*{  22{2**} {6* *}  }}")
  (expect-correct-string   "{5{22{2**}{6**}}{1*{3{111{4**}*}*}}}")
  (expect-correct-string   "{5{22{2**}{6**}}{1*{3{111{4{5**}*}*}*}}}")
  (expect-correct-string   "*")
  (expect-correct-string   "{2 {4 {5**}*} *}")
  (expect-correct-string   "{2 {4*  *} {5**}}")
  (expect-correct-string   "{12345 {3 {58 * {4 * *}} *} {12 * *}}")
  (expect-correct-string   "{1    {  7 {12* {1*   *}  }*} { 55 **} }")
  (expect-incorrect-string "{2{4***}*}")
  (expect-incorrect-string "{{4**}*}")
  (expect-incorrect-string "{2{4**}}")
  (expect-incorrect-string "{1{3a*}*}")
  (expect-incorrect-string "{1{{3**}}*}")
  (expect-incorrect-string "{5*{  22{{2**} {6* *}  }}")
  (expect-incorrect-string "{5{22{2**}{6**}}{1*{3{111{4**}}*}*}}}")
  (expect-incorrect-string "{*}")
  (expect-incorrect-string "{* * *}")
  (expect-incorrect-string "* * *")
  (expect-incorrect-string "{* * * *}")
  (expect-incorrect-string "{}")
  (expect-incorrect-string "}{")
  (expect-incorrect-string "{}{}{}{}")
  (expect-incorrect-string "{{{}}}")
  (expect-incorrect-string "{2 25{4 * *} *}")
  (expect-incorrect-string "}2 }4 * *{ *{")
  (expect-incorrect-string "{2 {4 {8**}{7}} *}")
 )

 (test-suite
  "Balanced tree validation"
  (expect-correct-balanced   '())
  (expect-correct-balanced   (list 2 (list 4 '() '()) '()))
  (expect-correct-balanced   (list 2 '() (list 4 '() '())))
  (expect-correct-balanced   (list 6 (list 10 (list 21 '() '()) '()) (list 15 '() '())))
  (expect-correct-balanced   (list 1 (list 2 (list 4 (list 7 '() '()) '()) (list 5 '() '())) (list 3 (list 6 '() '()) '())))
  (expect-incorrect-balanced (list 5 (list 22 (list 2 '() '()) (list 6 '() '())) (list 1 '() (list 3 (list 111 '() '()) '()))))
  (expect-incorrect-balanced (list 1 (list 2 (list 4 (list 8 '() '()) '()) (list 5 '() '())) (list 3 '() '())))
  (expect-incorrect-balanced (list 8 (list 7 (list 1 '() '()) '()) (list 3 '() (list 15 (list 1000 '() '()) '()))))
  (expect-incorrect-balanced (list 5 (list 1 (list 8 '() (list 22 '() '())) '()) (list 17 '() '())))
  (expect-incorrect-balanced (list 25 (list 20 (list 10 '() (list 12 '() '())) (list 22 '() '())) (list 36 '() '())))
 )

 (test-suite
  "Order tree validation"
  (expect-correct-order   '())
  (expect-correct-order   (list 2 '() '()))
  (expect-correct-order   (list 8 (list 3 (list 1 '() '()) (list 6 (list 4 '() '()) (list 7 '() '()))) (list 10 '() (list 14 (list 13 '() '()) '()))))
  (expect-correct-order   (list 25
                                (list 20 (list 10 (list 5 '() '()) (list 12 '() '())) (list 22 '() '()))
                                (list 36 (list 30 (list 28 '() '()) '()) (list 40 (list 38 '() '()) (list 48 '() '())))))
  (expect-incorrect-order (list 8 (list 3 (list 1 '() '()) (list 6 (list 4 '() '()) (list 7 '() '()))) (list 10 '() (list 14 (list 15 '() '()) '()))))
  (expect-incorrect-order (list 8 (list 3 (list 1 '() '()) (list 6 (list 4 '() '()) (list 7 '() '()))) (list 7 '() (list 14 (list 13 '() '()) '()))))
  (expect-incorrect-order (list 25
                                (list 20 (list 10 (list 5 '() '()) (list 12 '() '())) (list 5 '() '()))
                                (list 36 (list 30 (list 28 '() '()) '()) (list 40 (list 38 '() '()) (list 48 '() '())))))
  (expect-incorrect-order (list 19
                                (list 20 (list 10 (list 5 '() '()) (list 12 '() '())) (list 22 '() '()))
                                (list 36 (list 30 (list 28 '() '()) '()) (list 40 (list 38 '() '()) (list 48 '() '())))))
 )

 (test-suite
  "From tree to string validation"

  (test-equal? "The empty tree should be *"
               (tree->string '()) "*")
  (test-equal? ""
               (tree->string (list 2 (list 4 '() '()) '())) "{2 {4 * *} *}")
  (test-equal? ""
               (tree->string (list 2 '() (list 4 '() '()))) "{2 * {4 * *}}")
  (test-equal? ""
               (tree->string (list 6 (list 10 (list 21 '() '()) '()) (list 15 '() '()))) "{6 {10 {21 * *} *} {15 * *}}")
  (test-equal? ""
               (tree->string (list 1 (list 2 (list 4 (list 7 '() '()) '()) (list 5 '() '())) (list 3 (list 6 '() '()) '())))
               "{1 {2 {4 {7 * *} *} {5 * *}} {3 {6 * *} *}}")
  )
  
)
;Процедура (enumerate-interval from to), която връща списък от целите числа в интервала [from, to],
;подредени в нарастващ ред от from до to, където from и to са цели числа.

(define (enumerate-interval from to)
  (if (> from to) '()
      (cons from (enumerate-interval (+ from 1) to))))
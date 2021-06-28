;Дефинирайте процедура (flip fn), която обръща реда на аргументите на процедурата fn независимо от броя им.

(define (flip fn)
  (lambda args
    (apply fn (reverse args))))
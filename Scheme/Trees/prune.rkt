(load "./basics.rkt")

; Да се напише функция (prune t), която премахва всички листа в дървото t.

(define (prune t)
  (cond ((leaf? t) empty-tree) ; няма наследници (листо)
        ((empty-tree? (right-tree t)) ; само ляв наследник
         (make-tree (root-tree t)
                    (prune (left-tree t))
                    empty-tree))
        ((empty-tree? (left-tree t)) ; само десен наследник
         (make-tree (root-tree t)
                    empty-tree
                    (prune (right-tree t))))
        (else (make-tree (root-tree t) ; два наследника
                         (prune (left-tree t))
                         (prune (right-tree t))))))
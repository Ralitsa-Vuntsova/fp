(define sample ; klucha moje da e proizvodel, stoinostta moje da e s proizvolen broi
  ; moje da ima kluch koito da ne e svurzan sus stoinost
  (list
   (cons 'title "The Mythical Man-Month") ; moje i da e list
   (cons 'author "Frederick Brooks")
   (cons 'isbn "0-201-00650-2")
   (cons 'publisher "Addison-Wesley")
   (cons 'year 1975)))

(assoc 'title sample)

; dobavyane na kluch i stoinost chrez skrivane na drugiya (ako ima veche sushtiya kluch v spisyka)
(define (update-entry key value L)
  (cons (cons key value) L))

; dobavyane na kluch i stoinost chrez iztrivane na drugiya (ako ima veche sushtiya kluch v spisyka)
(define (update-entry-with-replacement key value L)
  (cond
    ((null? L) (list (cons key value)))
    ((equal? key (caar L)) (cons (cons key value)
                                 (cdr L)))
    (else (cons (car L)
                (update-entry-with-replacement key value (cdr L))))))

; po dadeni kluchove, napravi asociativen spisyk, v koito stoinostite sa f ot kluchovete
(define (make-alist f keys)
  (map (lambda (key)
         (cons key (f key)))
       keys))

(define (keys alist)
  (map car alist))

(define (values alist)
  (map cdr alist))

(define (delete-key key alist)
  (filter (lambda (kv)
            (not (equal? (car kv) key)))
          alist))
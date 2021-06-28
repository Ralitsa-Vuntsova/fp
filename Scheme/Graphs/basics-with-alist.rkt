(define empty-graph '())

(define (make-alist f keys)
  (map (lambda (key)
         (cons key (f key)))
       keys))

(define (make-graph vs)
  (make-alist (lambda (_) '())
              vs))

(define (keys alist)
  (map car alist))

(define vertices keys)

(define (children v g)
  (cdr (assoc v g)))

(define (edge? u v g)
  (member v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (any? p l)
  (and (not (null? l))
       (or (p (car l))
           (any? p (cdr l)))))

(define (search-child v p g)
  (any? p (children v g)))

(define (update-entry key value L)
  (cons (cons key value) L))

(define (add-vertex v g)
  (if (assoc v g)
      g
      (update-entry v '() g)))

(define (add-if-missing x l)
  (if (member x l)
      l
      (cons x l)))

(define (add-edge u v g)
  (let ((g-with-u-v (add-vertex v (add-vertex u g))))
    (update-entry u
                 (add-if-missing v (children u g-with-u-v))
                  g-with-u-v)))

(define (remove-edge u v g)
  (update-entry u
                (filter (lambda (u)
                       (not (equal? u v)))
                     (children u g))
                g))
(define x (list (list 1 2) (list 3 4)))

(define (npair? l)
  (not (pair? l)))

(define (tolist v)
  (if (null? v)
      v
      (list v)))

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(print (fringe x))
(print (fringe (list x (list x x))))

(define (equal? l1 l2)
  (cond ((eq? l1 l2) #t)
        ((or (not (pair? l1)) (not(pair? l2))) #f)
        (else (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))))

(print (equal? '(this is a list) '(this is a list)))
(print (equal? '(this is a list) '(this (is a) list)))
(print (equal? '(this (is a) list) '(this (is a) list)))

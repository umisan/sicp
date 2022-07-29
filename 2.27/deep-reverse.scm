(define x (list (list 1 2) (list 3 4)))

(define (reverse l)
  (define (reverse-itr now result)
    (if (null? (cdr now))
        (cons (car now) result)
        (reverse-itr (cdr now) (cons (car now) result))))
  (reverse-itr (cdr l) (cons (car l) '())))

(print (reverse x))


(define (deep-reverse l)
  (define (d-itr sl ans)
    (cond ((null? sl) ans)
          ((not (pair? (car sl))) (d-itr (cdr sl) (cons (car sl) ans)))
          (else (d-itr (cdr sl) (cons (deep-reverse (car sl)) ans)))))
  (d-itr l '()))

(print (deep-reverse (list 1 2 3 4)))
(print (deep-reverse x))
(print (deep-reverse (list 1 (list 3 4))))
(print (deep-reverse (list 1 (list 3 4) (list 5 6))))
(print (deep-reverse (list 1 (list 3 4 (list 5 6)))))

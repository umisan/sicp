(define (retain l p)
  (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (retain (cdr l) p))
          (retain (cdr l) p))))

(print (retain (list 1 2 3 4) (lambda(x) (= (remainder x 2) 0))))
(print (retain (list 1 2 3 4) (lambda(x) (= (remainder x 2) 1))))

(define (same-parity x . l)
  (cons x (retain l (lambda (y) (= (remainder x 2) (remainder y 2))))))

(print (same-parity 1 2 3 4 5 6 7))
(print (same-parity 2 3 4 5 6 7))


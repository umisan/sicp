(define (double f)
  (lambda (x)
    (f (f x))))

(print ((double (lambda (x) (+ x 1))) 1))

(define (inc x) (+ x 1))

(print (((double (double double)) inc) 5))

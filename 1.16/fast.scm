(define (even n) (= (remainder n 2) 0))
(define (fast-expr b n)
  (define (fast-expr-itr x y)
    (if (= y 1)
        x
        (fast-expr-itr (* x x) (/ y 2))))
  (cond ((= n 0) 1)
        ((even n) (fast-expr-itr b n))
        (else (* b (fast-expr-itr b (- n 1))))))

(print (fast-expr 2 0))
(print (fast-expr 2 4))
(print (fast-expr 2 5))

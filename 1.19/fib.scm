(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter
           a
           b
           (update_p p q)
           (update_q p q)
           (/ count 2)))
         (else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))

(define (double x) (* x x))

(define (update_p p q) (+ (double q) (double p)))

(define (update_q p q) (+ (* 2 q p) (double q)))

(print (fib 4))
(print (fib 0))
(print (fib 1))
(print (fib 2))
(print (fib 3))
(print (fib 10))

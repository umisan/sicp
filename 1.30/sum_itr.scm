(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add_dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add_dx b)
     dx))


(print (integral cube 0 1 0.01))


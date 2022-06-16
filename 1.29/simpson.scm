(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add_dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add_dx b)
     dx))

(print (integral cube 0 1 0.01))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y_k k) 
    (cond ((= k 0) (f a))
           ((even? k) (* 2 (f (+ a (* k h)))))
           (else (* 4 (f (+ a (* k h)))))))
  (define (inc x) (+ x 1))
  (* (/ h 3.0) (sum y_k 0 inc n)))

(print (simpson cube 0 1 100.0))
(print (simpson cube 0 1 1000.0))

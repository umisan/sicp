(define (product f start next end)
  (if (> start end)
      1
      (* (f start) (product f (next start) next end))))

(define (product-itr f start next end)
  (define (itr a result)
    (if (> a end)
        result
        (itr (next a) (* result (f a)))))
  (itr start 1))


(define (factorial n)
  (define (nop x) x)
  (define (inc x) (+ x 1))
  (product nop 1 inc n))

(define (factorial-itr n)
  (define (nop x) x)
  (define (inc x) (+ x 1))
  (product-itr nop 1 inc n))


(print (factorial 3))
(print (factorial-itr 3))
(print (factorial 4))
(print (factorial-itr 4))

(define (pi n)
  (define (inc x) (+ x 1))
  (define (f1 x) 
    (+ 2 (* 2 (floor (/ x 2.0)))))
  (define (f2 x)
    (if (= x 1)
        3
        (+ 3 (* 2 (floor (/ (- x 1) 2.0))))))
  (/ (product f1 1 inc n) (product f2 1 inc n)))

(print (pi 1))
(print (pi 3))
(print (pi 5))
(print (pi 10))
(print (pi 100))

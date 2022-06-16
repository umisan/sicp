(define (cube x) (* x x x))

(define (p x) (- (* x 3) (* 4 (cube x))))

(define (sine x)
  (if (not (> (abs x) 0.1))
      x
      (p (sine (/ x 3.0)))))

(print (sine 12.15))

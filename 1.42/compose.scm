(define (compose f g)
  (lambda (x)
    (f (g x))))

(print ((compose square (lambda (x) (+ x 1))) 6))

(define (average x y)
  (/ (+ x y) 2.0))

(define (iterative-improve check calc-next)
  (define (itr guess)
    (if (check guess)
        guess
        (itr (calc-next guess))))
  (lambda (x) (itr x)))

(define (sqrt x)
  ((iterative-improve
    (lambda (y) (< (abs (- (square y) x)) 0.001))
    (lambda (y) (average y (/ x y)))) 1.0))

(print (sqrt 4))
(print (sqrt 9))

(define (fixed-point f first-guess)
  ((iterative-improve
     (lambda (guess) (< (abs (- guess (f guess))) 0.00001))
     (lambda (guess) (f guess))) first-guess))

(print (fixed-point cos 1.0))

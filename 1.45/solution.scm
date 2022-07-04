(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average v1 v2)
  (/ (+ v1 v2) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (repeat-one f cnt)
    (if (= cnt n)
        f
        (repeat-one (compose f f) (+ cnt 1))))
  (repeat-one f 1))

(define (4-rt x)
  (fixed-point-of-transform (lambda (y) (/ x (* y y y)))
                            (repeated average-damp 2)
                            1.0))

(define (power x n)
  (define (power-iter v cnt)
    (if (= cnt n)
        v
        (power-iter (* v x) (+ cnt 1))))
  (power-iter x 1))

(define (n-rt x n repeat)
  (fixed-point-of-transform (lambda (y) (/ x (power y (- n 1))))
                            (repeated average-damp repeat)
                            1.0))


(print (4-rt 10))
(print (n-rt 10 5 2))
(print (n-rt 10 6 2))
(print (n-rt 10 7 2))
(print (n-rt 10 10 3))


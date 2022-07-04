(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (repeat-one f cnt)
    (if (= cnt n)
        f
        (repeat-one (compose f f) (+ cnt 1))))
  (repeat-one f 1))

(define (n-smooth n)
  (repeated smooth n))

(print (((n-smooth 3) (lambda (x) x)) 3))

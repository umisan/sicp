(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (repeat-one f cnt)
    (if (= cnt n)
        f
        (repeat-one (compose f f) (+ cnt 1))))
  (repeat-one f 1))

(print ((repeated square 2) 5))

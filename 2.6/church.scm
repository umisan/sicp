(define zero (lambda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (to_int x) (+ x 1))

(print ((one to_int) 0))
(print ((two to_int) 0))

(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(print (((+ one two) to_int) 0))

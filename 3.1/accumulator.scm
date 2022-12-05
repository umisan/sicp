(define (make-accumulator sum)
  (define (add-sum value)
    (begin (set! sum (+ sum value))
           sum))
  add-sum)

(define A (make-accumulator 5))
(define B (make-accumulator 10))

(print (A 10))

(print (A 10))

(print (B 10))

(print (B 10))

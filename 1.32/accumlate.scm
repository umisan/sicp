(define (accumlate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumlate combiner null-value term (next a) next b))))

(define (accumlate-itr combiner null-value term a next b)
  (define (itr a result)
    (if (> a b)
        result
        (itr (next a) (combiner result (term a)))))
  (itr a null-value))


(define (sum term a next b)
  (define (combiner x y) (+ x y))
  (accumlate combiner 0 term a next b))

(define (sum-itr term a next b)
  (define (combiner x y) (+ x y))
  (accumlate-itr combiner 0 term a next b))


(define (nop x) x)
(define (inc x) (+ x 1))
(print (sum nop 1 inc 10))
(print (sum-itr nop 1 inc 10))

(define (factorial n)
  (define (combiner x y) (* x y))
  (accumlate combiner 1 nop 1 inc n))

(define (factorial-itr n)
  (define (combiner x y) (* x y))
  (accumlate-itr combiner 1 nop 1 inc n))


(print (factorial 3))
(print (factorial 4))
(print (factorial-itr 3))
(print (factorial-itr 4))

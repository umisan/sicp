(define (accumlate combiner filter null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a) (accumlate combiner filter null-value term (next a) next b)))
        (else (combiner null-value (accumlate combiner filter null-value term (next a) next b)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n) (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (plus-combiner a b) (+ a b))
(define (multi-combiner a b) (* a b))
(define (nop x) x)
(define (inc x) (+ x 1))

(print (accumlate plus-combiner prime? 0 nop 1 inc 10))

(define (produc-coprime n)
  (define (check-coprime x) (= (gcd x n) 1))
  (accumlate multi-combiner check-coprime 1 nop 1 inc n))

(print (produc-coprime 10))

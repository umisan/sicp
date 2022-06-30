(use math.const)
(define (n x i)
  (if (= i 1)
      x
      (* x x)))

(define (d i)
  (if (= i 1)
      1.0
      (+ 1.0 (* 2.0 (- i 1)))))

(define (tan-cf x k)
  (define (tan-cf-calc i)
    (if (= i k)
        (/ (n x i) (d i))
        (/ (n x i) (- (d i) (tan-cf-calc (+ i 1))))))
  (tan-cf-calc 1))

(print (tan-cf (/ pi 3) 10))
(print (tan-cf (/ pi 3) 1000))

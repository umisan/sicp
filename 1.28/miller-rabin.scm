(use data.random)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check (expmod base (/ exp 2) m) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))


(define (check x m)
  (if (or (= x 1) (= x (- m 1)))
      1
      (if (= (remainder (square x) m) 1)
          0
          (remainder (square x) m))))



(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 ((integers$ (- n 1))))))

(print (miller-rabin-test 4))
(print (miller-rabin-test 5))
(print (miller-rabin-test 561))
(print (miller-rabin-test 1105))

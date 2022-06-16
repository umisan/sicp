(use data.random)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 ((integers$ (- n 1))))))

(print (fermat-test 561))

(define (try-it a n)
  (= (expmod a n n) a))

(define (test-all start end ret)
  (cond ((= start (- end 1)) ret)
        (else (test-all (+ start 1) end (and ret (try-it start end))))))

(print (test-all 1 561 #t))
(print (test-all 1 1105 #t))
(print (test-all 1 1729 #t))
(print (test-all 1 10 #t))
(print (test-all 1 11 #t))
(print (test-all 1 7 #t))
(print (test-all 1 4 #t))

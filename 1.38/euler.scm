(define (cont-frac n d k) 
  (define (calc i) 
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (calc (+ i 1))))))
  (calc 1))

(define (d i)
  (let ((j (- i 1)))
    (if (= j 0)
        1
        (+ 2 (* 2 (floor (/ j 3.0)))))))

(print (+ 2 (cont-frac (lambda (i) 1.0) d 10)))
(print (+ 2 (cont-frac (lambda (i) 1.0) d 100)))


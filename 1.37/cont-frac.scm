(define (cont-frac n d k) 
  (define (calc i) 
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (calc (+ i 1))))))
  (calc 1))

(define (cont-frac-itr n d k)
  (define (calc i before)
    (cond ((= i k) (calc (- i 1) (/ (n i) (d i))))
          ((= i 1) (/ (n i) (+ (d i) before)))
          (else (calc (- i 1) (/ (n i) (+ (d i) before))))))
  (calc k 1))


(print (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10))
(print (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))
(print (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1000))
(print (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10000))

(print (cont-frac-itr (lambda (i) 1.0) (lambda (i) 1.0) 10))
(print (cont-frac-itr (lambda (i) 1.0) (lambda (i) 1.0) 100))
(print (cont-frac-itr (lambda (i) 1.0) (lambda (i) 1.0) 1000))
(print (cont-frac-itr (lambda (i) 1.0) (lambda (i) 1.0) 10000))

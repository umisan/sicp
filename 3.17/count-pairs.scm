(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (count-pairs-2 x)
  (let ((counted '()))
    (define (count x)
      (cond ((not (pair? x)) 0)
            ((memq x counted) 0)
            (else (begin
                    (set! counted (cons x counted))
                    (+ (count (car x))
                       (count (cdr x))
                       1)))))
    (count x)))


(define p1 (cons 1 1))
(define p2 (cons 2 p1))
(define p3 (cons p1 p2))
(print (count-pairs (cons (cons 1 1) (cons 1 1))))
(print (count-pairs p3))

(print (count-pairs (cons (cons 1 1) (cons 1 1))))
(print (count-pairs-2 p3))

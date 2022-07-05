(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment x y) (cons x y))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (average x y)
  (/ (+ x y) 2.0))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point 
      (average (x-point start) (x-point end))
      (average (y-point start) (y-point end)))))

(print-point (midpoint-segment (make-segment
                                 (make-point 10 10)
                                 (make-point 20 30))))

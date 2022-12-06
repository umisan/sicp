(use data.random)
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low ((integers$ range)))))

(define (area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (estimate-integral p x1 x2 y1 y2 count)
  (* (area x1 x2 y1 y2) (monte-carlo 
                          count 
                          (lambda ()
                            (let ((random-x (random-in-range x1 x2))
                                  (random-y (random-in-range y1 y2)))
                              (begin 
                                (p random-x random-y)))))))

(define circle-p (lambda (x y) (<= (+ 
                                     (expt (- x 5.0) 2)
                                     (expt (- y 7.0) 2))
                                   9.0)))

(print (/ (estimate-integral circle-p 2 8 4 10 100000.0) 9.0))


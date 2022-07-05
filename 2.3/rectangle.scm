(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment x y) (cons x y))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-rectangle left top) (cons left top))
(define (left rectangle) 
  (abs (
        - 
        (y-point (start-segment (car rectangle)))
        (y-point (end-segment (car rectangle))))))

(define (top rectangle)
  (abs (
        -
        (x-point (start-segment (cdr rectangle)))
        (x-point (end-segment (cdr rectangle))))))

(define (make-rectangle2 left-top right-bottom) (cons left-top right-bottom))
(define (left2 rectangle)
  (abs (- 
         (x-point (car rectangle)) 
         (x-point (cdr rectangle)))))

(define (top2 rectangle)
  (abs (-
         (y-point (car rectangle))
         (y-point (cdr rectangle)))))

(define (perimeter rectangle)
  (+ (* 2 (left rectangle)) (* 2 (top rectangle))))

(define (area rectangle)
  (* (left rectangle) (top rectangle)))

(define (perimeter2 rectangle)
  (+ (* 2 (left2 rectangle)) (* 2 (top2 rectangle))))

(define (area2 rectangle)
  (* (left2 rectangle) (top2 rectangle)))



(print (perimeter (make-rectangle
                    (make-segment 
                      (make-point 0 0)
                      (make-point 0 10))
                    (make-segment
                      (make-point 0 10)
                      (make-point 20 10)))))

(print (area (make-rectangle
                    (make-segment 
                      (make-point 0 0)
                      (make-point 0 10))
                    (make-segment
                      (make-point 0 10)
                      (make-point 20 10)))))

(print (perimeter2 (make-rectangle2 (make-point 0 10) (make-point 20 0))))

(print (area2 (make-rectangle2 (make-point 0 10) (make-point 20 0))))

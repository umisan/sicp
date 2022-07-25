(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (<= (lower-bound y) 0) (<= (upper-bound y) 0))
      (error "span 0")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))


(define (lower-bound z) (min (car z) (cdr z)))

(define (upper-bound z) (max (car z) (cdr z)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((unit (* c p)))
    (make-interval (- c unit) (+ c unit))))

(define (percent z)
  (/ (width z) (center z)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(print (par1
         (make-interval 10 20)
         (make-interval 30 40)))

(print (par2
         (make-interval 10 20)
         (make-interval 30 40)))

(print (div-interval (make-center-percent 100 0.001) (make-center-percent 100 0.001)))
(print (div-interval (make-center-percent 100 0.001) (make-center-percent 300 0.001)))


(print (div-interval (make-center-percent 100 0.0001) (make-center-percent 100 0.0001)))
(print (div-interval (make-center-percent 100 0.0001) (make-center-percent 300 0.0001)))

(print (par1
         (make-center-percent 100 0.01)
         (make-center-percent 300 0.01)))

(print (par2
         (make-center-percent 100 0.01)
         (make-center-percent 300 0.01)))


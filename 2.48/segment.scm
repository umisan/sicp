(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2) 
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (make-segment v1 v2)
  (cons v1 (add-vect v1 v2)))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define s (make-segment (make-vect 1 2) (make-vect 3 4)))

(print (start-segment s))

(print (end-segment s))


(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define f1 (make-frame (make-vect 1 2) (make-vect 3 4) (make-vect 5 6)))

(print (origin-frame f1))
(print (edge1-frame f1))
(print (edge2-frame f1))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))

(define f2 (make-frame2 (make-vect 1 2) (make-vect 3 4) (make-vect 5 6)))

(print (origin-frame2 f2))
(print (edge1-frame2 f2))
(print (edge2-frame2 f2))


(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoint-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoint-set x (cdr set))))))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (union-set (cdr set1) set2))
              ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
              (else (cons x2 (union-set set1 (cdr set2))))))))

(print (union-set (list 1 2 3) (list 3 4 5)))
(print (union-set (list 1 2) (list 3 4 5)))


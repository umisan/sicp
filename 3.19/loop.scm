;ちょっとルール違反
(define (detect l)
  (cond ((null? l) #f)
        ((eq? (cdr l) 'l) #t)
        (else (let ((tmp (cdr l)))
                (set-cdr! l 'l)
                (detect tmp)))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(print (detect z))
(print (detect (list 'a 'b 'c)))

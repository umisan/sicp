(define (detect l)
  (let ((viewd '()))
    (define (check l)
      (cond ((null? l) #f)
            ((memq (cdr l) viewd) #t)
            (else (begin
                    (set! viewd (cons (cdr l) viewd))
                    (check (cdr l))))))
    (check l)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(print (detect z))


;(define (make-mobile left right)
;  (list left right))

(define (make-mobile left right)
  (cons left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
;  (car (cdr mobile)))
  (cdr mobile))

(define (make-branch length structure)
;  (list length structure))
  (cons length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
;  (car (cdr branch)))
  (cdr branch))

(define (total-weight mobile)
  (cond ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define (moment branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define mobile1 (make-mobile (make-branch 10 20) (make-branch 10 30)))
(define mobile2 (make-mobile (make-branch 10 mobile1) (make-branch 10 30)))
(define mobile3 (make-mobile (make-branch 10 mobile1) (make-branch 10 mobile2)))
(define mobile4 (make-mobile (make-branch 10 20) (make-branch 10 20)))

(print (total-weight mobile1))
(print (total-weight mobile2))
(print (total-weight mobile3))

(define (balance? mobile)
  (cond ((not (pair? mobile)) #t)
        ((= (moment (left-branch mobile)) (moment (right-branch mobile))) (and (balance? (branch-structure (left-branch mobile)))
                                                                               (balance? (branch-structure (right-branch mobile)))))
        (else #f)))


(print (balance? mobile1))
(print (balance? mobile4))
(print (balance? (make-mobile (make-branch 10 mobile3) (make-branch 10 mobile3))))
(print (balance? (make-mobile (make-branch 10 mobile4) (make-branch 10 mobile4))))

(define (make-account balance pass)
  (let ((pass-list (list pass)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)) 
      balance)
    (define (incorrect-password amount) (print "Incorrect password"))
    (define (add-pass new-pass)
      (set! pass-list (append pass-list (list new-pass))))
    (define (dispatch sec m)
      (cond ((not (meq? sec pass-list)) incorrect-password)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'add-pass) add-pass)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (meq? target l)
  (cond ((null? l) #f)
        ((eq? target (car l)) #t)
        (else (meq? target (cdr l)))))

(define (make-joint account original-pass new-pass)
  (begin 
    ((account original-pass 'add-pass) new-pass)
    account))

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

(print ((paul-acc 'rosebud 'deposit) 40))
(print ((peter-acc 'rosebud 'withdraw) 20))

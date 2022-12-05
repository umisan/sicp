(define (make-account balance pass)
  (let ((retry-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)) 
      balance)
    (define (incorrect-password amount) 
      (if (>= retry-count 6)
          (call-the-cops)
          (begin (set! retry-count (+ retry-count 1))
                 (print "Incorrect password"))))
    (define (call-the-cops) (print "call-the-cops"))
    (define (dispatch sec m)
      (cond ((not (eq? sec pass)) incorrect-password)
            ((eq? m 'withdraw) (begin (set! retry-count 0) withdraw))
            ((eq? m 'deposit) (begin (set! retry-count 0) deposit))
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define acc (make-account 100 'secret-password))
(define acc2 (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

((acc2 'some-other-password 'deposit) 50)
((acc2 'secret-password 'withdraw) 40)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
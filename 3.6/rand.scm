(define random-init 10)
(define (rand-update i) (+ i 1))
(define rand
  (let ((x random-init))
    (define (generate) 
      (set! x (rand-update x))
      x)
    (define (reset new-value)
      (set! x new-value)
      x)
    (define (dispatch m)
      (cond ((eq? m 'generate) generate)
            ((eq? m 'reset) reset)
            (else (error "Invalid message"))))
    dispatch))

(print ((rand 'generate)))
(print ((rand 'reset) 3))
(print ((rand 'generate)))

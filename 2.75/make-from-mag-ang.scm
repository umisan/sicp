(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define i (make-from-mag-ang 1 0))

(define (apply-generic op arg) (arg op))

(print (apply-generic 'magnitude i))
(print (apply-generic 'real-part i))

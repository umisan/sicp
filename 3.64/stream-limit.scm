(define-macro (delay x)
  `(memo-proc (lambda () ,x)))
(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))
(define (force x) (x))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (null? argstreams)
      '()
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (display x)
  (newline))

(define (display-partial-stream s n)
  (if (= n 0)
      'done
      (begin 
        (display-line (stream-car s))
        (display-partial-stream (stream-cdr s) (- n 1)))))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))


(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (divide-streams s1 s2)
  (stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (intergers-starting-from n)
  (cons-stream n (intergers-starting-from (+ n 1))))

(define integers (intergers-starting-from 1))

(define (integrate-series a)
  (divide-streams a integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) 
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-series s)
  (cons-stream (stream-car s)
               (scale-stream 
                 (mul-series (stream-cdr s) (invert-series s))
                 -1)))

(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "invalid")
      (mul-series s1 (invert-series s2))))

(define tan-series (div-series sine-series cosine-series))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2.0))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit s t)
  (let ((v1 (stream-car s))
        (v2 (stream-car (stream-cdr s))))
    (if (< (abs (- v1 v2)) t)
        v2
        (stream-limit (stream-cdr s) t))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(print (sqrt 2 0.1))

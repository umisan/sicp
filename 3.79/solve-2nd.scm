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


(define (alter-stream n)
  (cons-stream (/ 1.0 n)
               (stream-map - (alter-stream (+ n 1)))))

(define (partital-sum s)
  (define tmp (cons-stream (stream-car s) (add-streams tmp (stream-cdr s))))
  tmp)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                            (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (append (list (stream-car s)) x))
                  (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define t (triples integers integers integers))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                  ((> (weight s1car) (weight s2car))
                   (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                  (else
                    (cons-stream s1car
                                 (merge-weighted s1
                                                 (stream-cdr s2)
                                                 weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted 
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (pow x y)
  (+ (* x x )
     (* y y)))

(define ordered-pow-sum 
  (stream-map (lambda (x) (pow (car x) (cadr x)))
              (weighted-pairs integers
                              integers
                              (lambda (x) (pow (car x) (cadr x))))))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(display-partial-stream (solve-2nd (lambda (x y) (+ (* 1 x) (* 2 y))) 0.001 1 1) 10)

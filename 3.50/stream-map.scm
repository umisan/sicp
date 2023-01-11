(define-macro (delay x)
  `(lambda () ,x))
(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))
(define (force x) (x))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define (stream-map proc . argstreams)
  (if (null? argstreams)
      '()
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define a (integers-starting-from 1))
(define b (integers-starting-from 2))
(define c (stream-map + a b))
(print (stream-car c))
(print (stream-car (stream-cdr c)))

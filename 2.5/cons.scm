(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (power base n)
  (define (power-itr i result)
     (if (= i n)
        result
        (power-itr (+ i 1) (* result base))))
  (power-itr 1 base))

(define (divide-cnt x base)
  (define (divide-cnt-itr target cnt)
    (if (divide? target base)
        (divide-cnt-itr (/ target base) (+ cnt 1))
        cnt))
  (divide-cnt-itr x 0))

(define (divide? x y)
  (= (remainder x y) 0))

(define (car z)
  (divide-cnt z 2))


(define (cdr z)
  (divide-cnt z 3))

(print (car (cons 2 3)))
(print (cdr (cons 2 3)))

(print (car (cons 10 3)))
(print (cdr (cons 10 3)))

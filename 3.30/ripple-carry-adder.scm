(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-list b-list s-list c)
  (define (make-c-list c)
    (define (make-c-list-itr result count)
      (if (= count 0)
          result
          (make-c-list-itr (cons (make-wire) result) (- count 1))))
    (cons c (make-c-list-itr '() (length a-list))))
  (define (connect-full-adder a-list b-list s-list c-list)
    (if (null? a-list)
        'ok
        (let ((a (car a-list))
              (b (car b-list))
              (s (car s-list))
              (c-out (car c-list))
              (c-in (cadr c-list)))
          (full-adder a b c-in s c-out)
          (connect-full-adder (cdr a-list) (cdr b-list) (cdr s-list) (cdr c-list)))))
  (let ((c-list (make-c-list c)))
    (connect-full-adder a-list b-list s-list c-list)))


(define (reverse l)
  (define (reverse-itr now result)
    (if (null? (cdr now))
        (cons (car now) result)
        (reverse-itr (cdr now) (cons (car now) result))))
  (reverse-itr (cdr l) (cons (car l) '())))

(print (reverse (list 1 4 9 16 25)))

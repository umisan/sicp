(define (assoc same-key? key records)
  (cond ((null? records) #f)
        ((same-key? key (caar records)) (car records))
        (else (assoc same-key? key (cdr records)))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc same-key? key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc same-key? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unkonwn operation -- TABLE" m))))
    dispatch))

(define table (make-table (lambda (x y) (and (>= x (- y 1)) (<= x (+ y 1))))))

((table 'insert-proc!) 10 20 1)
((table 'insert-proc!) 10 40 2)
(print ((table 'lookup-proc) 10 20))
(print ((table 'lookup-proc) 10 20))
(print ((table 'lookup-proc) 10 40))

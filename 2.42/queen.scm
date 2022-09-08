(define (enumerate-interval i j)
  (define (itr next result)
    (cond ((> next j) '())
          ((= next j) (append result (list next)))
          (else (itr (+ next 1) (append result (list next))))))
  (itr i '()))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (append (list (cons k new-row)) rest-of-queens))

(define (safe? k positions)
  (define (remove k positions)
    (let ((top (car positions)))
      (filter
        (lambda (p) (or 
                      (= (abs (/ (- (cdr top) (cdr p)) (- (car top) (car p)))) 1)
                      (= (cdr top) (cdr p))))
        (cdr positions))))
  (null? (remove k positions)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(print (queens 1))
(print (queens 2))
(print (queens 3))
(print (queens 4))
(print (queens 5))
(print (queens 6))

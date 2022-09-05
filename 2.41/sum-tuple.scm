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

(define (unique-tuple n)
  (flatmap (lambda (i) (flatmap 
                         (lambda (j) 
                           (map 
                             (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                         (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (find-sum-tuple n s)
  (define (sum t)
    (+ (car t) (cadr t) (caddr t)))
  (filter (lambda (t) (= s (sum t))) (unique-tuple n)))

(print (find-sum-tuple 4 6))



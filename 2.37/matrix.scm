(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (me) (dot-product v me)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (me) (matrix-*-vector cols me)) m)))

(define matrix (list (list 1 2) (list 4 5)))
(define matrix2 (list (list 6 7) (list 8 9)))
(define vec (list 1 2 3))

(print (matrix-*-vector matrix vec))
(print (transpose matrix))
(print (matrix-*-matrix matrix matrix2))

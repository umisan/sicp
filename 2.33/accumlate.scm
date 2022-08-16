(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(print (map (lambda (x) (+ 10 x)) (list 1 2 3)))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(print (append (list 1 2 ) (list 3 4)))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(print (length (list 1 2 3 4)))

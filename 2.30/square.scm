(define list1 (list 1
                    (list 2 (list 3 4) 5)
                    (list 6 7)))

(define (square-tree1 tree)
  (cond ((null? tree) '())
         ((not (pair? tree)) (* tree tree))
         (else (cons (square-tree1 (car tree))
                     (square-tree1 (cdr tree))))))

(print (square-tree1 list1))

(define (square-tree2 list1)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (* sub-tree sub-tree)))
       list1))


(print (square-tree2 list1))

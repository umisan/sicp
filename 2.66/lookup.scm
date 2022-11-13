(define (key tree) (car tree))

(define (value tree) (cadr tree))

(define (left-branch tree) (caddr tree))

(define (right-branch tree) (cadddr tree))

(define (make-tree key value left right)
  (list key value left right))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key set-of-records))
         (value set-of-records))
        ((< given-key (key set-of-records)) (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))

(print (lookup 1 (make-tree 3 'user3 (make-tree 1 'user1 '() '()) (make-tree 4 'user4 () '()))))
(print (lookup 2 (make-tree 3 'user3 (make-tree 1 'user1 '() '()) (make-tree 4 'user4 () '()))))

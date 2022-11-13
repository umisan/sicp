(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (ordered-union-set set1 set2)
  (if (null? set1)
      set2
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (ordered-union-set (cdr set1) set2))
              ((< x1 x2) (cons x1 (ordered-union-set (cdr set1) set2)))
              (else (cons x2 (ordered-union-set set1 (cdr set2))))))))

(define (union-set s1 s2)
  (let ((l1 (tree->list s1))
        (l2 (tree->list s2)))
    (let ((result (ordered-union-set l1 l2)))
      (list->tree result))))


(define (ordered-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (ordered-intersection-set (cdr set1)
                                               (cdr set2))))
              ((< x1 x2)
               (ordered-intersection-set (cdr set1) set2))
              ((< x2 x1)
               (ordered-intersection-set set1 (cdr set2)))))))

(define (intersection-set s1 s2)
  (let ((l1 (tree->list s1))
        (l2 (tree->list s2)))
    (let ((result (ordered-intersection-set l1 l2)))
      (list->tree result))))


(print (union-set (list->tree (list 1 2 3)) (list->tree (list 3 4 5))))
(print (intersection-set (list->tree (list 1 2 3)) (list->tree (list 3 4 5))))

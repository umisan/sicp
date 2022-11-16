(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                                (cdr pair))
                     (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
  (cond ((= (length leaf-set) 1) (car leaf-set))
        ((= (length leaf-set) 2) (make-code-tree (car leaf-set) (cadr leaf-set)))
        (else
          (let ((s1 (car leaf-set))
                (s2 (cadr leaf-set)))
            (successive-merge (adjoin-set
                                (make-code-tree s1 s2)
                                (cddr leaf-set)))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(print (generate-huffman-tree (list (cons 'A 4) (cons 'B 2) (cons 'C 1) (cons 'D 1))))

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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol char tree)
  (cond ((leaf? tree) '())
        ((contains char (symbols (left-branch tree)))
         (cons '0 (encode-symbol char (left-branch tree))))
        ((contains char (symbols (right-branch tree)))
         (cons '1 (encode-symbol char (right-branch tree))))
        (else (error "not exists char" char))))

(define (contains char symbols)
  (cond ((null? symbols) #f)
        ((eq? char (car symbols)) #t)
        (else (contains char (cdr symbols)))))


(define huffman-tree (generate-huffman-tree (list 
                                (cons 'A 2) 
                                (cons 'BOOM 1) 
                                (cons 'GET 2) 
                                (cons 'JOB 2)
                                (cons 'NA 16)
                                (cons 'SHA 3)
                                (cons 'YIP 9)
                                (cons 'WAH 1)
                                )))

(define msg '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(print (encode msg huffman-tree))

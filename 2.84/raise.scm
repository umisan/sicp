(define (raise x)
  (let ((rf (get 'raise (type-tag x))))
    (rf x)))

(define (height x)
  (define (height-itr current cnt)
    (let ((current-tag (type-tag current)))
      (if (eq? current-tag 'complex)
          cnt
          (height-itr (raise current) (+ cnt 1)))))
  (height-itr x 0))

(define (raise-to x type)
  (if (eq? (type-tag x) type)
      x
      (raise-to (raise x) type)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args))
                    (height1 (height a1))
                    (height2 (height a2)))
                (if (< height1 height2)
                    (let ((raised (raise-to a2 type1)))
                      (apply-generic op a1 raised))
                    (let ((raised (raise-to a1 type2)))
                      (apply-generic op raised a2))))
              (error "No method for these types" (list op type-tags)))))))

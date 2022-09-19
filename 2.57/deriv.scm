(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exps num)
  (and (number? exps) (= exps num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) x)
        ((and (number? x) (number? n)) (pow x n))
        (else (list '** x n))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (append '(+) (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (append '(*) (cddr p))))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (deriv exps var)
  (cond ((number? exps) 0)
        ((variable? exps)
         (if (same-variable? exps var) 1 0))
        ((sum? exps)
         (make-sum (deriv (addend exps) var)
                   (deriv (augend exps) var)))
        ((product? exps)
         (make-sum
           (make-product (multiplier exps)
                         (deriv (multiplicand exps) var))
           (make-product (deriv (multiplier exps) var)
                         (multiplicand exps))))
        ((exponentiation? exps)         
         (make-product
           (make-product (exponent exps) (make-exponentiation (base exps) (make-sum (exponent exps) -1)))
           (deriv (base exps) var))) 
        (else
          (error "unknown expression type --DERIV" exps))))

(print (deriv '(+ x y) 'x))
(print (deriv '(+ x y 3) 'x))
(print (deriv '(* x y 3) 'x))
(print (deriv '(* x y (+ x 3)) 'x))


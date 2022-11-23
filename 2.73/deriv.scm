(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (make-exponentiation x n) (list '** x n))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else (error "unknown expression type -- DERIV" exp))))


(define (operator exp) (car exp))

(define (operand exp) (cdr exp))

(define (addend2 operand) (car s))

(define (augend2 operand) (cdr s))

(define (multiplier2 operand) (car p))

(define (multiplicand2 operand) (cdr p))

(define (base2 operand) (car operand))

(define (exponent2 operand) (cdr operand))

(define (sum-deriv operand var)
  (make-sum (deriv2 (addend2 operand) var)
            (deriv2 (augend2 operand) var)))

(define (product-deriv operand var)
  (make-sum
    (make-product (multiplier2 operand)
                  (deriv2 (multiplicand2 operand) var))
    (make-product (deriv2 (multiplier2 operand) var)
                  (multiplicand2 operand))))

(define (exponent-deriv operand var)
  (make-product
    (make-product (exponent2 operand) (make-exponentiation (base2 operand) (make-sum (exponent2 operand) -1)))
    (deriv2 (base2 operand) var)))

(put 'deriv '+ sum-deriv)
(put 'deriv '* product-deriv)
(put 'deriv '** exponent-deriv)

(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'derive (operator exp)) (operand exp) var))))

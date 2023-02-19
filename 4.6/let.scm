(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (or? exp)
  (tagged-list? exp 'or))

(define (or-conditions exp)
  (cdr exp))

(define (eval-or exp env)
  (define (itr conds env)
    (cond ((null? conds) #t)
          ((eval (car conds) env) #t)
          (else (itr (cdr conds) env))))
  (itr (or-conditions exp) env))

(define (or->if exp)
  (define (conditions->if condition alternatives)
    (if (null? alternative)
        (make-if condition #t #f)
        (make-if condition #t (conditions->if (car alternatives) (cdr alternatives)))))
  (conditions->if (car (or-conditions exp)) (cdr (or-conditions exp))))

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-conditions exp)
  (cdr exp))

(define (eval-and exp env)
  (define (itr conds env)
    (cond ((null? conds) #t)
          ((not (eval (car conds) env)) #f)
          (else (itr (cdr conds) env))))
  (itr (and-conditions exp) env))

(define (and->if exp)
  (define (conditions->if condition alternatives)
    (if (null? alternative)
        (make-if condition #t #f)
        (make-if condition (conditions->if (car alternatives) (cdr alternatives)) #f)))
  (conditions->if (car (and-conditions exp)) (cdr (and-conditions exp))))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp sep)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cond 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-caluse? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-recipient-clause? clause)
  (eq? (car (cond-actions clause)) '=>))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond-recipient clause) (caddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clause)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-caluse? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last --COND->IF"))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (eval-cond exp env)
  (define (rec-eval-cond clauses env)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (car clauses)))
          (cond ((cond-else-caluse? first)
                 (if (null? rest)
                     (eval-sequence (cond-actions first) env)
                     (error "ELSE clause isn't last --eval-cond")))
                ((cond-recipient-clause? first)
                 (let ((evaluated (eval (cond-predicate first) env)))
                   (if (true? evaluated)
                       (apply (eval (cond-recipient first) env) (list evaluated))
                       (rec-eval-cond (cdr clauses env)))))
                (else (let ((evaluated (eval (cond-predicate first) env)))
                        (if (true? evaluated)
                            (eval-sequence (cond-actions first) env)
                            (rec-eval-cond (cdr clauses) env))))))))
  (rec-eval-cond (car exp) env))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-paramenters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda paramenters body)
  (cons 'lambda (cons paramenters body)))


(define (let? exp) (tagged-list? exp 'let))

(define (let-definitions exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (let ((paramenters (map car (let-definitions exp)))
        (expressions (map cadr (let-definitions exp))))
    (cons 
      (make-lambda definitions (let-body exp))
      (expressions))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-paramenters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((or? exp) (eval-or exp env))
        ((and? exp) (eval-and exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

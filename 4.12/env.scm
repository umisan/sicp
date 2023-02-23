(define (eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoated? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr)
         (make-procedure (lambda-parameters expr)
                         (lambda-body expr)
                         env))
        ((begin? expr)
         (eval-sequence (begin-actions expr) env))
        ((cond? expr) (eval (cond->if expr) env))
        ((application? expr)
         (apply (eval (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compond-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exprs env)
  (if (no-operands? exprs)
      '()
      (cons (eval (first-operand exprs) env)
            (list-of-values (rest-operands exprs) env))))

(define (eavl-if? expr env)
  (if (true? (eval (if-predicate expr) env))
      (eval (if-consequent expr) env)
      (eval (if-alternative expr) env)))


(define (eval-sequence exprs env)
  (cond ((last-expr? exprs) (eval (first-expr exprs) env))
        (else (eval (first-exprs exprs) env)
              (eval-sequence (rest-exprs exprs) env))))


(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (eval (assignment-value expr) env)
                       env)
  'ok)


(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (eval (definition-value expr) env)
                    env)
  'ok)

(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        (else #f)))

(define (variable? expr) (symbol? expr))

(define (quoated? expr)
  (tagged-list? expr 'quote))

(define (text-of-quotation expr) (cadr expr))

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      #f))

(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-variable expr) (cadr expr))

(define (assignment-value expr) (caddr expr))

(define (definition? expr)
  (tagged-list? expr 'define))

(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (make-lambda (cdadr expr)
                   (cddr expr))))

(define (lambda? expr) (tagged-list? expr 'lambda))

(define (lambda-parameters expr) (cadr expr))

(define (lambda-body expr) (cddr expr))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? expr) (tagged-list? expr 'if))

(define (if-predicate expr) (cadr expr))

(define (if-consequent expr) (caddr expr))

(define (if-alternative expr)
  (if (not (null? (cddr expr)))
      (cadddr expr)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? expr) (tagged-list? expr 'begin))

(define (begin-actions expr) (cdr expr))

(define (last-expr? seq) (null? (cdr seq)))

(define (first-expr seq) (car seq))

(define (rest-exprs seq) (cdr seq))

(define (sequence->expr seq)
  (cond ((null? seq) seq)
        ((last-expr? seq) (first-expr seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? expr) (pair? expr))

(define (operator expr) (car expr))

(define (operands expr) (cdr expr))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? expr) (tagged-list? expr 'cond))

(define (cond-clauses expr) (cdr expr))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clauses) (car clauses))

(define (cond-actions clause) (cdr clause))

(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->expr (cond-actions first))
                (error "ELSE clause in't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->expr (cond-actions first))
                     (expand-clauses rest))))))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compond-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (caddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons 'frame (map cons variables values)))

(define (get-bindings frame) (cdr frame))

(define (first-binding bindings) (car bindings))

(define (rest-bindings bindings) (cdr bindings))

(define (search-binding frame var)
  (define (bindings-loop bindings)
    (cond ((null? bindings) '())
          ((eq? var (get-var (first-binding bindings)))
           (first-binding bindings))
          (else (bindings-loop (rest-bindings bindings)))))
  (bindings-loop (get-bindings frame)))

(define (get-var binding) (car binding))

(define (get-val binding) (cdr binding))

(define (set-val! binding new-val) (set-cdr! binding new-val))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (let ((binding (search-binding frame var)))
            (if (null? binding)
                (env-loop (enclosing-environment env))
                (get-val binding))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (let ((binding (search-binding frame var)))
            (if (null? binding)
                (env-loop (enclosing-environment env))
                (set-val! binding val))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((binding (search-binding frame var)))
      (if (null? binding)
          (add-binding-to-frame! var val frame)
          (set-val! binding val)))))

(define env (extend-environment '(a) (list 10) the-empty-environment))

(print env)

(add-binding-to-frame! 'b 20 (first-frame env))

(print env)
(print (get-bindings (first-frame env)))

(print (lookup-variable-value 'a env))
(print (lookup-variable-value 'b env))

(set-variable-value! 'b 30 env)

(print (lookup-variable-value 'b env))

(define-variable! 'c 20 env)

(print (lookup-variable-value 'c env))

(define-variable! 'c 10 env)

(print (lookup-variable-value 'c env))

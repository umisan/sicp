(define (meval expr env)
  (print env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoated? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((unbind? expr) (eval-unbind expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr)
         (make-procedure (lambda-parameters expr)
                         (lambda-body expr)
                         env))
        ((begin? expr)
         (eval-sequence (begin-actions expr) env))
        ((cond? expr) (meval (cond->if expr) env))
        ((application? expr)
         (mapply (meval (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (mapply procedure arguments)
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
          (error "Unknown procedure type -- APPLY" procedure arguments))))


(define (list-of-values exprs env)
  (if (no-operands? exprs)
      '()
      (cons (meval (first-operand exprs) env)
            (list-of-values (rest-operands exprs) env))))

(define (eval-if expr env)
  (if (true? (meval (if-predicate expr) env))
      (meval (if-consequent expr) env)
      (meval (if-alternative expr) env)))

(define (eval-sequence exprs env)
  (cond ((last-expr? exprs) (meval (first-expr exprs) env))
        (else (meval (first-exprs exprs) env)
              (eval-sequence (rest-exprs exprs) env))))


(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (meval (assignment-value expr) env)
                       env)
  'ok)


(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (meval (definition-value expr) env)
                    env)
  'ok)

(define (eval-unbind expr env)
  (unbind-variable! (unbind-variable expr)
                    env)
  'ok)

(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        (else #f)))

(define (variable? expr) 
  (symbol? expr))

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

(define (unbind? expr)
  (tagged-list? expr 'unbind))

(define (unbind-variable expr) (cadr expr))

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

(define (procedure-environment p) (cadddr p))

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

(define (unbind-binding! frame var)
  (let ((bindings (get-bindings frame)))
    (set-cdr! 
      frame 
      (filter 
        (lambda (binding) (not (eq? var (get-var binding))))
        bindings))))

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

(define (unbind-variable! var env)
  (let ((frame (first-frame env)))
    (unbind-binding! frame var)))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
    (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (meval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input str)
  (newline)
  (newline)
  (display str)
  (newline))

(define (announce-output str)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compond-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))
(driver-loop)

;Syntax specification, hehe
(define (self-evaluating? exp)
  (cond ((number? exp) true)
		((string? exp) true)
		(else false)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
	(eq? (car exp) tag)
	false))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
	(cadr exp)
	(caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
	(caddr exp)
	(make-lambda (cdadr exp) ;formal parameters
				 (cddr exp)))) ;body
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
	(cadddr exp)
	'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
	'false ;no else clause
	(let ((first (car clauses))
		  (rest (cdr clauses)))
	  (if (cond-else-clause? first)
		(if (null? rest)
		  (sequence->exp (cond-actions first))
		  (error "ELSE clause isn't last: COND->IF" clauses))
		(make-if (cond-predicate first)
				 (sequence->exp (cond-actions first))
				 (expand-clauses rest))))))






(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((unbinding? exp) (eval-unbinding exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp) (make-procedure (lambda-parameters exp)
									   (lambda-body exp)
									   env))
		((begin? exp) (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((application? exp) (apply (eval (operator exp) env)
								   (list-of-values (operands exp) env)))
		(else (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-procedure procedure arguments))
		((compound-procedure? procedure)
		 (eval-sequence
		   (procedure-body procedure)
		   (extend-environment
			 (procedure-parameters procedure)
			 arguments
			 (procedure-environment procedure))))
		(else (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exp)
	'()
	(cons (eval (first-operand exps) env)
		  (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
	(eval (if-consequent exp) env)
	(eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
		(else (eval (first-exp exps) env)
			  (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
					   (eval (assignment-value exp) env)
					   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
					(eval (definition-value exp) env)
					env)
  'ok)

;And now starts subchapter 4.1.3 :)

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
	(cons (make-frame vars vals) base-env)
	(if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars) (env-loop (enclosing-environment env)))
			((eq? var (car vars)) (car vals))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
	  (error "Unbound variable" var)
	  (let ((frame (first-frame env)))
		(scan (frame-variables frame)
			  (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars) (env-loop (enclosing-environment env)))
			((eq? var (car vars)) (set-car! vals val))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
	  (error "Unbound variable: SET!" var)
	  (let ((frame (first-frame env)))
		(scan (frame-variables frame)
			  (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
	(define (scan vars vals)
	  (cond ((null? vars) (add-binding-to-frame! var val frame))
			((eq? var (car vars)) (set-car! vals val))
			(else (scan (cdr vars) (cdr vals)))))
	(scan (frame-variables frame) (frame-values frame))))


;4.11 - change the environment abstraction to use a list of bindings rather than two lists. Eg. ( (v1 w1), ..., (vn wn))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))


(define (make-frame variables values)
  (define (make-frame-iter vars vals)
	(if (null? vars)
	  '()
	  (cons (cons (car vars) (car vals))
			(make-frame-iter (cdr vars) (cdr vals)))))
  (if (not (= (length variables) (length values)))
	(error "make-frame called with unequal amount of vars and vals:" vars vals)
	(make-frame-iter variables values)))
(define (frame-variables frame)
  (map car frame))
(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (define (iter subframe)
	(if (null? (cdr subframe))
	  (set-cdr! subframe (cons (cons var val) '()))
	  (iter (cdr subframe))))
  (iter frame))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
	(cons (make-frame vars vals) base-env)
	(if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))


;4.12 :)
(define (lookup-variable-value var env)
  (define on-empty (lambda (dummy) (error "Unbound variable" var)))
  (define on-eq (lambda (bindings) (cdar bindings)))
  (define on-null (lambda (env) (env-traversal (enclosing-environment env) var on-null on-empty on-eq)))
  (env-traversal env var on-null on-empty on-eq))


(define (set-variable-value! var val env)
  (define on-empty (lambda (dummy) (error "Unbound variable" var)))
  (define on-eq (lambda (bindings) (set-cdr! (car bindings) val)))
  (define on-null (lambda (env) (env-traversal (enclosing-environment env) var on-null on-empty on-eq)))
  (env-traversal env var on-null on-empty on-eq))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
		 (on-null (lambda (dummy) (begin (display "fat cow") (add-binding-to-frame! var val frame))))
		 (on-empty on-null)
		 (on-eq (lambda (bindings) (set-cdr! (car bindings) val))))
	(env-traversal env var on-null on-empty on-eq)))

(define (env-traversal env var on-null on-empty on-eq)
  (define (scan bindings)
	(cond ((null? bindings) (on-null env))
		  ((eq? var (caar bindings)) (on-eq bindings))
		  (else (scan (cdr bindings)))))
  (if (eq? env the-empty-environment)
	(on-empty env)
	(scan (first-frame env))))


;4.13
;So, implementing a make-unbound! DUN DUN DUN.
;I think it makes the most sense for make-unbound! to only look in one frame - the first frame of the environment - the current frame -
;since... why do you want to unbind variables? Probably because you have little use for them anymore. But unbinding is *more* state 
;manipulation, and the "smaller scale" we can keep it, the better, I think. So if a variable is done with, the 'responsible' frame
;can get to unbind it. after all, overwriting a variable lower down to turn the old one invisible is trivial.

(define (unbinding? exp) (tagged-list? exp 'make-unbound!))
(define (unbind-variable exp) (cadr exp))
(define (eval-unbinding exp env)
  (unbind-variable! (unbind-variable exp) env)
  'ok)
(define (unbind-variable! var env)
  (filter! (lambda (binding) (not (eq? (car binding) var))) (first-frame env)))













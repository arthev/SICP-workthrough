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
(define (and? exp) (tagged-list exp 'and))
(define (and-clauses exp) (cdr exp))
(define (or? exp) (tagged-list exp 'or))
(define (or-clauses exp) (cdr exp))






(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((and? exp) (eval-and (and-clauses exp) env))
		((or? exp) (eval-or (or-clauses exp) env))
		((let? exp) (eval (let->combination exp) env))
		((let*? exp) (eval (let*->nested-lets exp) env))
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
	(let ((first (eval (first-operand exps) env)))
	  (let ((rest (list-of-values (rest-operands exps) env)))
		(cons first rest)))))

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

(define (eval-and exps env)
  (if (null? clauses)
	true
	(let ((first (eval (first-exp exps) env)))
	  (if first
		(let ((rest (rest-exps exps)))
		  (if (null? rest)
			first
			(eval-and (rest-exps exps) env)
			false))))))

(define (eval-or exps env)
  (if (null? clauses)
	false
	(let ((first (eval (first-exp exps) env)))
	  (if first
		first
		(eval-or (rest-exps exps) env)))))




;4.5
(define (test-recipient-clause? clause) (eq? (cadr clause) '=>))
(define (test-clause clause) (car clause))
(define (recipient-clause clause) (caddr clause))
(define (cond-actions clause)
  (if (test-recipient-clause? clause)
	(cons (recipient-clause clause) (test-clause clause))
	(cdr clause)))

;4.7
;Because let* binds sequentially from left to right, with the bindings visible to the later bindings,
;this translates obviously to a sequence of lets, all with one binding each. (Where we bind from left to right, of course.)
;E.g. (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) body)
;readily becomes:
;(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) body)))
;Format of a let: (let (bindings) body)
(define (let*? exp) (tagged-list exp 'let*))
(define (let*->nested-lets the-let*)
  (define (binding-list) (cadr the-let*))
  (define (the-body) (cddr the-let*))
  (define (internal remaining-bindings)
	(cond ((null? remaining-bindings) (the-body))
		  (else
			(list 'let (list (car remaining-bindings)) (internal (cdr remaining-bindings))))))
  (internal (binding-list)))
;It should be sufficient to add the clause with action (eval (let*->nested-lets exp) env) to the evaluator,
;since this means the program will build the nested let translation, and then evaluate it in the correct environment.


;4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let->combination the-let)
  (define (the-params) (map car (cadr the-let)))
  (define (the-exps) (map cadr (cadr the-let)))
  (define (the-body) (cddr the-let))
  (cons (make-lambda (the-params) (the-body))
		(the-exps)))

;4.8
(define (let? exp) (tagged-list? exp 'let))
(define (let->combination the-let)
  (define (standard-let)
	(define (the-params) (map car (cadr the-let)))
	(define (the-exps) (map cadr (cadr the-let)))
	(define (the-body) (cddr the-let))
	(cons (make-lambda (the-params) (the-body))
		  (the-exps)))
  (define (named-let)
	(define (the-name) (cadr the-let))
	(define (the-params) (map car (caddr the-let)))
	(define (the-exps) (map cadr (caddr the-let)))
	(define (the-body) (cdddr the-let))
	(cons (make-lambda (list (the-name))
					   (make-begin
						 (list 'set! (the-name)
							   (make-lambda (the-params)
											(the-body)))
						 (list (the-name) (the-exps))))
		  (list 0)))
  (if (not (pair? (cadr the-let)))
	(named-let)
	(standard-let)))






























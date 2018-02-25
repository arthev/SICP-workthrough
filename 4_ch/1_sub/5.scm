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





;4.5 - quite the rough solution, but I think it should work.
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (test-recipient-clause? clause) (eq? (cadr clause) '=>))
(define (test-clause clause) (car clause))
(define (recipient-clause clause) (caddr clause))
(define (expand-clauses clauses)
  (if (null? clauses)
	'false ;no else clause
	(let ((first (car clauses))
		  (rest (cdr clauses)))
	  (if (cond-else-clause? first)
		(if (null? rest)
		  (sequence->exp (cond-actions first))
		  (error "ELSE clause isn't last: COND->IF" clauses))
		(if (test-recipient-clause? first)
		  (make-if (test-clause first)
				   ((recipient-clause first) (test-clause first))
				   (expand-clauses rest))
		  (make-if (cond-predicate first)
				   (sequence->exp (cond-actions first))
				   (expand-clauses rest)))))))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))
;4.5 - elegant solution found on the web, with the same fault as above solution. (Above solution evaluates the test-clause twice, which may introduce unintended side-effects.)
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
(define (cond-actions clause)
  (if (test-recipient-clause? clause)
	(cons (recipient-clause clause) (test-clause clause))
	(cdr clause)))




























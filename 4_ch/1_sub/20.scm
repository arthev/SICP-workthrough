(define apply-in-underlying-scheme apply)
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
(define (begin-actions exp) (begin (newline) (display "1") (cdr exp)))
(define (last-exp? seq) (begin (newline) (display "2") (null? (cdr seq))))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (begin (newline) (display "3") (cdr seq)))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (begin (newline) (display "4") (cdr exp)))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (begin (newline) (display "5") (cdr ops)))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (begin (newline) (display "6") (cdr exp)))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (begin (newline) (display "7") (cdr clause)))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
	'false ;no else clause
	(let ((first (car clauses))
		  (rest ((begin (newline) (display "8") (cdr clauses)))))
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

		((let? exp) (eval (let->combination exp) env))
		((letrec? exp) (eval (letrec->let exp) env))
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
		(else (error "Ascheme: Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  ;(newline) (display "exps:") (display exps) (newline) (display "no-operands? exp") (display (no-operands? exp))
  (if (no-operands? exps)
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
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (begin (newline) (display "9") (cdr env)))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (begin (newline) (display "10") (cdr frame)))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (begin (newline) (display "11") (cdr frame)))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
	(cons (make-frame vars vals) base-env)
	(if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

;4.11 - change the environment abstraction to use a list of bindings rather than two lists. Eg. ( (v1 w1), ..., (vn wn))


(define (enclosing-environment env) (begin (newline) (display "13") (cdr env)))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))


(define (make-frame variables values)
  (define (make-frame-iter vars vals)
	(newline) (display "14")
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
  (newline) (display "15")
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (newline) (display "16")
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
(define (set-variable-value! var val env)
  (define on-empty (lambda (dummy) (error "Unbound variable" var)))
  (define on-eq (lambda (bindings) (set-cdr! (car bindings) val)))
  (define on-null (lambda (env) (env-traversal (enclosing-environment env) var on-null on-empty on-eq)))
  (env-traversal env var on-null on-empty on-eq))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
		 (on-null (lambda (dummy) (add-binding-to-frame! var val frame)))
		 (on-empty on-null)
		 (on-eq (lambda (bindings) (set-cdr! (car bindings) val))))
	(env-traversal env var on-null on-empty on-eq)))

(define (env-traversal env var on-null on-empty on-eq)
  (newline) (display "17")
  (define (scan bindings)
	(cond ((null? bindings) (on-null env))
		  ((eq? var (caar bindings)) (on-eq bindings))
		  (else (scan (cdr bindings)))))
  (if (eq? env the-empty-environment)
	(on-empty env)
	(scan (first-frame env))))

;4.13, heh.
(define (unbinding? exp) (tagged-list? exp 'make-unbound!))
(define (unbind-variable exp) (cadr exp))
(define (eval-unbinding exp env)
  (unbind-variable! (unbind-variable exp) env))
(define (unbind-variable! var env)
  (filter! (lambda (binding) (not (eq? (car binding) var))) (first-frame env)))




;Section 4.1.4
(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
										 (primitive-procedure-objects)
										 the-empty-environment)))
	(define-variable! 'true true initial-env)
	(define-variable! 'false false initial-env)
	initial-env))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)
		(list 'list list)
		(list '+ +)
		(list '* *)
		(list '- -)
		(list '/ /)
		(list '= =)
		(list 'nil '())
		))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))





(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
	(let ((output (eval input the-global-environment)))
	  (announce-output output-prompt)
	  (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)(newline)(display string)(newline))
(define (announce-output string)
  (newline)(display string)(newline))
(define (user-print object)
  (if (compound-procedure? object)
	(display (list 'compound-procedure 
				   (procedure-parameters object)
				   (procedure-body object)
				   '<procedure-env>))
	(display object)))

(define the-global-environment (setup-environment))



;4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let->combination the-let)
  (define the-params (map car (cadr the-let)))
  (define the-exps (map cadr (cadr the-let)))
  (define the-body (cddr the-let))
  (cons (make-lambda the-params the-body)
		the-exps))

;4.16
;;Installed scan-out-defines in make-procedure since then it'll only get called once per procedure.
(define (lookup-variable-value var env)
  (define on-empty (lambda (dummy) (error "Unbound variable" var)))
  (define on-eq (lambda (bindings) (cdar bindings)))
  (define on-null (lambda (env) (env-traversal (enclosing-environment env) var on-null on-empty on-eq)))
  (let ((found (env-traversal env var on-null on-empty on-eq)))
	(if (eq? found '*unassigned*)
	  (error "Unassigned value found for" var env)
	  found)))

(define (scan-out-defines body)
  (let ((the-defs (filter (lambda (s) (definition? s)) body)))
	(if (null? the-defs)
	  body
	  (let* ((def-names (map definition-variable the-defs))
			 (def-bods (map definition-value the-defs))
			 (main-body (filter (lambda (s) (not (definition? s))) body))
			 (let-name-list (map (lambda (v) (list v ''*unassigned*)) def-names))
			 (let-body-sets (map (lambda (v e) (list 'set! v e)) def-names def-bods)))
		(list (append (append (list 'let let-name-list)
							  let-body-sets)
					  main-body))))))

;4.20 - taking inspiration from 4.6...
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->let exp)
  (define the-params (map car (cadr exp)))
  (define the-exps (map cadr (cadr exp)))
  (define the-body (cddr exp))
  (append (append (list 'let (map (lambda (v) (list v ''*unassigned*)) the-params))
						(map (lambda (v e) (list 'set! v e)) the-params the-exps))
				the-body))

























(driver-loop)

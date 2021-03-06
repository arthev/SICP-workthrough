;;;; (setting up the global environment and starting the driver loop).
;;;;In the driver loop, do
;(define (require p)
;  (if (not p) (amb)))

(load "ch4-mceval.scm")

;;;Code from SECTION 4.3.3, modified as needed to run it

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; analyze from 4.1.6, with clause from 4.3.3 added
;; and also support for Let
(define (analyze exp)
  (cond ((self-evaluating? exp) 
		 (analyze-self-evaluating exp))
		((quoted? exp) (analyze-quoted exp))
		((variable? exp) (analyze-variable exp))
		((assignment? exp) (analyze-assignment exp))
		((permanent-assignment? exp) (analyze-permanent-assignment exp))
		((definition? exp) (analyze-definition exp))
		((if? exp) (analyze-if exp))
		((if-fail? exp) (analyze-if-fail exp))
		((lambda? exp) (analyze-lambda exp))
		((begin? exp) (analyze-sequence (begin-actions exp)))
		((cond? exp) (analyze (cond->if exp)))
		((let? exp) (analyze (let->combination exp))) ;**
		((amb? exp) (analyze-amb exp))                ;**
		((require? exp) (analyze-require exp))
		((ramb? exp) (analyze-ramb exp))
		((application? exp) (analyze-application exp))
		(else
		  (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;;Simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
	(succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
	(lambda (env succeed fail)
	  (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
	(succeed (lookup-variable-value exp env)
			 fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
		(bproc (analyze-sequence (lambda-body exp))))
	(lambda (env succeed fail)
	  (succeed (make-procedure vars bproc env)
			   fail))))

;;;Conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
		(cproc (analyze (if-consequent exp)))
		(aproc (analyze (if-alternative exp))))
	(lambda (env succeed fail)
	  (pproc env
			 ;; success continuation for evaluating the predicate
			 ;; to obtain pred-value
			 (lambda (pred-value fail2)
			   (if (true? pred-value)
				 (cproc env succeed fail2)
				 (aproc env succeed fail2)))
			 ;; failure continuation for evaluating the predicate
			 fail))))

;;4.52
(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-normal exp) (cadr exp))
(define (if-fail-fail exp) (caddr exp))

;;This attempt was as far as I got, and it passes the one example program...
(define (analyze-if-fail exp)
  (lambda (env succeed fail)
	(let ((result ((analyze (if-fail-normal exp))
				   env
				   (lambda (val fail2) val)
				   (lambda () 'if-fail-failure))))
	  (if (not (equal? result 'if-fail-failure))
		(succeed result fail)
		(succeed 
		  ((analyze (if-fail-fail exp)) env
										(lambda (val fail2) val)
										(lambda () 'failed))
		  fail)))))

;;So I looked online and found this, which scratched my itch of 'the above version looks 
;;very odd and probably mishandles the failures'... as evidenced when, after looking online,
;;I tried try-again and saw that the above attempt fails to produce correct try-again values.
;;Herpaderp.
(define (analyze-if-fail exp)
  (let ((sproc (analyze (if-fail-normal exp)))
		(fproc (analyze (if-fail-fail exp))))
	(lambda (env succeed fail)
	  (sproc env
			 (lambda (val fail2)
			   (succeed val fail2))
			 (lambda ()
			   (fproc env succeed fail))))))






(define (analyze-sequence exps)
  (define (sequentially a b)
	(lambda (env succeed fail)
	  (a env
		 ;; success continuation for calling a
		 (lambda (a-value fail2)
		   (b env succeed fail2))
		 ;; failure continuation for calling a
		 fail)))
  (define (loop first-proc rest-procs)
	(if (null? rest-procs)
	  first-proc
	  (loop (sequentially first-proc (car rest-procs))
			(cdr rest-procs))))
  (let ((procs (map analyze exps)))
	(if (null? procs)
	  (error "Empty sequence -- ANALYZE"))
	(loop (car procs) (cdr procs))))

;;;Definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
		(vproc (analyze (definition-value exp))))
	(lambda (env succeed fail)
	  (vproc env                        
			 (lambda (val fail2)
			   (define-variable! var val env)
			   (succeed 'ok fail2))
			 fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
		(vproc (analyze (assignment-value exp))))
	(lambda (env succeed fail)
	  (vproc env
			 (lambda (val fail2)        ; *1*
			   (let ((old-value
					   (lookup-variable-value var env))) 
				 (set-variable-value! var val env)
				 (succeed 'ok
						  (lambda ()    ; *2*
							(set-variable-value! var
												 old-value
												 env)
							(fail2)))))
			 fail))))

;;4.51
(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
		(vproc (analyze (assignment-value exp))))
	(lambda (env succeed fail)
	  (vproc env
			 (lambda (val fail2)
			   (set-variable-value! var val env)
			   (succeed 'ok fail2))
			 fail))))
;;For set! instead of permanent-set! the example
;;program displays (a b 1) and (a c 1) rather than
;;the (a b 2) and (a c 3) with permanent-set!


;;;Procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
		(aprocs (map analyze (operands exp))))
	(lambda (env succeed fail)
	  (fproc env
			 (lambda (proc fail2)
			   (get-args aprocs
						 env
						 (lambda (args fail3)
						   (execute-application
							 proc args succeed fail3))
						 fail2))
			 fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
	(succeed '() fail)
	((car aprocs) env
				  ;; success continuation for this aproc
				  (lambda (arg fail2)
					(get-args (cdr aprocs)
							  env
							  ;; success continuation for recursive
							  ;; call to get-args
							  (lambda (args fail3)
								(succeed (cons arg args)
										 fail3))
							  fail2))
				  fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
		 (succeed (apply-primitive-procedure proc args)
				  fail))
		((compound-procedure? proc)
		 ((procedure-body proc)
		  (extend-environment (procedure-parameters proc)
							  args
							  (procedure-environment proc))
		  succeed
		  fail))
		(else
		  (error
			"Unknown procedure type -- EXECUTE-APPLICATION"
			proc))))

;;;amb expressions

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
	(lambda (env succeed fail)
	  (define (try-next choices)
		(if (null? choices)
		  (fail)
		  ((car choices) env
						 succeed
						 (lambda ()
						   (try-next (cdr choices))))))
	  (try-next cprocs))))


;; 4.50
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
	(lambda (env succeed fail)
	  (define (try-next choices)
		(if (null? choices)
		  (fail)
		  (let* ((random-index (random (length choices)))
				 (random-choice (list-ref choices random-index))
				 (remaining-choices (append (sublist choices 0 random-index)
											(sublist choices (1+ random-index) (length choices)))))
			(random-choice env
						   succeed
						   (lambda ()
							 (try-next remaining-choices))))))
	  (try-next cprocs))))
;;Alyssa could use the ramb to change her program to make random choices about whether to generate prep
;;phrases or not, and thereby start to generate all possible sentences over the grammar, rather than
;;the word-substitutions over the sentence length given to *unparsed* as mentioned in my answer there.

;; 4.54
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
	(lambda (env succeed fail)
	  (pproc env
			 (lambda (pred-value fail2)
			   (if (not pred-value)
				 (fail2)
				 (succeed 'ok fail2)))
			 fail))))


;;;Driver loop

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
	(prompt-for-input input-prompt)
	(let ((input (read)))
	  (if (eq? input 'try-again)
		(try-again)
		(begin
		  (newline)
		  (display ";;; Starting a new problem ")
		  (ambeval input
				   the-global-environment
				   ;; ambeval success
				   (lambda (val next-alternative)
					 (announce-output output-prompt)
					 (user-print val)
					 (internal-loop next-alternative))
				   ;; ambeval failure
				   (lambda ()
					 (announce-output
					   ";;; There are no more values of")
					 (user-print input)
					 (driver-loop)))))))
  (internal-loop
	(lambda ()
	  (newline)
	  (display ";;; There is no current problem")
	  (driver-loop))))



;;; Support for Let (as noted in footnote 56, p.428)

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
	(make-combination (make-lambda (map let-var bindings)
								   (let-body exp))
					  (map let-val bindings))))



;; A longer list of primitives -- suitable for running everything in 4.3
;; Overrides the list in ch4-mceval.scm
;; Has Not to support Require; various stuff for code in text (including
;;  support for Prime?); integer? and sqrt for exercise code;
;;  eq? for ex. solution

(define primitive-procedures
  (list (list 'car car)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)
		(list 'list list)
		(list 'memq memq)
		(list 'member member)
		(list 'not not)
		(list '+ +)
		(list '- -)
		(list '* *)
		(list '= =)
		(list '> >)
		(list '>= >=)
		(list 'abs abs)
		(list 'remainder remainder)
		(list 'integer? integer?)
		(list 'sqrt sqrt)
		(list 'eq? eq?)
		(list 'display display)
		(list 'newline newline)
		(list 'equal? equal?)
		(list 'even? even?)
		(list 'odd? odd?)
		;;      more primitives
		))

(define the-global-environment (setup-environment))

'AMB-EVALUATOR-LOADED

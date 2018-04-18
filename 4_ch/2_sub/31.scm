;;;;LAZY EVALUATOR FROM SECTION 4.2 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm
;;;; Also includes enlarged primitive-procedures list

;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator of
;;;;  sections 4.1.1-4.1.4, since it uses the expression representation,
;;;;  environment representation, etc.
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.
;;;;**WARNING: Don't load mceval twice (or you'll lose the primitives
;;;;  interface, due to renamings of apply).

;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two lines at the end of the file ch4-mceval.scm
;;;; (setting up the global environment and starting the driver loop).


;;;;  To run without memoization, reload the first version of force-it below


;;**implementation-dependent loading of evaluator file
;;Note: It is loaded first so that the section 4.2 definition
;; of eval overrides the definition from 4.1.1
(load "ch4-mceval.scm")


;;;SECTION 4.2.2

;;; Modifying the evaluator

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))



(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))



(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
;(define (evaluated-thunk? obj)
;  (tagged-list? obj 'evaluated-thunk))

;(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
;

;; memoizing version of force-it

;(define (force-it obj)
;  (cond ((thunk? obj)
;         (let ((result (actual-value
;                        (thunk-exp obj)
;                        (thunk-env obj))))
;           (set-car! obj 'evaluated-thunk)
;           (set-car! (cdr obj) result)  ; replace exp with its value
;           (set-cdr! (cdr obj) '())     ; forget unneeded env
;           result))
;        ((evaluated-thunk? obj)
;         (thunk-value obj))
;        (else obj)))

;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))


;4.31
(define (lazy-param? p) (and (pair? p) (eq? (cadr p) 'lazy) (null? (cddr p))))
(define (lazy? obj) (tagged-list? obj 'lazy))
(define (lazy-mem-param? p) (and (pair? p) (eq? (cadr p) 'lazy-mem) (null? (cddr p))))
(define (lazy-mem? obj) (tagged-list? obj 'lazy-mem))

(define (thunk? obj)
  (or (lazy? obj) (lazy-mem? obj)))

(define (delay-lazy exp env)
  (list 'lazy exp env))
(define (delay-lazy-mem exp env)
  (list 'lazy-mem exp env))

(define (eval-lazy-memo? obj) (tagged-list? obj 'eval-lazy-memo))

(define (force-it obj)
  (cond ((lazy? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
		((lazy-mem? obj)
		 (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
		   (set-car! obj 'eval-lazy-memo)
		   (set-car! (cdr obj) result)
		   (set-cdr! (cdr obj) '())
		   result))
		((eval-lazy-memo? obj) (thunk-value obj))
		(else obj)))




(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-procedure
		   procedure
		   (list-of-arg-values arguments env)))
		((compound-procedure? procedure)
		 (let ((formal-parameters (procedure-parameters procedure)))
		   (eval-sequence
			 (procedure-body procedure)
			 (extend-environment
			   (map (lambda (p) (if (pair? p) (car p) p)) formal-parameters)
			   (list-of-mixed-args formal-parameters arguments env)
			   (procedure-environment procedure)))))
		(else
		  (error "Unknown procedure type -- APPLU" procedure))))

(define (list-of-mixed-args params args env)
  (map 
	(lambda (p a)
	  (cond ((lazy-param? p) (delay-lazy a env))
			((lazy-mem-param? p) (delay-lazy-mem a env))
			(else (eval a env))))
	params
	args))






'LAZY-EVALUATOR-LOADED

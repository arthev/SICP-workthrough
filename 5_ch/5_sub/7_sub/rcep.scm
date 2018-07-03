;;;;;;READ-COMPILE-EXECUTE-PRINT LOOP "EVALUATOR" FROM EXERCISE .549 OF
;;;;;:STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS
;;;;;;BUILT ON THE BASIS OF:

;;;;EXPLICIT-CONTROL EVALUATOR FROM SECTION 5.4 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS
;;;; MODIFIED TO SUPPORT COMPILED CODE (AS IN SECTION 5.5.7)

;;;;Changes to basic evaluator machine are
;;;; (1) some new eceval controller code for the driver and apply-dispatch;
;;;; (2) some additional machine operations from;
;;;; (3) support for compiled code to call interpreted code (exercise 5.47) --
;;;;     (new register and 1 new instruction at start)
;;;; (4) new startup aid start-eceval

;; Explicit-control evaluator.
;; To use it, load "load-eceval-compiler.scm", which loads this file and the
;;  support it needs (including the register-machine simulator)

;; To start, can use compile-and-go as in section 5.5.7
;;  or start-eceval as in the section 5.5.7 footnote.

;; To resume the machine without reinitializing the global environment
;; if you have somehow interrupted out of the machine back to Scheme, do

;: (set-register-contents! eceval 'flag false)
;: (start eceval)

;;;;;;;;

;; any old value to create the variable so that
;;  compile-and-go and/or start-eceval can set! it.
(define the-global-environment '())

;;; Interfacing compiled code with eceval machine
;;; From section 5.5.7
(define (start-eceval)
  (set! the-global-environment (setup-environment))
;  (set-register-contents! eceval 'flag false)
  (start eceval))

;; Modification of section 4.1.4 procedure
;; **replaces version in syntax file
(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))
;;
;;**NB. To [not] monitor stack operations, comment in/[out] the line after
;; print-result in the machine controller below
;;**Also choose the desired make-stack version in regsim.scm

(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read)			;used by eceval
   ;(list 'compile-and-run compile-and-run)
   (list 'assemble (lambda (statements) (assemble statements eceval)))
   (list 'compile (lambda (insts) (statements (compile insts 'val 'return))))

   ;;used by compiled code
   (list 'list list)
   (list 'cons cons)

   ;;operations in syntax.scm
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

   ;;operations in eceval-support.scm
   (list 'true? true?)
   (list 'false? false?)		;for compiled code
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
   (list 'get-global-environment get-global-environment)

   ;;for compiled code (also in eceval-support.scm)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
   ))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev
	 compapp			;*for compiled to call interpreted
	 )
   eceval-operations
  '(
;read-eval-print-loop
read-compile-execute-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; RCEPL input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label compile-time))
print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-compile-execute-loop))
compile-time
  (assign val (op compile) (reg exp))
  (assign val (op assemble) (reg val))
execute-time
  (save continue)
  (goto (reg val))

   )))

'(RCEP LOADED)

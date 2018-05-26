;;;;REGISTER-MACHINE SIMULATOR FROM SECTION 5.2 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch5.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can define and simulate machines as shown in section 5.2

;;;**NB** there are two versions of make-stack below.
;;; Choose the monitored or unmonitored one by reordering them to put the
;;;  one you want last, or by commenting one of them out.
;;; Also, comment in/out the print-stack-statistics op in make-new-machine
;;; To find this stack code below, look for comments with **
(define (print arg)
  (newline) (display "PRINT:") (display arg))


(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
		(tracing? #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
			   (if tracing? 
				 (begin (newline) (display "REGTRACE ") (display name) 
						(display ": ") (display contents) (display " -> ") (display value)))
			   (set! contents value)))
			((eq? message 'trace-on) (set! tracing? #t))
			((eq? message 'trace-off) (set! tracing? #f))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (trace-off tracer)
  (tracer 'trace-off))

(define (trace-on tracer)
  (tracer 'trace-on))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;**monitored version from section 5.2.4
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
		(all-instructions '())
		(entry-regs '())
		(stack-users '())
		(reg-sources '())
		(instruction-count 0)
		(tracing? #t))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag)))
		  (breakpoints '()))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
		  (cond ((null? insts) 'done)
				((symbol? (car insts)) 
				 (let ((label (car insts)))
				   (if tracing?
					 (print label))

				   (for-each
					 (lambda (br)
					   (reset-breakpoint br)
					   (activate-breakpoint br))
					 (filter
					   (lambda (br) (equal? (breakpoint-label br) label))
					   breakpoints))

				   (advance-pc pc)
				   (execute)))
				(else
				  (set! instruction-count (1+ instruction-count))
				  
				  (if tracing?
					(print (instruction-text (car insts))))
				  
				  (let* ((break-flag? #f)
						 (active-brs (filter breakpoint-active? breakpoints)))
					(for-each (lambda (br) (set-car! (cddr br) (1+ (breakpoint-c br))))
							  active-brs)
					(for-each
					  (lambda (br)
						(newline)(display "BREAKPOINT: ")(display (breakpoint-label br))
						(display " - ")(display (breakpoint-n br))
						(set! break-flag? #t)
						(reset-breakpoint br))
					  (filter (lambda (br) (eq? (breakpoint-n br) (breakpoint-c br)))
							  active-brs))
					
					(if break-flag?
					  (begin
						(print "BREAK FLAG TRUARU")
						'paused)
					  (begin
						((instruction-execution-proc (car insts)))
						(execute))))))))
	  (define (cancel-breakpoint label n)
		(define (looper brs)
		  (cond ((null? brs) 
				 '())
				((and (eq? label (breakpoint-label (car brs)))
					  (eq? n (breakpoint-n (car brs))))
				 (looper (cdr brs)))
				(else
				  (cons (car brs) (looper (cdr brs))))))
		(set! breakpoints (looper breakpoints)))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
			  ((eq? message 'proceed) (execute))
			  ((eq? message 'set-breakpoint)
			   (lambda (label n)
				 (set! breakpoints (cons (make-breakpoint label n)
										 breakpoints))))
			  ((eq? message 'cancel-all-breakpoints)
			   (set! breakpoints '()))
			  ((eq? message 'cancel-breakpoint) cancel-breakpoint)
			  ((eq? message 'get-breakpoints) breakpoints)
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
			  ((eq? message 'get-all-instructions) all-instructions)
			  ((eq? message 'set-all-instructions) 
			   (lambda (new) (set! all-instructions new)))
			  ((eq? message 'get-entry-regs) entry-regs)
			  ((eq? message 'set-entry-regs)
			   (lambda (new) (set! entry-regs new)))
			  ((eq? message 'get-stack-users) stack-users)
			  ((eq? message 'set-stack-users)
			   (lambda (new) (set! stack-users new)))
			  ((eq? message 'get-reg-sources) reg-sources)
			  ((eq? message 'set-reg-sources)
			   (lambda (new) (set! reg-sources new)))
			  ((eq? message 'reset-instruction-count)
			   (begin 
				 (newline) (display "Instruction count:") (display instruction-count) 
				 (set! instruction-count 0)))
			  ((eq? message 'trace-on) (set! tracing? #t))
			  ((eq? message 'trace-off) (set! tracing? #f))
			  ((eq? message 'regtrace-on) 
			   (lambda (reg) (trace-on (lookup-register reg))))
			  ((eq? message 'regtrace-off)
			   (lambda (reg) (trace-off (lookup-register reg))))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (proceed-machine machine)
  (machine 'proceed))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (get-breakpoints machine)
  (machine 'get-breakpoints))

(define (make-breakpoint label n)
  (list label n 0 #f))

(define (activate-breakpoint br)
  (set-car! (cdddr br) #t))

(define (reset-breakpoint br)
  (set-car! (cdddr br) #f)
  (set-car! (cddr br) 0))

(define (breakpoint-label br)
  (car br))
(define (breakpoint-n br)
  (cadr br))
(define (breakpoint-c br)
  (caddr br))
(define (breakpoint-active? br)
  (cadddr br))


(define (regtrace-on machine reg)
  ((machine 'regtrace-on) reg))
(define (regtrace-off machine reg)
  ((machine 'regtrace-off) reg))

(define supported-insts '(assign dec test branch goto save restore perform))

(define (analyze-data-path inst machine)
  ;;;(newline) (display inst)
  (let ((all-insts (machine 'get-all-instructions))
		(entry-regs (machine 'get-entry-regs))
		(stack-users (machine 'get-stack-users))
		(reg-sources (machine 'get-reg-sources))
		(inst-type (car inst))
		(set-all-insts (machine 'set-all-instructions))
		(set-entry-regs (machine 'set-entry-regs))
		(set-stack-users (machine 'set-stack-users))
		(set-reg-sources (machine 'set-reg-sources)))
	;;First, let's add the inst to the all-insts.
	(define (add-to-all-insts)
	  ;;;(newline) (display "In add-to-all-insts")
	  (if (not (assoc inst-type all-insts))
		(set! all-insts (cons (list inst-type '()) all-insts)))
	  (let ((rel-list (cadr (assoc inst-type all-insts))))
		(if (not (member inst rel-list))
			(set-car! (cdr (assoc inst-type all-insts)) (cons (cdr inst) rel-list))))
	  'ok)
	;;Next, let's add the registers used to hold entry points.
	(define (add-to-entry-regs)
	  ;;;(newline) (display "In add-to-entry-regs")
	  (if (equal? inst-type 'goto)
		(if (equal? (caadr inst) 'reg)
		  (if (not (member (cadadr inst) entry-regs))
			(set! entry-regs (cons (cadadr inst) entry-regs)))))
	  'ok)
	;;Next, let's add the registers that use the stack.
	(define (add-to-stack-users)
	  ;;;(newline) (display "In add-to-entry-regs")
	  (if (or (equal? inst-type 'save)
			  (equal? inst-type 'restore))
		(if (not (member (cadr inst) stack-users))
		  (set! stack-users (cons (cadr inst) stack-users))))
	  'ok)
	;;And finally, the list of reg-sources.
	(define (add-to-reg-sources)
	 ;;;(newline) (display "In add-to-entry-regs")
	  (if (equal? inst-type 'assign)
		(let ((r (cadr inst)))
		  (if (not (assoc r reg-sources))
			(set! reg-sources (cons (list r '()) reg-sources)))
		  (let ((r-list (cadr (assoc r reg-sources)))
				(source (cddr inst)))
			(if (not (member source r-list))
			  (set-car! (cdr (assoc r reg-sources)) (cons source r-list)))))))
	;;And we just invoke...
	(add-to-all-insts)
	(add-to-entry-regs)
	(add-to-stack-users)
	(add-to-reg-sources)
	;;And then we just set the real ones...
	(set-all-insts all-insts)
	(set-entry-regs entry-regs)
	(set-stack-users stack-users)
	(set-reg-sources reg-sources)))






(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels 
	controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)
	machine))

(define (extract-labels text receive machine)
  (if (null? text)
      (receive '() '())
      (extract-labels 
		(cdr text)
        (lambda (insts labels)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
			  (if (memq next-inst (map car labels))
				  (error "Label occurs more than once -- EXTRACT-LABELS:" next-inst)
				  (let ((linsts (cons next-inst insts)))
					(receive linsts
							 (cons (make-label-entry next-inst
													 linsts)
								   labels))))
			  (begin 
				;;Not a label - therefore an instruction - therefore time to
				;;do the datapath analysis for exercise 5.12. 
				;;In its own function, of course.
				(analyze-data-path next-inst machine)
				(receive (cons (make-instruction next-inst)
							   insts)
						 labels)))))
		machine)))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (if (symbol? inst)
	inst
	(car inst)))


(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (if (not (symbol? inst))
	(set-cdr! inst proc)))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))


(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((symbol? inst) '())
		((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
		((eq? (car inst) 'dec)
		 (make-dec inst machine pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))


(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-dec inst machine pc)
  (let ((target
		  (get-register machine (dec-reg-name inst))))
	(lambda ()
	  (set-contents! target (- (get-contents target) 1))
	  (advance-pc pc))))

(define (dec-reg-name dec-inst)
  (define (well-formed?)
	(and (= (length dec-inst) 2)
		 (symbol? (cadr dec-inst))))
  (if (well-formed?)
	(cadr dec-inst)
	(error "Bad DEC instruction -- DEC-REG-NAME:" dec-inst)))



(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (newline) (display "TEST-CON")
  (cdr test-instruction))


(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) 
  (newline) (display "PROFOM_ACT")
  (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
				(if (label-exp? e)
				  (error "Machine ops cannot operate on labels -- MAKE-OP:" e)
                  (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;; from 4.1
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

'(REGISTER SIMULATOR LOADED)

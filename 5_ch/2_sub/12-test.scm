(load "12.scm")

(define FIB
  (make-machine
	'(continue n val)
	(list (list '+ +) (list '< <) (list '- -))
	'(  (assign continue (label fib-done))
	  fib-loop
	    (test (op <) (reg n) (const 2))
		(branch (label immediate-answer))
		;;set up to compute Fib(n-1)
		(save continue)
		(assign continue (label afterfib-n-1))
		(save n)
		(dec n)
		(goto (label fib-loop))
	  afterfib-n-1
	    (restore n)
		;;set up to compute Fib(n-2)
		(assign n (op -) (reg n) (const 2))
		(assign continue (label afterfib-n-2))
		(save val)
		(goto (label fib-loop))
	  afterfib-n-2
	    (assign n (reg val))
		(restore val)
		(restore continue)
		(assign val (op +) (reg val) (reg n))
		(goto (reg continue))
	  immediate-answer
	    (assign val (reg n))
		(goto (reg continue))
	  fib-done)))

(FIB 'get-all-instructions)
(FIB 'get-entry-regs)
(FIB 'get-stack-users)
(FIB 'get-reg-sources)

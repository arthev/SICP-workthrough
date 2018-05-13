(load "regsim.scm")

(define REM
  (make-machine
	'(b n val continue)
	(list (list '* *) (list '- -) (list '= =))
	'(  (assign continue (label expt-done))
	  expt-loop
	    (test (op =) (reg n) (const 0))
		(branch (label base-case))
		(save continue)
		(assign n (op -) (reg n) (const 1))
		(assign continue (label after-expt))
		(goto (label expt-loop))
	  after-expt
	    (restore continue)
		(assign val (op *) (reg val) (reg b))
		(goto (reg continue))
	  base-case
	    (assign val (const 1))
		(goto (reg continue))
	  expt-done)))

(define IEM
  (make-machine
	'(b n val)
	(list (list '* *) (list '- -) (list '= =))
	'(  (assign val (const 1))
	  expt-loop
	    (test (op =) (reg n) (const 0))
		(branch (label expt-done))
		(assign n (op -) (reg n) (const 1))
		(assign val (op *) (reg val) (reg b))
		(goto (label expt-loop))
	  expt-done)))

(define (test-machine machine b n)
  (set-register-contents! machine 'b b)
  (set-register-contents! machine 'n n)
  (start machine)

  (let ((direct-calc (expt b n))
		(machine-calc (get-register-contents machine 'val)))
	(if (= direct-calc machine-calc)
	  machine-calc
	  (begin
		(newline) (display "Expected: " direct-calc)
		(newline) (display "Actual: " machine-calc)
		'error))))

;;Tested interactively in the REPL for assorted values including zeroes
;;and negative b's. The implementation assumes positive n's.

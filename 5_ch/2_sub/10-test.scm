(load "10.scm")

(define IEM
  (make-machine
	'(b n val)
	(list (list '* *) (list '- -) (list '= =))
	'(  (assign val (const 1))
	  expt-loop
	    (test (op =) (reg n) (const 0))
		(branch (label expt-done))
		(dec n (const 1))
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

(test-machine IEM 2 4)
(test-machine IEM 0 0)
(test-machine IEM 1 100)
(test-machine IEM -2 3)

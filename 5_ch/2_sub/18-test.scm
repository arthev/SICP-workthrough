(load "18.scm")

(define FACT
  (make-machine
	'(val n continue)
	(list (list '* *) (list '- -) (list '= =) (list 'print print) (list 'read read))
	'(fact-func
	    (perform (op initialize-stack))
		(assign n (op read))
		(assign continue (label fact-done))
	  fact-loop
	    (test (op =) (reg n) (const 1))
		(branch (label base-case))
		(save continue)
		(save n)
		(dec n)
		(assign continue (label after-fact))
		(goto (label fact-loop))
	  after-fact
	    (restore n)
		(restore continue)
		(assign val (op *) (reg n) (reg val))
		(goto (reg continue))
	  base-case
	    (assign val (const 1))
		(goto (reg continue))
	  fact-done
	    (perform (op print) (reg val))
		(perform (op print-stack-statistics)))))
(start FACT)

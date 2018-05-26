(load "14.scm")

(define (print arg)
  (newline) (display "PRINT:") (display arg) (newline))

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
		(perform (op print-stack-statistics))
		(goto (label fact-func)))))



;;n = 1: total-pushes =  0, maximum-depth = 0
;;n = 2: total-pushes =  2, maximum-depth = 2
;;n = 3: total-pushes =  4, maximum-depth = 4
;;n = 4: total-pushes =  6, maximum-depth = 6
;;n = 5: total-pushes =  8, maximum-depth = 8
;;n = 6: total-pushes = 10, maximum-depth = 10

;;We see that for small n, we get total-pushes equal to maximum-depth.
;;And we also get total-pushes = 2n - 2.

;;We should then expect n = 21 to yield total-pushes & maximum-depth 40,
;;which it does.

;;Let's try n = 2100:
;;(total-pushes = 4198 maximum-depth = 4198)

;;Relation between n and total-pushes and maximum-depth seems clear in this case.

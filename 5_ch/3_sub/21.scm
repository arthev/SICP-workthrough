(load "regsim.scm")

(define IMPL
  (make-machine
	'(tree n temp continue)
	(list (list '+ +) (list 'print print) (list 'read read) (list 'null? null?)
		  (list 'not not) (list 'pair? pair?) (list 'car car) (list 'cdr cdr))
	'(  (assign continue (label cntlf-done))
	    (save continue)
		(save tree)
		(assign n (const 0))
		(assign temp (const 0))
	  cntlf-loop
	    (test (op null?) (reg tree))
		(branch (label null-case))
		(test (op pair?) (reg tree))
		(branch (label ctnlf-pair))
		(goto (label atom-case))
	  cntlf-pair
	    (assign continue (label cntlf-loop))
	    (assign temp (op cdr) (reg tree))
		(save continue)
		(save temp)
		(assign tree (op car) (reg tree))
		(goto (label cntlf-loop))
	  atom-case
	    (assign n (op +) (reg n) (const 1))
		(restore tree)
		(restore continue)
		(goto (reg continue))
	  null-case
	    (restore tree)
		(restore continue)
		(goto (reg continue))
	  cntlf-done)))
	  
(set-register-contents! IMPL 'tree '(a b (c d (e)) f g))
(start IMPL)
(get-register-contents IMPL 'n)



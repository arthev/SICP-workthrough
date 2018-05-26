(load "19.scm")

(define GCD
  (make-machine
	'(a b t)
	(list (list 'rem remainder) (list '= =)
		  (list 'print print) (list 'read read))
	'(gcd-start
	    (perform (op initialize-stack))
		(assign a (op read))
		(assign b (op read))
	  test-b
	    (test (op =) (reg b) (const 0))
	    (branch (label gcd-done))
	    (assign t (op rem) (reg a) (reg b))
	    (assign a (reg b))
	    (assign b (reg t))
	    (goto (label test-b))
	  gcd-done
	    (perform (op print) (reg a))
		(perform (op print-stack-statistics)))))
;;Tested interactively in REPL,
;;the breakpoint functionality seems to work.
